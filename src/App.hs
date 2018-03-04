module App
  ( App
  , AppEnv
  , AppConfig
  , AppState
  , HasAppOptions
  , HasAppConfig
  , HasAppState

  , runApp
  , newAppEnv
  , inputStream
  , drainBuffers

  , defaultAppConfig

  -- AppEnv lenses
  , options
  , config
  , state

  -- AppConfig lenses
  , fps
  , drainRate
  , windowWidth
  , windowHeight

  -- AppState lenses
  , chartRefs
  ) where

import           Control.Lens
import qualified Data.HashMap.Lazy      as Map
import           Data.IORef
import           Data.Monoid            ((<>))
import           Data.Text.Encoding     (decodeUtf8)

import qualified Control.Concurrent     as CC
import           Control.Exception.Safe
import           Control.Monad          (forever, void)
import           Control.Monad.Reader

import           Pipes
import qualified Pipes.ByteString       as PB
import qualified Pipes.Group            as PG

import           App.Types
import           Chart
import           Options
import           Parser.Point


newAppEnv :: AppOptions -> AppConfig -> AppState -> AppEnv
newAppEnv = AppEnv

runApp :: App a -> AppEnv -> IO ()
runApp app = void . runReaderT (unApp app)

--------------------------------------------------------------------------------
-- EXCEPTIONS

data AppException
  = NoParse String

instance Show AppException where
  show (NoParse e) = e

instance Exception AppException


--------------------------------------------------------------------------------
-- PIPES

parsePoint :: MonadThrow m => Pipe PB.ByteString Message m ()
parsePoint = forever $ do
  rawString <- await
  case parseMessage rawString of
    Left e  -> throw (NoParse e)
    Right p -> yield p


fillBuffers :: (MonadIO m, MonadReader env m, HasAppState env)
            => Consumer Message m ()
fillBuffers = loop
  where
    loop = do
      msg    <- await
      refMap <- view chartRefs

      case Map.lookup (decodeUtf8 $ msg^.chartID) refMap of
        Nothing  -> loop
        Just ref -> do
          chart <- liftIO $ readIORef ref
          liftIO $ writeIORef ref
                 $ chart & subcharts . traverse %~ pushToBuffer (msg^.point)
          loop

drainBuffers :: (MonadIO m, MonadReader env m, HasAppState env, HasAppConfig env)
             => m ()
drainBuffers = loop
  where
    loop = do
      μs <- view drainRate
      liftIO (CC.threadDelay μs)

      refMap <- view chartRefs
      forM_ (Map.elems refMap) $ \ref -> liftIO $ do
        chart <- readIORef ref
        writeIORef ref $ chart & subcharts . traverse %~ drainBufferToDataset
      loop

-- | Receives points and places them in their respective buffers.
inputStream :: App ()
inputStream =
  runEffect $ accumLines PB.stdin
          >-> parsePoint
          >-> fillBuffers

accumLines :: Monad m => Producer PB.ByteString m r -> Producer PB.ByteString m r
accumLines = mconcats . view PB.lines

mconcats :: (Monoid a, Monad m) => PB.FreeT (Producer a m) m r -> Producer a m r
mconcats = PG.folds (<>) mempty id
