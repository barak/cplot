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
  , drainBuffersEvery

  -- AppEnv lenses
  , options
  , config
  , state

  -- AppState lenses
  , chartRefs
  ) where

import           Control.Lens
import qualified Data.HashMap.Lazy      as Map
import           Data.IORef
import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack, unpack)

import qualified Control.Concurrent     as CC
import           Control.Exception.Safe
import           Control.Monad          (forever, void)
import           Control.Monad.Reader

import           Pipes
import qualified Pipes.Group            as PG
import qualified Pipes.Text             as PT
import qualified Pipes.Text.IO          as PT

import           App.Types
import           Chart
import           Options
import           Parser.Generic         as Parser
import           Parser.Point


newAppEnv :: AppOptions -> AppConfig -> AppState -> AppEnv
newAppEnv = AppEnv

runApp :: App a -> AppEnv -> IO ()
runApp app = void . runReaderT (unApp app)

--------------------------------------------------------------------------------
-- EXCEPTIONS

data AppException
  = NoParse Text

instance Show AppException where
  show (NoParse e) = unpack e

instance Exception AppException


--------------------------------------------------------------------------------
-- PIPES

parsePoint :: MonadThrow m => Pipe Text Message m ()
parsePoint = forever $ do
  rawString <- await
  case parseMessage rawString of
    Left e  -> throw $ NoParse (pack (Parser.parseErrorPretty e))
    Right p -> yield p


fillBuffers :: (MonadIO m, MonadReader env m, HasAppState env)
            => Consumer Message m ()
fillBuffers = loop
  where
    loop = do
      msg    <- await
      refMap <- view chartRefs

      case Map.lookup (msg^.chartID) refMap of
        Nothing  -> loop
        Just ref -> do
          chart <- liftIO $ readIORef ref
          liftIO $ writeIORef ref
                 $ chart & subcharts . traverse %~ pushToBuffer (msg^.point)
          loop

drainBuffersEvery :: (MonadIO m, MonadReader env m, HasAppState env)
                  => Int -> m ()
drainBuffersEvery μs = loop
  where
    loop = do
      liftIO (CC.threadDelay μs)
      refMap <- view chartRefs
      forM_ (Map.elems refMap) $ \ref -> liftIO $ do
        chart <- readIORef ref
        writeIORef ref $ chart & subcharts . traverse %~ drainBufferToDataset
      loop

-- | Receives points and places them in their respective buffers.
inputStream :: App ()
inputStream =
  runEffect $ accumLines PT.stdin
          >-> parsePoint
          >-> fillBuffers

accumLines :: Monad m => Producer Text m r -> Producer Text m r
accumLines = mconcats . view PT.lines

mconcats :: (Monoid a, Monad m) => PT.FreeT (Producer a m) m r -> Producer a m r
mconcats = PG.folds (<>) mempty id
