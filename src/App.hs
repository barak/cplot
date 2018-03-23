module App
  ( App
  , AppEnv
  , AppConfig
  , AppState
  , HasOptions
  , HasConfig
  , HasState

  , runApp
  , newAppEnv
  , inputStream
  , flushBuffers

  , defaultAppConfig

  -- AppEnv lenses
  , options
  , config
  , state

  -- AppConfig lenses
  , fps
  , flushRate
  , windowWidth
  , windowHeight

  , lineConfig
  , cycleAfter

  -- AppState lenses
  , chartRefs
  ) where

import           Control.Concurrent.MVar
import           Control.Lens
import           Data.HashMap.Strict     ((!))
import qualified Data.HashMap.Strict     as Map
import           Data.Monoid             ((<>))
import qualified Data.Text               as T
import           Data.Text.Encoding      (decodeUtf8)

import qualified Control.Concurrent      as CC
import           Control.Exception.Safe
import           Control.Monad           (forever, void)
import           Control.Monad.Reader

import           Pipes
import qualified Pipes.ByteString        as PB
import qualified Pipes.Group             as PG

import           App.Types
import           Chart
import           Options
import           Parser.Message          (Message, chartID, point)
import qualified Parser.Message          as Parser


-- | Alias for 'AppEnv' so we don't expose the data type.
newAppEnv :: AppOptions -> AppConfig -> AppState -> AppEnv
newAppEnv = AppEnv

-- | Supplies an application with an initial environment.
runApp :: App a -> AppEnv -> IO ()
runApp app = void . runReaderT (unApp app)

--------------------------------------------------------------------------------
-- EXCEPTIONS

data AppException
  = ParseError String
  | ChartNotFound String

instance Show AppException where
  show = \case
    ParseError input ->
        "Couldn't parse input: \""
      <> input
      <> "\"\nSyntax is \"[chartID]: [p1] [p2]\""

    ChartNotFound identifier ->
      "Chart identifier not found: \"" <> identifier <> "\""

instance Exception AppException


--------------------------------------------------------------------------------
-- PIPES

-- | Parse input to 'Message' and send downstream.
parseMessage :: MonadThrow m
             => Pipe PB.ByteString Message m ()
parseMessage = forever $ do
  rawInput <- await
  case Parser.parseMessage rawInput of
    Left  _       -> throw $ ParseError (T.unpack (decodeUtf8 rawInput))
    Right message -> yield message


-- | Based on 'Message' content, fill relevant chart buffers.
fillBuffers :: (MonadIO m, MonadThrow m, HasState r m)
            => Consumer Message m ()
fillBuffers = loop
  where
    loop = do
      msg   <- await
      refs  <- view chartRefs

      let identifier = decodeUtf8 (msg^.chartID)

      case Map.lookup identifier refs of
        Nothing  -> throw $ ChartNotFound (T.unpack identifier)
        Just (ref, flag) -> do
          liftIO $ modifyMVar_ ref $ \chart -> return $
            chart & subcharts.traverse %~ pushToBuffer (msg^.point)

          trySignal flag
          loop


-- | Equivalent of a non-blocking signal for a typical binary semaphore.
trySignal :: MonadIO m => MVar () -> m ()
trySignal flag = void . liftIO $ tryPutMVar flag ()

-- | Periodically (depending on the flush rate) flush chart 'Buffer's into their
--   respective 'Dataset's.
flushBuffers :: (MonadIO m, HasState r m, HasConfig r m)
             => m ()
flushBuffers = loop
  where
    loop = do
      μs <- view flushRate
      liftIO (CC.threadDelay μs)

      refs <- view chartRefs
      let chartIDs = Map.keys refs

      forM_ chartIDs $ \cid -> do
        let (ref, _) = refs ! cid
        liftIO $ modifyMVar_ ref $ \chart -> return $
          chart & subcharts.traverse %~ flushBufferToDataset

      loop

-- | Assemble input stream.
inputStream :: App ()
inputStream =
  runEffect $ accumLines PB.stdin
          >-> parseMessage
          >-> fillBuffers

-- | Chunk a ByteString producer by lines.
accumLines :: Monad m => Producer PB.ByteString m r -> Producer PB.ByteString m r
accumLines = mconcats . view PB.lines

mconcats :: (Monoid a, Monad m) => PB.FreeT (Producer a m) m r -> Producer a m r
mconcats = PG.folds (<>) mempty id
