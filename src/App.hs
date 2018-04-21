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


-- | Alias for 'AppEnv' so we don't expose the data type. Packages command line
--   options, configuration data, and mutable state into the 'AppEnv' type.
newAppEnv :: AppOptions -> AppConfig -> AppState -> AppEnv
newAppEnv = AppEnv

-- | Supplies an App with an initial environment and executes it.
runApp :: App a -> AppEnv -> IO ()
runApp app = void . runReaderT (unApp app)

--------------------------------------------------------------------------------
-- EXCEPTIONS

-- | Enum containing all custom exceptions. This does not (and cannot) document
--   all possible exceptions, so really we're just providing more ways the
--   program can fail. Wonderful!
data AppException
  = ParseError String
  | ChartNotFound String

-- | All exceptions need a Show instance so Haskell knows how to report them.
instance Show AppException where
  show = \case
    ParseError input ->
        "Couldn't parse input: \""
      <> input
      <> "\"\nSyntax is \"[chartID]: [p1] [p2]\""

    ChartNotFound identifier ->
      "Chart identifier not found: \"" <> identifier <> "\""

-- | Deriving trivial Exception instance so we can 'throw' AppException's.
instance Exception AppException


--------------------------------------------------------------------------------
-- PIPES

-- | Parse ByteString input to a 'Message' data type and push downstream.
parseMessage :: MonadThrow m
             => Pipe PB.ByteString Message m ()
parseMessage = forever $ do
  rawInput <- await
  case Parser.parseMessage rawInput of
    Left  _       -> throw $ ParseError (T.unpack (decodeUtf8 rawInput))
    Right message -> yield message


-- | Interpret message and send information to each relevant chart. The
--   pattern:
--   f x y z = loop
--     where
--       loop = do
--         ...
--  is quite a nice way (in a monadic context) to have explicit control over
--  precisely how and when a function loops.
fillBuffers :: (MonadIO m, MonadThrow m, HasState r m)
            => Consumer Message m ()
fillBuffers = loop
  where
    loop = do
      msg   <- await
      refs  <- view chartRefs

      let identifier = decodeUtf8 (msg^.chartID)

      case Map.lookup identifier refs of
        Nothing ->
          throw $ ChartNotFound (T.unpack identifier)

        Just (ref, flag) -> do
          liftIO $ modifyMVar_ ref $ \chart -> return $
            chart & subcharts.traverse %~ pushToBuffer (msg^.point)

          trySignal flag
          loop


-- | Equivalent of a non-blocking signal for a typical binary semaphore.
trySignal :: MonadIO m => MVar () -> m ()
trySignal flag = void . liftIO $ tryPutMVar flag ()


-- | Periodically flush chart 'Buffer's into their respective 'Dataset's. Really
--   the code for the HashMaps shouldn't be in here but that's a simple
--   refactor.
flushBuffers :: (MonadIO m, HasState r m, HasConfig r m)
             => m ()
flushBuffers = loop
  where
    loop = do
      μs <- view flushRate
      liftIO (CC.threadDelay μs)  -- wait for 'μs' microseconds

      refs <- view chartRefs
      let chartIDs = Map.keys refs

      forM_ chartIDs $ \chartIdentifier -> do
        let (ref, _) = refs ! chartIdentifier
        liftIO $ modifyMVar_ ref $ \chart -> return $
          chart & subcharts.traverse %~ flushBufferToDataset

      loop


-- | Assemble input stream.
inputStream :: App ()
inputStream =
  runEffect $ accumLines PB.stdin  -- producer/source
          >-> parseMessage         -- pipe
          >-> fillBuffers          -- consumer/sink


-- | Chunk a ByteString producer by lines. Hope you like free monads!
accumLines :: Monad m => Producer PB.ByteString m r -> Producer PB.ByteString m r
accumLines = mconcats . view PB.lines

-- | Smooshes a bunch of Producers into a single Producer, whenever the type
--   produced is a monoid. This is required since the stdin pipe does not
--   automatically chunk by lines. We need to first 'view' it by lines, which
--   transforms it into a 'list of lines' (FreeT), then concatenate them back
--   into a single producer again, to create a producer of lines.
mconcats :: (Monoid a, Monad m) => PB.FreeT (Producer a m) m r -> Producer a m r
mconcats = PG.folds (<>) mempty id
