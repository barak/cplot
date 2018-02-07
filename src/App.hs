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

  -- AppEnv lenses
  , options
  , config
  , state

  -- AppState lenses
  , chartRefs
  ) where

import           Control.Lens
import           Data.IORef
import           Data.Monoid            ((<>))
import           Data.Text              (Text, pack, unpack)

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

updateRefs :: (MonadIO m, MonadReader env m, HasAppState env)
           => Consumer Message m ()
updateRefs = forever $ do
  msg  <- await
  refs <- view chartRefs

  liftIO $ forM_ refs $ \chartRef -> do
    chart <- readIORef chartRef
    let newPoint = msg ^. msgPoint

    when (chart ^. title == msg ^. chartID) $
      writeIORef chartRef $
        chart & subcharts . traverse %~ updateSubchart newPoint

updateSubchart :: (Double, Double) -> Subchart -> Subchart
updateSubchart newPoint subchart =
  subchart & numDataPoints +~ 1
           & dataset       %~ newData
  where
    nPoints   = subchart ^. numDataPoints
    maxPoints = subchart ^. maxDataPoints
    newData d =
      if nPoints >= maxPoints
        then snd (pushPopPoint newPoint d)
        else pushPoint newPoint d

inputStream :: App ()
inputStream =
  runEffect $ accumLines PT.stdin
          >-> parsePoint
          >-> updateRefs

accumLines :: Monad m => Producer Text m r -> Producer Text m r
accumLines = mconcats . view PT.lines

mconcats :: (Monoid a, Monad m) => PT.FreeT (Producer a m) m r -> Producer a m r
mconcats = PG.folds (<>) mempty id
