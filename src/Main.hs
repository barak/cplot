{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import           Control.Lens
import           Control.Monad            (void)
import           Control.Monad.Reader

import qualified Control.Concurrent       as CC
import qualified Data.DList               as DList
import           Data.IORef               (IORef)
import qualified Data.IORef               as IORef
import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk          (AttrOp ((:=)))
import qualified Graphics.UI.Gtk          as Gtk

import           Pipes
import qualified Pipes.ByteString as P

import           App.Types
import qualified Chart
import           Chart.Types              (Chart (..), ChartData (..),
                                           ChartType (..), chartData)
import           Options                  (AppOptions)
import qualified Options
import qualified Parser
import qualified Utils

main :: IO ()
main = do
  opts <- liftIO Options.parseArgs
  env <- createEnvironment opts
  CC.forkIO $ runApp appPipe env
  runGtkApp appGtk env

runApp :: App a -> AppEnv -> IO ()
runApp app = void . runReaderT (unApp app)

runGtkApp :: App a -> AppEnv -> IO ()
runGtkApp app env = Gtk.initGUI >> runApp app env >> Gtk.mainGUI

-- this should be taking advantage of the default class
createEnvironment :: AppOptions -> IO AppEnv
createEnvironment opts = do
  chartList <- forM (opts ^. Options.chartTypes) $ \ct -> do
    redrawRef <- IORef.newIORef False
    datasetRef <- case ct of
      Line       -> IORef.newIORef [LineData DList.empty]
      Scatter    -> IORef.newIORef [ScatterData DList.empty]
      TimeSeries -> error "not yet implemented"
    return $ Chart "sample chart" datasetRef redrawRef

  return $ AppEnv opts AppConfig (AppState chartList) putStrLn

appPipe :: App ()
appPipe = runEffect $ P.stdin >-> parsePoint >-> updateRefs

-- really wish gtk3 functions used MonadIO
appGtk :: App ()
appGtk = do
  chartList <- view $ appState . charts

  liftIO $ do
    mainWindow <- Gtk.windowNew
    Gtk.set mainWindow
      [ Gtk.windowTitle          := ("cplot" :: String)
      , Gtk.windowDefaultWidth   := 800
      , Gtk.windowDefaultHeight  := 600
      , Gtk.containerBorderWidth := 10
      ]
    bindQuit mainWindow

    grid <- Gtk.gridNew
    Gtk.gridSetRowHomogeneous grid True
    Gtk.gridSetColumnHomogeneous grid True

    let packing = Utils.packToSquare (length chartList)
    canvases <- forM (zip packing chartList) $ \((x,y,s), chart) -> do
      chartCanvas <- Gtk.drawingAreaNew

      -- This allows you to increase the size of the window, but not decrease
      -- it, which is utterly bizarre.
      chartCanvas `Gtk.on` Gtk.configureEvent $ do
        Gtk.Rectangle _ _ w h <- liftIO $ Gtk.widgetGetAllocation chartCanvas
        liftIO $ Gtk.widgetSetSizeRequest chartCanvas w h
        return True

      Gtk.gridAttach grid chartCanvas y x s 1

      chartCanvas `Gtk.on` Gtk.draw $
        updateCanvas chart chartCanvas

      return chartCanvas

    -- this is a very weird chart update model. when surfaces are introduced,
    -- each chart updates only when needed, and only at a specific frequency.
    CC.forkIO $ forever $ forM_ canvases $ \canvas -> do
      requestRedrawCanvas canvas
      CC.threadDelay (1000000 `div` (30 * 4))

    Gtk.set mainWindow [ Gtk.containerChild := grid ]

    Gtk.widgetShowAll mainWindow

-- | Convenience function for binding mainQuit to the deleteEvent of a widget.
bindQuit :: Gtk.WidgetClass a => a -> IO ()
bindQuit window =
  void $ window `Gtk.on` Gtk.deleteEvent
       $ liftIO Gtk.mainQuit >> return False

-- probably not worth refactoring to use the app env
updateCanvas :: MonadIO m => Chart -> Gtk.DrawingArea -> m ()
updateCanvas chart canvas =
  liftIO $ do
    datasets <- IORef.readIORef (chart ^. chartData)

    let ds = flip map datasets $ \dataset ->
          case dataset of
            LineData       d -> d
            ScatterData    d -> d
            TimeSeriesData _ -> error "not implemented"

    Gtk.Requisition w h <- Gtk.widgetSizeRequest canvas
    let renderable = Chart.renderChart (map DList.toList ds)
                                       (fromIntegral w, fromIntegral h)

    Just win <- Gtk.widgetGetWindow canvas
    Gtk.renderWithDrawWindow win renderable

requestRedrawCanvas :: Gtk.DrawingArea -> IO ()
requestRedrawCanvas canvas = Gtk.postGUIAsync $ Gtk.widgetQueueDraw canvas

--------------------------------------------------------------------------------
-- PIPES

parsePoint :: (MonadIO m, MonadReader env m, HasAppLogger env (String -> IO ()))
           => Pipe P.ByteString (Double, Double) m ()
parsePoint = forever $ do
  raw <- await
  logger <- view appLogger
  case Parser.point raw of
    Left e  -> liftIO $ logger $ Parser.parseErrorPretty e
    Right p -> yield p

updateRefs :: (MonadIO m, MonadReader env m, HasAppState env AppState)
           => Consumer (Double, Double) m ()
updateRefs = forever $ do
  point <- await
  allCharts <- view $ appState . charts
  liftIO $ forM_ allCharts $ \chart -> do
    let ref = chart ^. chartData
    [dataset] <- IORef.readIORef ref
    case dataset of
      LineData d       -> IORef.writeIORef ref [LineData $ DList.snoc d point]
      ScatterData d    -> IORef.writeIORef ref [LineData $ DList.snoc d point]
      TimeSeriesData _ -> error "not yet implemented"
