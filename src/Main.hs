{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Control.Concurrent       as CC
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad            (void)
import           Control.Monad.Reader
import           Data.Default             (def)
import           Data.IORef               (IORef)
import qualified Data.IORef               as IORef
import           Data.Text                (Text)
import           Data.Void                (Void)

import qualified Graphics.Rendering.Cairo as Cairo
import           Graphics.UI.Gtk          (AttrOp ((:=)))
import qualified Graphics.UI.Gtk          as Gtk

import           Conduit

import           App
import           Chart                    (Chart)
import qualified Chart
import           Options                  (AppOptions, chartTypes)
import qualified Options
import qualified Parser
import qualified Utils


main :: IO ()
main = do
  opts <- liftIO Options.parseArgs
  env <- createEnvironment opts
  CC.forkIO $ runApp inputStream env `onException` Gtk.postGUIAsync Gtk.mainQuit
  runGtkApp appGtk env

runGtkApp :: App a -> AppEnv -> IO ()
runGtkApp app env = Gtk.initGUI >> runApp app env >> Gtk.mainGUI

createEnvironment :: AppOptions -> IO AppEnv
createEnvironment opts = do
  chartRefList <- forM (opts ^. chartTypes) $ \subchartTypes -> do
    let
      -- TODO: ↓↓ this doesn't utilise the chart types
      subcharts =
        [ Chart.label   .~ "label"
        $ Chart.dataset .~ def
        $ def
        | _ <- subchartTypes
        ]

      chart
        = Chart.title     .~ "chart title"
        $ Chart.subcharts .~ subcharts
        $ def

    IORef.newIORef chart

  return $ newAppEnv opts def (def & chartRefs .~ chartRefList)

inputStream :: App ()
inputStream = runConduit conduit
  where
    conduit = stdinC
          =$= decodeUtf8C
          =$= linesUnboundedC
          =$= parsePoint
          =$= updateRefs

appGtk :: App ()
appGtk = do
  chartRefList <- view chartRefs

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

    let packing = Utils.packToSquare (length chartRefList)
    canvases <- forM (zip packing chartRefList) $ \((x,y,s), chart) -> do
      chartCanvas <- Gtk.drawingAreaNew

      -- FIXME: This allows you to increase the size of the window, but not
      -- decrease it, which is utterly bizarre.
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

updateCanvas :: MonadIO m => IORef Chart -> Gtk.DrawingArea -> m ()
updateCanvas chartRef canvas = liftIO $ do
  chart <- IORef.readIORef chartRef
  Gtk.Requisition w h <- Gtk.widgetSizeRequest canvas
  let renderable = Chart.renderChart chart (fromIntegral w, fromIntegral h)

  Just win <- Gtk.widgetGetWindow canvas
  Gtk.renderWithDrawWindow win renderable

requestRedrawCanvas :: Gtk.DrawingArea -> IO ()
requestRedrawCanvas canvas = Gtk.postGUIAsync $ Gtk.widgetQueueDraw canvas

--------------------------------------------------------------------------------
-- PIPES

data AppException
  = NoParse String

instance Show AppException where
  show (NoParse e) = e

instance Exception AppException

parsePoint :: MonadThrow m => ConduitM Text (Double, Double) m ()
parsePoint = do
  mrawString <- await
  forM_ mrawString $ \rawString ->
    case Parser.point rawString of
      Left e  -> throw $ NoParse (Parser.parseErrorPretty e)
      Right p -> yield p >> parsePoint

-- | for now, just add the point to all subcharts
updateRefs :: (MonadIO m, MonadReader env m, HasAppState env)
           => ConduitM (Double, Double) Void m ()
updateRefs = do
  mpoint <- await
  refs <- view chartRefs
  forM_ mpoint $ \point -> do
    liftIO $ forM_ refs $ \chartRef ->
      IORef.modifyIORef chartRef
        (Chart.subcharts . traverse . Chart.dataset %~ Chart.addPoint point)
    updateRefs
