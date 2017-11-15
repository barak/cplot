{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import qualified Control.Concurrent     as CC
import           Control.Exception.Safe
import           Control.Lens
import           Control.Monad          (void)
import           Control.Monad.Reader
import           Data.Default           (def)
import           Data.IORef             (IORef)
import qualified Data.IORef             as IORef

import           Graphics.UI.Gtk        (AttrOp ((:=)))
import qualified Graphics.UI.Gtk        as Gtk

import           App
import           Chart                  (Chart)
import qualified Chart
import           Options
import qualified Utils


main :: IO ()
main = do
  opts <- liftIO parseArgs
  env <- createEnvironment opts
  CC.forkIO $ runApp inputStream env `onException` Gtk.postGUIAsync Gtk.mainQuit
  runGtkApp appGtk env

runGtkApp :: App a -> AppEnv -> IO ()
runGtkApp app env = Gtk.initGUI >> runApp app env >> Gtk.mainGUI

createEnvironment :: AppOptions -> IO AppEnv
createEnvironment opts = do
  chartRefList <- mapM IORef.newIORef (opts ^. initialCharts)
  return $ newAppEnv opts def (def & chartRefs .~ chartRefList)

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
      CC.threadDelay (1000000 `div` 30)

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
