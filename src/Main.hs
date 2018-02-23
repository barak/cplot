{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent                as CC
import           Control.Exception.Safe
import           Control.Lens                      hiding (set)
import           Control.Monad.Reader
import           Data.Default                      (def)
import           Data.IORef

import qualified GI.Cairo
import           GI.Gtk                            hiding (main, parseArgs)
import qualified GI.Gtk                            as Gtk (init, main)
import qualified GI.Gdk                            as Gdk
import qualified GI.GLib                           as GLib

import           Foreign.Ptr                       (castPtr)
import           Graphics.Rendering.Cairo.Internal (Render (runRender))
import           Graphics.Rendering.Cairo.Types    (Cairo (Cairo))

import           App
import qualified Chart
import           Options


main :: IO ()
main = do
  opts <- liftIO parseArgs
  env <- createEnvironment opts
  CC.forkIO $ runApp inputStream env `onException` killGUI
  runGtkApp appGtk env

  where
    killGUI =
      Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT $ do
        mainQuit
        return True

runGtkApp :: App a -> AppEnv -> IO ()
runGtkApp app env = do
  Gtk.init Nothing
  runApp app env
  Gtk.main

createEnvironment :: AppOptions -> IO AppEnv
createEnvironment opts = do
  chartRefList <- mapM newIORef (opts ^. initialCharts)
  return $ newAppEnv opts def (def & chartRefs .~ chartRefList)

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ctx r =
  withManagedPtr ctx $ \p ->
    runReaderT (runRender r) (Cairo (castPtr p))

appGtk :: App ()
appGtk = do
  mainWindow <- new Window
    [ #title         := "cplot"
    , #defaultWidth  := 800
    , #defaultHeight := 600
    ]

  -- bind quit to main window
  on mainWindow #destroy mainQuit

  grid <- new Grid [ #orientation := OrientationVertical ]
  #setRowHomogeneous grid True
  #setColumnHomogeneous grid True
  #add mainWindow grid

  canvases <- generateChartCanvases
  mapM_ (#add grid) canvases

  liftIO $ CC.forkIO $ forever $ do
    mapM_ #queueDraw canvases
    CC.threadDelay (1000000 `div` 30)

  #showAll mainWindow

-- | Creates the canvases and links the draw event to the chart renderer.
generateChartCanvases :: (MonadIO m, MonadReader env m, HasAppState env) => m [DrawingArea]
generateChartCanvases = do
  refs <- view chartRefs
  forM refs $ \ref -> do
    canvas <- new DrawingArea []

    on canvas #draw $ \ctx -> do
      w <- fromIntegral <$> #getAllocatedWidth canvas
      h <- fromIntegral <$> #getAllocatedHeight canvas

      chart <- readIORef ref

      renderWithContext ctx (Chart.renderChart chart (w, h))
      return True

    return canvas
