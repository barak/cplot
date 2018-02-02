{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent                as CC
import           Control.Exception.Safe
import           Control.Lens                      hiding (set)
import           Control.Monad                     (void)
import           Control.Monad.Reader
import           Data.Default                      (def)
import           Data.IORef
import           Data.Maybe                        (fromJust)

import qualified GI.Cairo                          as GI.Cairo
import           GI.Gtk                            hiding (main, parseArgs)
import qualified GI.Gtk                            as Gtk (init, main)
import qualified GI.Gdk                            as Gdk
import qualified GI.GLib                           as GLib

import           Foreign.Ptr                       (castPtr)
import           Graphics.Rendering.Cairo.Internal (Render (runRender))
import           Graphics.Rendering.Cairo.Types    (Cairo (Cairo))

import           App
import           Chart                             (Chart)
import qualified Chart
import           Options
import qualified Utils


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
  chartRefList <- view chartRefs
  let testChartRef = head chartRefList

  mainWindow <- new Window
    [ #title         := "cplot"
    , #defaultWidth  := 800
    , #defaultHeight := 600
    ]

  -- bind quit to main window
  on mainWindow #destroy mainQuit

  -- grid <- new Grid [ #orientation := OrientationHorizontal ]

  chartCanvas <- new DrawingArea []
  #setSizeRequest chartCanvas 400 300
  #add mainWindow chartCanvas

  on chartCanvas #draw $ \context -> do
    w <- fromIntegral <$> #getAllocatedWidth chartCanvas
    h <- fromIntegral <$> #getAllocatedHeight chartCanvas

    testChart <- readIORef testChartRef

    renderWithContext context (Chart.renderChart testChart (w, h))
    return True

  liftIO $ CC.forkIO $ forever $ do
    #queueDraw chartCanvas
    CC.threadDelay (1000000 `div` 30)

  #showAll mainWindow
