{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedLabels  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Control.Concurrent                as CC
import           Control.Exception.Safe
import           Control.Lens                      hiding (set)
import           Control.Monad.Reader
import           Data.Default                      (def)
import qualified Data.HashMap.Lazy                 as Map
import           Data.IORef

import qualified Data.ByteString                   as B
import qualified Data.Yaml                         as Y

import qualified GI.Cairo
import           GI.Gtk                            hiding (main, parseArgs)
import qualified GI.Gtk                            as Gtk (init, main)
import qualified GI.Gdk                            as Gdk
import qualified GI.GLib                           as GLib

import           Foreign.Ptr                       (castPtr)
import           Graphics.Rendering.Cairo.Internal (Render (runRender))
import           Graphics.Rendering.Cairo.Types    (Cairo (Cairo))

import           System.Directory                  (doesFileExist)

import           App
import qualified Chart
import           Options


throwEither :: (MonadThrow m, Exception e) => Either e a -> m a
throwEither = \case
  Left e  -> throw e
  Right a -> return a

main :: IO ()
main = do
  let confPath = "cplot.yaml"
  opts <- liftIO parseArgs
  conf <- (throwEither =<<) $ do
    confPresent <- doesFileExist confPath
    if confPresent
      then Y.decodeFileEither confPath
      else do
        putStrLn $ "No cplot.yaml detected, creating default config.\n\n"
                ++ "It is highly recommended that you customise this file as "
                ++ "per your requirements."
        B.writeFile confPath defaultAppConfig
        Y.decodeFileEither confPath

  env <- createEnvironment opts conf

  CC.forkIO $ runApp inputStream env `onException` killGUI
  CC.forkIO $ runApp drainBuffers env `onException` killGUI

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

createEnvironment :: AppOptions -> AppConfig -> IO AppEnv
createEnvironment opts conf = do
  chartRefList <- mapM newIORef (opts ^. initialCharts)
  return $ newAppEnv opts conf (def & chartRefs .~ chartRefList)

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ctx r =
  withManagedPtr ctx $ \p ->
    runReaderT (runRender r) (Cairo (castPtr p))

appGtk :: App ()
appGtk = do
  w <- view windowWidth
  h <- view windowHeight
  mainWindow <- new Window
    [ #title         := "cplot"
    , #defaultWidth  := w
    , #defaultHeight := h
    ]

  on mainWindow #destroy mainQuit

  grid <- new Grid [ #orientation := OrientationVertical ]
  #setRowHomogeneous grid True
  #setColumnHomogeneous grid True
  #add mainWindow grid

  canvases <- generateChartCanvases
  mapM_ (#add grid) canvases

  framerate <- view fps
  void $ GLib.timeoutAdd GLib.PRIORITY_HIGH (1000 `div` framerate) $ do
    mapM_ #queueDraw canvases
    return True

  #showAll mainWindow

-- | Creates the canvases and links the draw event to the chart renderer.
generateChartCanvases :: (MonadIO m, MonadReader env m, HasAppState env) => m [DrawingArea]
generateChartCanvases = do
  refs <- view chartRefs
  forM (Map.elems refs) $ \ref -> do
    canvas <- new DrawingArea []

    on canvas #draw $ \ctx -> do
      w <- fromIntegral <$> #getAllocatedWidth canvas
      h <- fromIntegral <$> #getAllocatedHeight canvas

      chart <- readIORef ref

      renderWithContext ctx (Chart.renderChart chart (w, h))
      return True

    return canvas
