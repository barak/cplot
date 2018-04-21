{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE ViewPatterns     #-}

module Main where

import qualified Control.Concurrent                as CC
import           Control.Exception.Safe
import           Control.Lens                      hiding (set)
import           Control.Monad.Reader
import           Data.Default                      (def)
import           Data.FileEmbed
import           Data.HashMap.Strict               (HashMap, (!))
import qualified Data.HashMap.Strict               as Map
import           Data.Maybe                        (fromJust)

import qualified Data.ByteString                   as B
import           Data.Text                         (Text)
import qualified Data.Text.Encoding                as T
import qualified Data.Yaml                         as Y

import qualified GI.Cairo
import qualified GI.Gdk                            as Gdk
import qualified GI.GLib                           as GLib
import           GI.Gtk                            hiding (main, parseArgs)
import qualified GI.Gtk                            as Gtk

import           Foreign.Ptr                       (castPtr)
import           Graphics.Rendering.Cairo.Internal (Render (runRender))
import           Graphics.Rendering.Cairo.Types    (Cairo (Cairo))

import           System.Directory                  (doesFileExist)
import           System.IO

import           App
import           Chart                             (Chart, Subchart)
import qualified Chart
import           Options
import           Parser.GUI                        as Parser


main :: IO ()
main = do
  opts <- parseArgs

  -- Check for a configuration file. If it doesn't exist, create a new one.
  let confPath = "cplot.yaml"
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

  env <- createEnvironment conf opts

  -- set the input stream and flushing mechanism going on two separate threads.
  CC.forkIO $ runApp inputStream env `onException` killGUI
  CC.forkIO $ runApp flushBuffers env `onException` killGUI

  runGtkApp appGtk env

  where
    -- Generalizes Either to MonadThrow.
    throwEither :: (MonadThrow m, Exception e) => Either e a -> m a
    throwEither = \case
      Left e  -> throw e
      Right a -> return a

    -- Communicate from a separate thread that the GUI should be killed.
    killGUI = Gdk.threadsAddIdle GLib.PRIORITY_DEFAULT (mainQuit >> return True)

-- | Pass control of the main thread to GTK.
runGtkApp :: App a -> AppEnv -> IO ()
runGtkApp app env = do
  Gtk.init Nothing
  runApp app env
  Gtk.main


-- | Creates a new AppEnv from configuration data and command line options.
createEnvironment :: AppConfig -> AppOptions -> IO AppEnv
createEnvironment conf (withConf conf -> opts) = do
  charts <- mapM CC.newMVar (opts^.initialCharts)
  flags  <- mapM (const CC.newEmptyMVar) (opts^.initialCharts)

  let chartsWithFlags = Map.intersectionWith (,) charts flags

  return $ newAppEnv opts conf (def & chartRefs .~ chartsWithFlags)


-- | Convenience function for updating subchart configuration
withConf :: AppConfig -> AppOptions -> AppOptions
withConf conf opts =
  opts & initialCharts.traverse
       . Chart.subcharts.traverse %~ Chart.setConfig conf


-- | Convenience function for rendering cairo surfaces to a GTK+ DrawingArea.
renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext ctx r =
  withManagedPtr ctx $ \p ->
    runReaderT (runRender r) (Cairo (castPtr p))


-- | Bake UI XML file into source with TemplateHaskell. By far the easiest way
--   to make sure that external resources can be packaged into the binary
--   properly.
ui :: B.ByteString
ui = $(embedFile "resources/gui.glade")


-- | Main GTK application.
appGtk :: App ()
appGtk = do

  -- Retrieve GUI template from Glade.
  builder <- builderNewFromString (T.decodeUtf8 ui) (fromIntegral $ B.length ui)

  ------------------------------------------------------------------------------
  -- MAIN WINDOW

  mainWindow <- liftIO (builderGetObject builder "main-window"
            >>= unsafeCastTo Window . fromJust)

  width  <- view windowWidth
  height <- view windowHeight

  mainWindow `set`
    [ #title         := "cplot"
    , #defaultWidth  := width
    , #defaultHeight := height
    ]

  on mainWindow #destroy mainQuit


  ------------------------------------------------------------------------------
  -- CHART CANVASES

  chartGrid <- liftIO (builderGetObject builder "chart-grid"
           >>= unsafeCastTo Grid . fromJust)

  canvases <- generateChartCanvases
  bindDraw canvases

  mapM_ (#add chartGrid) canvases

  ------------------------------------------------------------------------------
  -- AXIS BUTTONS

  radioLinear <- liftIO (builderGetObject builder "linear-log-radio1"
             >>= unsafeCastTo RadioButton . fromJust)

  charts <- view chartRefs

  on radioLinear #toggled $ do
    toggleButtonGetActive radioLinear >>= \case
      True  -> mapM_ (setLinearAxis . fst) charts
      False -> mapM_ (setLogAxis . fst) charts
    mapM_ #queueDraw canvases

  ------------------------------------------------------------------------------
  -- BUFFER BUTTONS

  radioBuffer <- liftIO (builderGetObject builder "buffer-radio1"
             >>= unsafeCastTo RadioButton . fromJust)

  on radioBuffer #toggled $ do
    toggleButtonGetActive radioBuffer >>= \case
      True  -> mapM_ (setMMQ . fst) charts
      False -> mapM_ (setDefaultBuffer . fst) charts
    mapM_ #queueDraw canvases

  ------------------------------------------------------------------------------
  -- TEXT BOX (for setting the max number of data points)
  maxPointsEntry <- liftIO (builderGetObject builder "max-points-entry"
                >>= unsafeCastTo Entry . fromJust)

  view chartRefs >>= \refs -> on maxPointsEntry #activate $ do
    input <- entryGetText maxPointsEntry
    
    case Parser.parseNumericEntry input of
      Left e ->
        hPutStr stderr (Parser.parseErrorPretty e)

      Right n -> forM_ refs $ \(ref,_) ->
        updateAllSubcharts ref (Chart.setMaxDataPoints n)

  ------------------------------------------------------------------------------
  -- REDRAW CHARTS

  -- GLib SourceFunc needs IO explicitly, not MonadIO, so we need to pass part
  -- of the environment as arguments (unfortunately)
  framerate <- view fps
  refs      <- view chartRefs

  GLib.timeoutAdd GLib.PRIORITY_HIGH (1000 `div` framerate) $ do
    maybeRedrawCharts refs canvases
    return True

  -- show everything
  #showAll mainWindow


type ChartRef = CC.MVar Chart
type DrawFlag = CC.MVar ()

-- | Convenience function for updating a chart reference with a pure function.
updateChart :: ChartRef -> (Chart -> Chart) -> IO ()
updateChart ref f = CC.modifyMVar_ ref (pure . f)

-- | Convenience function for updating all subcharts within a chart reference
--   with a pure function.
updateAllSubcharts :: ChartRef -> (Subchart -> Subchart) -> IO ()
updateAllSubcharts ref f = updateChart ref (Chart.subcharts.traverse %~ f)

-- | Updates a charts axis scaling.
setLinearAxis, setLogAxis :: ChartRef -> IO ()
setLinearAxis ref = updateChart ref Chart.setLinearAxis
setLogAxis    ref = updateChart ref Chart.setLogAxis

-- | Swaps out buffers.
setMMQ, setDefaultBuffer :: ChartRef -> IO ()
setMMQ           ref = updateAllSubcharts ref Chart.setMMQ
setDefaultBuffer ref = updateAllSubcharts ref Chart.setDefaultBuffer

-- | Redraw any charts with raised DrawFlag's.
maybeRedrawCharts :: HashMap Text (ChartRef, DrawFlag)
                  -> HashMap Text DrawingArea
                  -> IO ()
maybeRedrawCharts refs canvases = do
  let chartIDs = Map.keys refs

  forM_ chartIDs $ \cid -> do
    let
      canvas    = canvases ! cid
      (_, flag) = refs     ! cid

    needsRedraw <- liftIO (CC.tryTakeMVar flag)
    case needsRedraw of
      Nothing -> return ()
      Just () -> #queueDraw canvas


-- | Define how each canvas should be drawn.
bindDraw :: (MonadIO m, HasState r m)
         => HashMap Text DrawingArea
         -> m ()
bindDraw canvases = do
  refs <- view chartRefs
  let chartIDs = Map.keys refs

  forM_ chartIDs $ \cid -> do
    let canvas = canvases ! cid
    on canvas #draw $ \ctx -> do
      w <- fromIntegral <$> #getAllocatedWidth canvas
      h <- fromIntegral <$> #getAllocatedHeight canvas

      chart <- CC.readMVar (fst $ refs ! cid)

      renderWithContext ctx (Chart.renderChart chart (w, h))
      return True


-- | Creates a mapping of chart identifiers to DrawingAreas.
generateChartCanvases :: (MonadIO m, HasState r m)
                      => m (HashMap Text DrawingArea)
generateChartCanvases = do
  refs <- view chartRefs
  let chartIDs = Map.keys refs

  canvasesWithID <- forM chartIDs $ \cid -> do
    canvas <- new DrawingArea []
    return (cid, canvas)

  return (Map.fromList canvasesWithID)
