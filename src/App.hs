{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE TemplateHaskell        #-}

-- Concerned as to whether having frpnow as base is a good idea. Concurrency
-- seems difficult. Might just write regular GTK, or perhaps try to use
-- reactive-banana (which could be better decoupled from GTK than frpnow)

module App (main_) where

import           Control.Arrow                        (left)
import           Data.Default
import           Data.DList                           (DList)
import qualified Data.DList                           as DList
import           Data.Monoid                          ((<>))
import qualified Data.Text                            as T
import           Data.Void                            (Void)

import qualified Control.Concurrent                   as CC
import qualified Control.Concurrent.Async.Lifted.Safe as CALS
import           Control.Lens
import           Control.Monad.Reader
import           Data.IORef                           (IORef)
import qualified Data.IORef                           as IORef

import           Options.Applicative
import qualified Text.Megaparsec                      as MP
import qualified Text.Megaparsec.Char                 as MP

import           Pipes
import qualified Pipes.ByteString                     as P

import           Control.FRPNow
import           Control.FRPNow.GTK
import qualified Graphics.Rendering.Cairo             as Cairo
import           Graphics.UI.Gtk                      (AttrOp ((:=)))
import qualified Graphics.UI.Gtk                      as Gtk

import qualified Chart
import qualified Parser

--------------------------------------------------------------------------------
-- DATA TYPES

data ChartType = Line | Scatter | TimeSeries
  deriving Show

-- | Each chart has a type and a list of data associated with it
type ChartRefs = [IORef Chart]
type Chart = (ChartType, Dataset)

-- | This works but doesn't account for time series data (that should probably
--   be another option)
type Dataset = [DList (Double, Double)]

type App = ReaderT AppEnv Now

-- Global app environment
data AppEnv = AppEnv
  { _appEnvOptions   :: !AppOptions
  , _appEnvChartRefs :: !ChartRefs
  }

-- Command line options
data AppOptions = AppOptions
  { _optChartTypes :: [ChartType]
  }

instance Default AppEnv where
  def = AppEnv def def

instance Default AppOptions where
  def = AppOptions def

makeLensesWith abbreviatedFields ''AppOptions
makeLensesWith camelCaseFields ''AppEnv


--------------------------------------------------------------------------------
-- PARSING

type MParser = MP.Parsec Void T.Text

parseArgs :: IO AppOptions
parseArgs = execParser (info (helper <*> parseOptions)
                             (header "cplot"))

parseOptions :: Parser AppOptions
parseOptions = AppOptions <$> chartTypes'
  where
    chartTypes' =
      some (option chartReader
      ( long "chart"
     <> short 'c'
     <> help "line/scatter (up to four may be specified)"))

-- | Optparse specific ChartType parser
chartReader :: ReadM ChartType
chartReader = parsecReadM parseChartType

-- | Transforms megaparsec parsers into optparse ReadM parsers
parsecReadM :: MParser a -> ReadM a
parsecReadM p = eitherReader (left MP.parseErrorPretty . MP.parse p "" . T.pack)

-- | megaparsec parser for ChartType
-- TODO: maybe write some TH to autogenerate this parser?
parseChartType :: MParser ChartType
parseChartType = (MP.string "line"    >> return Line)
             <|> (MP.string "scatter" >> return Scatter)
             <|> (MP.string "series"  >> return TimeSeries)

-- | Initializes the env (including any state) using the options provided.
--   Should this init gtk widgets too?
initAppEnv :: AppOptions -> IO AppEnv
initAppEnv opts = do
  refs <- forM (opts ^. chartTypes) $ \ct ->
    IORef.newIORef ((ct, []) :: (ChartType, Dataset))

  return $ AppEnv opts refs

-- | Executes app given command line arguments
execApp :: AppOptions -> IO ()
execApp opts = runNowGTK $ runReaderT app =<< liftIO (initAppEnv opts)

-- Everything here seems waaaaaaaaaaay too tightly coupled
app :: App ()
app = do
  mainWindow <- lsync Gtk.windowNew
  lsync $ Gtk.set mainWindow
    [ Gtk.windowTitle          := ("cplot" :: String)
    , Gtk.windowDefaultWidth   := 800
    , Gtk.windowDefaultHeight  := 600
    , Gtk.containerBorderWidth := 10
    ]

  -- cts <- asks (^. options . chartTypes)
  -- liftIO $ print cts

  lsync $ bindQuit mainWindow

  chartCanvas <- lsync Gtk.drawingAreaNew
  lsync $ Gtk.set mainWindow
    [ Gtk.containerChild := chartCanvas ]

  let
    requestRedrawCanvas :: IO ()
    requestRedrawCanvas = Gtk.postGUIAsync $ Gtk.widgetQueueDraw chartCanvas

    -- No idea how to give this access to the env without just defining it in
    -- main
    updateCanvas :: Dataset -> Gtk.DrawingArea -> Cairo.Render ()
    updateCanvas dataset canvas = liftIO $ do
      let renderable = Chart.renderChart (map DList.toList dataset)

      Just win <- Gtk.widgetGetWindow canvas
      Gtk.renderWithDrawWindow win renderable

  lasyncOS $ forever $ do
    requestRedrawCanvas
    CC.threadDelay (1000000 `div` 30)

  -- attach updateCanvas to draw handler (grab first dataset)
  datasetRefs    <- asks (^. chartRefs)
  (_, dataset):_ <- liftIO $ forM datasetRefs IORef.readIORef
  lsync $ chartCanvas `Gtk.on` Gtk.draw $ updateCanvas dataset chartCanvas

  -- This obviously blocks, so you won't see anything, and as far as I can see
  -- there's no good way to run this asynchronously without calling runReaderT,
  -- passing it options directly, etc., which is horrible, and probably won't
  -- sit well with the mechanics of FRPNow anyway
  runEffect pipe

  -- sadly never reached
  lsync $ Gtk.widgetShowAll mainWindow

lsync :: IO a -> App a
lsync  = lift . sync

lasyncOS :: IO a -> App (Event a)
lasyncOS = lift . asyncOS

bindQuit :: Gtk.WidgetClass a => a -> IO ()
bindQuit window = void $ window `Gtk.on` Gtk.deleteEvent $ liftIO Gtk.mainQuit >> return False


--------------------------------------------------------------------------------
-- PIPES

parsePoint :: MonadIO m => Pipe P.ByteString (Double, Double) m ()
parsePoint =
  forever $ do
    raw <- await
    case Parser.point raw of
      Left e  -> liftIO $ putStr $ Parser.parseErrorPretty e
      Right p -> yield p

-- | Something is wrong with this, can't seem to see any chartRefs. Probably
--   a symptom of the weird app stack
updateRefs :: (MonadIO m, MonadReader env m, HasChartRefs env ChartRefs)
           => Consumer (Double, Double) m ()
updateRefs =
  forever $ do
    point <- await
    refs <- asks (^. chartRefs)
    liftIO $ forM_ refs $ \ref -> do
      -- apparently (x:xs) is always empty
      (ctype, x:xs) <- IORef.readIORef ref
      case ctype of
        Line       -> IORef.writeIORef ref (ctype, DList.snoc x point : xs)
        Scatter    -> IORef.writeIORef ref (ctype, DList.snoc x point : xs)
        TimeSeries -> IORef.writeIORef ref (ctype, DList.snoc x point : xs)

-- base monad probably should not be App, or at least if it is, App should have
-- IO as base, since we can then actually use Control.Concurrent.Async.Lifted
-- without it crying about the Now monad
pipe :: Effect App ()
pipe = P.stdin >-> parsePoint >-> updateRefs

main_ :: IO ()
main_ = execApp =<< liftIO parseArgs
