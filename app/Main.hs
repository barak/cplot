module Main where

import           Data.Attoparsec.ByteString.Char8
import           Pipes
import qualified Pipes.ByteString                 as P

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad

import           Graphics.UI.Gtk                  as Gtk

import           Control.Lens
import           Data.Colour
import           Data.Colour.Names
import           Data.Default.Class
import           Graphics.Rendering.Chart
import           Graphics.Rendering.Chart.Gtk

main :: IO ()
main = do
  initGUI

  mainWindow <- windowNew
  Gtk.set mainWindow
    [ windowTitle          := "test"
    , windowDefaultWidth   := 800
    , windowDefaultHeight  := 600
    , containerBorderWidth := 1
    ]

  canvas <- drawingAreaNew
  containerAdd mainWindow canvas

  datTVar <- newTVarIO (False, [])
  forkIO $ runEffect $ P.stdin >-> updatePlotData datTVar

  -- update the UI 60 times per second, but only if data is there
  forkIO $ forever $ do
    (newData, dat) <- readTVarIO datTVar
    when newData $ do
      postGUIAsync $ void $ updateCanvas (buildChart "test" [dat]) canvas
      atomically $ modifyTVar datTVar (\(_, pts) -> (False, pts))
    threadDelay (1000000 `div` 60)

  widgetShowAll mainWindow
  onDestroy mainWindow mainQuit

  mainGUI


-- Builds test line chart
buildChart :: String -> [[(Double, Double)]] -> Renderable ()
buildChart chartTitle datasets = toRenderable layout
  where
    linePlot =
        plot_lines_values .~ datasets
      $ plot_lines_style  .~ solidLine 2 (opaque blue)
      $ def

    layout =
        layout_title .~ chartTitle
      $ layout_plots .~ [ toPlot linePlot ]
      $ def

updatePlotData :: TVar (Bool, [(Double, Double)]) -> Consumer P.ByteString IO ()
updatePlotData datTVar =
  forever $ do
    raw <- await
    let Right pt = parsePoint raw
    lift $ atomically $ modifyTVar datTVar (\(_, pts) -> (True, pt:pts))

parsePoint :: P.ByteString -> Either String (Double, Double)
parsePoint s =
  flip parseOnly s $ do
    x <- double
    skipSpace
    y <- double
    endOfLine
    return (x, y)
