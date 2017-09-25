{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Graphics.Gloss.Interface.IO.Game

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Pipes
import qualified Pipes.ByteString                 as P

import           Events
import           Model
import           View


main :: IO ()
main = do
  putStrLn "\nCtrl-D to quit"
  model <- initializeModel <$> newTVarIO []
  _   <- forkIO $ runGloss model
  runEffect $ P.stdin >-> updatePlotData (plotData model)

--------------------------------------------------------------------------------

initPlotData :: IO PlotData
initPlotData = newTVarIO []

addData :: (Float, Float) -> PlotData -> STM ()
addData a = flip modifyTVar' (a:)

updatePlotData :: PlotData -> Consumer P.ByteString IO ()
updatePlotData dat = forever $ do
  raw <- await
  let Right point = parsePoint raw  -- need proper error handling later
  lift $ atomically $ addData point dat

parsePoint :: P.ByteString -> Either String (Float, Float)
parsePoint pt = flip parseOnly pt $ do
  x <- double
  skipSpace
  y <- double
  endOfLine
  return (realToFrac x, realToFrac y)

runGloss :: Model -> IO ()
runGloss model =
  playIO
    (InWindow "cplot" (800, 600) (10, 10))  -- window setup
    white                                   -- background colour
    30                                      -- FPS
    model                                   -- initialize model
    drawModel
    handleEvent
    updateModel
