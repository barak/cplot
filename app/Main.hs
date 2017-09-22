{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Graphics.Gloss.Interface.IO.Animate

import           Control.Concurrent
import           Control.Concurrent.STM
import           Control.Monad
import           Data.Attoparsec.ByteString.Char8
import           Pipes
import qualified Pipes.ByteString                    as P

main :: IO ()
main = do
  putStrLn "\nCtrl-D to quit"
  dat <- initPlotData
  _   <- forkIO $ runGloss dat
  runEffect $ P.stdin >-> updatePlotData dat

--------------------------------------------------------------------------------

type PlotData a = TVar [a]


initPlotData :: IO (PlotData a)
initPlotData = newTVarIO []

addData :: a -> PlotData a -> STM ()
addData a = flip modifyTVar (a:)

readData :: PlotData a -> STM [a]
readData = readTVar

updatePlotData :: PlotData (Float, Float) -> Consumer P.ByteString IO ()
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

-- May need to change animateIO -> playIO for flexibility
runGloss :: PlotData (Float, Float) -> IO ()
runGloss dat = animateIO
  (InWindow "cplot" (800, 600) (10, 10))  -- window setup
  white                                   -- background colour

  -- update function
  (\_ -> do
    dat' <- atomically $ readData dat
    return $ Pictures [ translate x y $ circleSolid 2 | (x, y) <- dat' ])

  -- controller function (not sure if we'll need this yet)
  (\_ -> return ())
