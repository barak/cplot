{-# LANGUAGE OverloadedStrings #-}

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
import           Model.Frame
import           View


main :: IO ()
main = do
  putStrLn "\nCtrl-D to quit"
  model <- initializeModel

  _ <- forkIO $ runGloss model
  runEffect $ P.stdin >-> updateFrameData (frames model)

--------------------------------------------------------------------------------

addData :: (Float, Float) -> FrameData -> STM ()
addData a = flip modifyTVar' (a:)

updateFrameData :: [Frame] -> Consumer P.ByteString IO ()
updateFrameData fs =
  forever $ do
    raw <- await
    let Right (ix, point) = parsePoint raw
    lift $ atomically $ addData point (frameData $ fs !! (ix - 1))

parsePoint :: P.ByteString -> Either String (Int, (Float, Float))
parsePoint pt = flip parseOnly pt $ do
  frame <- decimal
  _     <- char ':'
  x <- double
  skipSpace
  y <- double
  endOfLine
  return (frame, (realToFrac x, realToFrac y))

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
