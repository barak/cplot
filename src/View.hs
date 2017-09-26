module View
    ( drawModel
    ) where

import           Control.Concurrent.STM
import           Graphics.Gloss.Interface.IO.Game

import           Model
import           View.Axes

offset :: Num a => a
offset = 30

drawModel :: Model -> IO Picture
drawModel model = do
  let
    (width, height) = windowSize model
    (ox, oy)        = (-width `div` 2 + offset, -height `div` 2 + offset)
    (ox', oy')      = (realToFrac ox, realToFrac oy)

  dat <- readTVarIO (plotData model)
  return $ Pictures $
    mconcat
      [ [ Translate (x + ox') (y + oy') $ circleSolid 2 | (x, y) <- dat ]
      , [ mkAxes (width, height) ]
      ]
