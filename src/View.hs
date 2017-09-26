module View
    ( drawModel
    ) where

import           Control.Concurrent.STM
import           Graphics.Gloss.Interface.IO.Game

import           Model
import           Model.Frame
import           View.Axes
import           Utils


drawModel :: Model -> IO Picture
drawModel model = Pictures <$> mapM drawFrame (frames model)

drawFrame :: Frame -> IO Picture
drawFrame frame = do
  let
    (w, h)      = r2f $ frameDims frame
    (ox, oy)    = r2f $ frameOrigin frame
    frameAxes   = mkAxes frame
    frameBorder = mkBorder frame

  dat <- readTVarIO (frameData frame)

  return $ Pictures
    [ frameAxes
    , frameBorder
    , Pictures
        [ Translate (x + ox) (y + oy) $ circleSolid 2
        | (x, y) <- dat
        , -w / 2 < x && x < w / 2
        , -h / 2 < y && y < h / 2
        ]
    ]
