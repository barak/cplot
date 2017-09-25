module View
    ( drawModel
    ) where

import           Control.Concurrent.STM
import           Graphics.Gloss.Interface.IO.Game
import           Model

drawModel :: Model -> IO Picture
drawModel model = do
  dat <- readTVarIO (plotData model)
  return $ Pictures
    [ Translate x y $ circleSolid 2 | (x, y) <- dat ]
