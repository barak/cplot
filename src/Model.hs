module Model
    ( Model(..)
    , FrameData
    , updateModel
    , initializeModel
    ) where

import           Control.Concurrent.STM
import           Control.Monad
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Graphics.Gloss.Interface.IO.Game

import           Model.Frame


data Model = Model
  { frames         :: [Frame]
  , offset         :: Int
  , windowSize     :: (Int, Int)
  , cursorLoc      :: (Double, Double)
  , clickLoc       :: (Float, Float)
  , buttonsPressed :: Set Key
  }


initializeModel :: IO Model
initializeModel = do
  -- stick with 4 till we have a tiling algorithm
  [d1, d2, d3, d4] <- replicateM 4 (newTVarIO [])
  return Model
    { frames =
        -- hard coded for testing
        [ Frame d1 (-200, 150)  (0, 0) (400 - 30, 300 - 30) Blank
        , Frame d2 (200, 150)   (0, 0) (400 - 30, 300 - 30) Blank
        , Frame d3 (-200, -150) (0, 0) (400 - 30, 300 - 30) Blank
        , Frame d4 (200, -150)  (0, 0) (400 - 30, 300 - 30) Blank
        ]
    , offset         = 30
    , windowSize     = (800, 600)
    , cursorLoc      = (0, 0)
    , clickLoc       = (0, 0)
    , buttonsPressed = Set.empty
    }

updateModel :: Float -> Model -> IO Model
updateModel _ = return
