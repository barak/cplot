module Model
    ( Model(..)
    , PlotData
    , updateModel
    , initializeModel
    ) where

import           Control.Concurrent.STM
import           Data.Set                         (Set)
import qualified Data.Set                         as Set
import           Graphics.Gloss.Interface.IO.Game


type PlotData = TVar [(Float, Float)]

data Model = Model
  { picture        :: Picture
  , windowSize     :: (Int, Int)
  , zoom           :: Double
  , cursorLoc      :: (Double, Double)
  , buttonsPressed :: Set Key
  , clickLoc       :: (Float, Float)
  , plotData       :: PlotData
  }


initializeModel :: PlotData -> Model
initializeModel dat =
  Model
    { picture        = Blank
    , windowSize     = (0, 0)
    , zoom           = 0.0
    , cursorLoc      = (0.0, 0.0)
    , buttonsPressed = Set.empty
    , clickLoc       = (0.0, 0.0)
    , plotData       = dat
    }

updateModel :: Float -> Model -> IO Model
updateModel _ = return
