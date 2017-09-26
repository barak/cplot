module Model.Frame
    ( FrameData
    , Frame(..)
    ) where

import           Control.Concurrent.STM
import           Graphics.Gloss.Data.Picture

type FrameData = TVar [(Float, Float)]

data Frame = Frame
  { frameData    :: FrameData
  , frameOrigin  :: (Int, Int)
  , framePan     :: (Int, Int)
  , frameDims    :: (Int, Int)
  , framePicture :: Picture
  }
