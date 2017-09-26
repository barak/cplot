module View.Axes
    ( mkAxes
    , mkBorder
    ) where

import           Graphics.Gloss
import           Model.Frame
import           Utils


mkAxes :: Frame -> Picture
mkAxes frame =
  Pictures
    [ Line $ map r2f
        [ (ox, oy + h `div` 2)
        , (ox, oy - h `div` 2)
        ]
    , Line $ map r2f
        [ (ox - w `div` 2, oy)
        , (ox + w `div` 2, oy)
        ]
    ]
  where
    (w, h)   = frameDims frame
    (ox, oy) = frameOrigin frame

mkBorder :: Frame -> Picture
mkBorder frame =
  Pictures
    [ Translate ox oy $ rectangleWire x y
    ]
  where
    (ox, oy) = r2f $ frameOrigin frame
    (x, y)   = r2f $ frameDims frame
