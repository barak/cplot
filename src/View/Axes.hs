module View.Axes
    ( mkAxes
    ) where

import Graphics.Gloss

offset :: Num a => a
offset = 30

mkAxes :: (Int, Int) -> Picture
mkAxes (x, y) =
  let
    (x', y') = (realToFrac $ x `div` 2, realToFrac $ y `div` 2)
    origin   = (-x' + offset, -y' + offset)
  in Pictures
       [ Line [ (-x' + offset,  y' - offset)
              , origin
              , ( x' - offset, -y' + offset)
              ]
       ]
