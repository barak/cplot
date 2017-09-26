module Utils
    ( r2f
    ) where

import Control.Arrow ((***))

r2f :: (Real a, Fractional b) => (a, a) -> (b, b)
r2f = realToFrac *** realToFrac
