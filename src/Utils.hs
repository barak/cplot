module Utils
    ( takeEvery
    ) where

-- | potentially helpful for simple data decimation
takeEvery :: Int -> [a] -> [a]
takeEvery n xs =
  case drop (n - 1) xs of
    []     -> []
    (y:ys) -> y : takeEvery n ys
