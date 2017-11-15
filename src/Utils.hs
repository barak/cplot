module Utils
  ( takeEvery
  , packToSquare
  ) where

-- | potentially helpful for simple data decimation
takeEvery :: Int -> [a] -> [a]
takeEvery n xs =
  case drop (n - 1) xs of
    []     -> []
    (y:ys) -> y : takeEvery n ys

-- | Given some number of items, generates a packing table that can be
--   fed basically verbatim to Gtk to pack the charts nicely (for certain
--   values of 'nicely')
packToSquare :: Int -> [(Int, Int, Int)]
packToSquare n = go n (ceiling $ sqrt $ realToFrac n) 0
  where
    go n m k
      | n >= m = [(k, i, 1) | i <- [0 .. m - 1]] ++ go (n - m) m (k + 1)
      | n <  m = if n /= 0
                   then let j = if n == 0 then 0 else m `div` n
                        in [(k, i, j) | i <- [0, j .. m - 1]]
                   else []
