module Dataset.Backend.Scatter
  ( dataset
  ) where

import           Dataset.Internal.Types


-- | The dataset for a histogram is not terribly interesting, it's basically
--   just a list.
dataset :: Dataset Point
dataset = Dataset
  { _insert    = (:)
  , _removeEnd = id
  , _dataset   = []
  , _toList    = id
  , _xbounds   = const Nothing
  , _ybounds   = const Nothing
  }
