module Dataset.Backend.Scatter
  ( scatterDataset
  ) where

import           Dataset.Internal.Types


-- | By default, scatter data doesn't do anthing fancy
scatterDataset :: Dataset Point
scatterDataset = Dataset
  { _insert    = (:)
  , _removeEnd = id
  , _dataset   = []
  , _toList    = id
  , _xbounds   = const Nothing
  , _ybounds   = const Nothing
  }
