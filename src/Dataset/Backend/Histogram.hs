module Dataset.Backend.Histogram
  ( histogramDataset
  ) where

import           Dataset.Internal.Types


histogramDataset :: Dataset Point
histogramDataset = Dataset
  { _insert    = (:)
  , _removeEnd = id
  , _dataset   = []
  , _toList    = id
  , _xbounds   = const Nothing
  , _ybounds   = const Nothing
  }
