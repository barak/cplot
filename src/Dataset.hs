{-# LANGUAGE RecordWildCards #-}

module Dataset
  ( Dataset
  , Point(..)
  , push
  , removeEnd
  , toList
  , xbounds
  , ybounds
  ) where

import           Dataset.Internal.Types


-- | Pushes a single point into the dataset.
push :: p -> Dataset p -> Dataset p
push p Dataset{..} =
  Dataset _insert
          _removeEnd
          (_insert p _dataset)
          _toList
          _xbounds
          _ybounds

-- | Removes a single point from the end of the dataset.
removeEnd :: Dataset p -> Dataset p
removeEnd Dataset{..} =
  Dataset _insert
          _removeEnd
          (_removeEnd _dataset)
          _toList
          _xbounds
          _ybounds

-- | Converts a dataset to a list.
toList :: Dataset p -> [p]
toList Dataset{..} = _toList _dataset

-- | Computes bounds for the dataset, if they can be calculated.
xbounds, ybounds :: Dataset p -> Maybe (Double, Double)
xbounds Dataset{..} = _xbounds _dataset
ybounds Dataset{..} = _ybounds _dataset
