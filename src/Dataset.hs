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


push :: p -> Dataset p -> Dataset p
push p Dataset{..} =
  Dataset _insert
          _removeEnd
          (_insert p _dataset)
          _toList
          _xbounds
          _ybounds

removeEnd :: Dataset p -> Dataset p
removeEnd Dataset{..} =
  Dataset _insert
          _removeEnd
          (_removeEnd _dataset)
          _toList
          _xbounds
          _ybounds

toList :: Dataset p -> [p]
toList Dataset{..} = _toList _dataset

xbounds, ybounds :: Dataset p -> Maybe (Double, Double)
xbounds Dataset{..} = _xbounds _dataset
ybounds Dataset{..} = _ybounds _dataset
