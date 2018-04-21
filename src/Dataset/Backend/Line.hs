module Dataset.Backend.Line
  ( dataset
  ) where

import qualified Dataset.Backend.MinMaxQueue as MMQ
import           Dataset.Internal.Types


-- | By default, line data sets use a 'MinMaxQueue'.
dataset :: Dataset Point
dataset = Dataset
  { _insert    = MMQ.push
  , _removeEnd = MMQ.removeEnd
  , _dataset   = MMQ.empty
  , _toList    = MMQ.toList
  , _xbounds   = xbounds
  , _ybounds   = const Nothing
  }

xbounds :: MMQ.MinMaxQueue Point -> Maybe (Double, Double)
xbounds dataset' = do
  mx <- MMQ.maximum dataset'
  mn <- MMQ.minimum dataset'
  case (mn, mx) of
    (P2 x _, P2 x' _) -> Just (x, x')
    _                 -> Nothing
