{-# LANGUAGE ExistentialQuantification #-}

module Dataset.Internal.Types where


data Point
  = Single Double
  | Point Double Double
  deriving (Eq, Ord, Show)

data Dataset p = forall dat. Dataset
  { _insert    :: p -> dat p -> dat p
  , _removeEnd :: dat p -> dat p
  , _dataset   :: dat p
  , _toList    :: dat p -> [p]
  , _xbounds   :: dat p -> Maybe (Double, Double)
  , _ybounds   :: dat p -> Maybe (Double, Double)
  }
