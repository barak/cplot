module Dataset.Internal.Types where


-- | Enum representing any point cplot can process.
data Point
  = P1 Double
  | P2 Double Double
  deriving (Eq, Ord, Show)

-- | Abstract dataset type. Contains all information required to compute with
--   that dataset.
data Dataset p = forall dat. Dataset
  { _insert    :: p -> dat p -> dat p
  , _removeEnd :: dat p -> dat p
  , _dataset   :: dat p
  , _toList    :: dat p -> [p]
  , _xbounds   :: dat p -> Maybe (Double, Double)
  , _ybounds   :: dat p -> Maybe (Double, Double)
  }
