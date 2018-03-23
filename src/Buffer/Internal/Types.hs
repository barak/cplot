module Buffer.Internal.Types where

data Buffer e = forall buf. Buffer
  { _put    :: e -> buf e -> buf e
  , _flush  :: buf e -> [e]
  , _empty  :: buf e
  , _buffer :: buf e
  }
