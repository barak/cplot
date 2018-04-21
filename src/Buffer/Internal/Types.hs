module Buffer.Internal.Types where

-- | Abstract buffer type. The quantifier ensures that internally the buffer
--   itself can be represented by any container-y type.
data Buffer e = forall buf. Buffer
  { _put    :: e -> buf e -> buf e
  , _flush  :: buf e -> [e]
  , _empty  :: buf e
  , _buffer :: buf e
  }
