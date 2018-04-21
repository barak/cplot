module Buffer.Backend.MinMax
  ( minMaxBuffer
  , minMaxBufferBy
  ) where

import Buffer.Internal.Types

-- | A MinMaxBuffer only holds at most two elements at a time, the max and the
--   min, in whatever order they arrive in.
data MinMaxBuffer a
  = Nil
  | Single a
  | Pair a a

-- | Put function for orderable types.
put :: Ord a => a -> MinMaxBuffer a -> MinMaxBuffer a
put = putBy compare

-- | Put function for abitrary types, as long as they have a comparison function
--   defined. This is looks annoyingly complicated, but most of the
--   complications arise due to the fact that the elements need to retain their
--   order. If 'a' comes after 'b', and both are inserted successfully, then the
--   order must be [b, a].
putBy :: (a -> a -> Ordering) -> a -> MinMaxBuffer a -> MinMaxBuffer a
putBy cmp x = \case
  Nil      -> Single x
  Single a -> Pair a x
  Pair a b -> case (cmp x a == LT, cmp x b == LT, cmp a b == LT) of
    (False, False, False) -> Pair b x
    (False, False, True)  -> Pair a x
    (True,  True,  False) -> Pair a x
    (True,  True,  True)  -> Pair b x
    _                     -> Pair a b

-- | Flushes the buffer to a list.
flush :: MinMaxBuffer a -> [a]
flush = \case
  Nil      -> []
  Single a -> [a]
  Pair a b -> [b, a]

-- | Alias for the empty buffer, so we don't expose the MinMaxBuffer
--   constructors.
empty :: MinMaxBuffer a
empty = Nil

-- | Wraps the functionality of a MinMaxBuffer in a 'Buffer'.
minMaxBuffer :: Ord a => Buffer a
minMaxBuffer = Buffer
  { _put    = put
  , _flush  = flush
  , _empty  = empty
  , _buffer = empty
  }

-- | Given a comparison function, wrap the functionality of a MinMaxBuffer in a
--   'Buffer'.
minMaxBufferBy :: (a -> a -> Ordering) -> Buffer a
minMaxBufferBy cmp = Buffer
  { _put    = putBy cmp
  , _flush  = flush
  , _empty  = empty
  , _buffer = empty
  }
