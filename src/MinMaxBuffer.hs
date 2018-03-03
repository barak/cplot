{-# LANGUAGE LambdaCase #-}

module MinMaxBuffer
  ( MinMaxBuffer
  , empty
  , singleton
  , put
  , putBy
  , drain
  ) where

data MinMaxBuffer a
  = Nil
  | Single a
  | Pair a a

put :: Ord a => a -> MinMaxBuffer a -> MinMaxBuffer a
put = putBy compare

putBy :: (a -> a -> Ordering) -> a -> MinMaxBuffer a -> MinMaxBuffer a
putBy cmp a = \case
  Nil      -> Single a
  Single b -> Pair b a
  Pair b c -> case (cmp a b == LT, cmp a c == LT, cmp b c == LT) of
    (False, False, False) -> Pair c a
    (False, False, True)  -> Pair b a
    (True,  True,  False) -> Pair b a
    (True,  True,  True)  -> Pair c a
    _                     -> Pair b c

drain :: MinMaxBuffer a -> [a]
drain = \case
  Nil      -> []
  Single a -> [a]
  Pair a b -> [a, b]

empty :: MinMaxBuffer a
empty = Nil

singleton :: a -> MinMaxBuffer a
singleton = Single
