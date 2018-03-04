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
putBy cmp x = \case
  Nil      -> Single x
  Single a -> Pair a x
  Pair a b -> case (cmp x a == LT, cmp x b == LT, cmp a b == LT) of
    (False, False, False) -> Pair b x
    (False, False, True)  -> Pair a x
    (True,  True,  False) -> Pair a x
    (True,  True,  True)  -> Pair b x
    _                     -> Pair a b

drain :: MinMaxBuffer a -> [a]
drain = \case
  Nil      -> []
  Single a -> [a]
  Pair a b -> [b, a]

empty :: MinMaxBuffer a
empty = Nil

singleton :: a -> MinMaxBuffer a
singleton = Single
