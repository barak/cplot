module Dataset.Backend.MinMaxQueue
  ( MinMaxQueue
  , empty
  , toList
  , fromList
  , push
  , pop
  , pushPop
  , removeEnd
  , maximum
  , minimum
  ) where

import           Data.Default
import           Data.List    hiding (maximum, minimum)
import           Prelude      hiding (maximum, minimum)

-- MinMaxQueue description:
-- Implement using two 'stacks' (lists)
--   PUSH:
--     Push element to stack1 and compute new max & min
--
--   POP:
--     1. If stack2 not empty, pop normally from stack2
--     2. If stack2 empty, push elements from stack1 to stack2
--        using above PUSH function, then POP from stack2
--
--   GET_MAX:
--     Just compute the max of the two max elements on top of the stacks
--
--   GET_MIN:
--     Analogous

-- The first component is the element itself.
-- The second component is the max at time of computation.
-- The third component is the min at time of computation.
type MinMaxStack a = [(a, a, a)]

-- | Min-max queue, offering O(1) push, min, and max, and amortized O(1) pop.
data MinMaxQueue a = MMQ (MinMaxStack a) (MinMaxStack a)
  deriving Show

-- | Convenience functions in the style of lens
_1, _2, _3 :: (a, a, a) -> a
_1 (a,_,_) = a
_2 (_,b,_) = b
_3 (_,_,c) = c

-- | Empty 'MinMaxQueue'
empty :: MinMaxQueue a
empty = MMQ [] []

-- | Writes a list of orderable elements into a 'MinMaxQueue'.
fromList :: Ord a => [a] -> MinMaxQueue a
fromList = foldr push empty

-- | Converts a min-max queue to a list (dropping the min-max information).
toList :: MinMaxQueue a -> [a]
toList (MMQ s s') = map _1 s ++ map _1 (reverse s')

-- | Push a single element onto the stack. The new max and min is computed by
--   updating these respective components in the obvious way.
pushStack :: Ord a => a -> MinMaxStack a -> MinMaxStack a
pushStack a = \case
  []                    -> [(a, a, a)]
  stk@((_, mx, mn) : _) -> (a, max a mx, min a mn) : stk

-- | Pop a single element from the stack (if it exists) and return both the
--   element and the new stack.
popStack :: MinMaxStack a -> Maybe (a, MinMaxStack a)
popStack = \case
  []          -> Nothing
  (a,_,_) : l -> Just (a, l)

-- | Internal function for reversing the stack and recomputing the min-max
--   information.
invertStack :: Ord a => MinMaxStack a -> MinMaxStack a
invertStack = foldl' (flip (pushStack . _1)) []

-- | Push a single element to a 'MinMaxQueue'.
push :: Ord a => a -> MinMaxQueue a -> MinMaxQueue a
push a (MMQ s s') = MMQ (pushStack a s) s'

-- | Pop a single element from a 'MinMaxQueue' (if it exists) and return both
--   the element and the new queue.
pop :: Ord a => MinMaxQueue a -> Maybe (a, MinMaxQueue a)
pop = \case
  MMQ [] [] -> Nothing
  MMQ s  [] -> pop (MMQ [] (invertStack s))
  MMQ s  s' -> case popStack s' of
    Nothing     -> Nothing
    Just (a, l) -> Just (a, MMQ s l)

-- | Find the largest element in a 'MinMaxQueue'.
maximum :: Ord a => MinMaxQueue a -> Maybe a
maximum = \case
  MMQ           []            [] -> Nothing
  MMQ ((_,mx,_):_)            [] -> Just mx
  MMQ           [] ((_,mx',_):_) -> Just mx'
  MMQ ((_,mx,_):_) ((_,mx',_):_) -> Just (max mx mx')

-- | Find the smallest element in a 'MinMaxQueue'.
minimum :: Ord a => MinMaxQueue a -> Maybe a
minimum = \case
  MMQ           []            [] -> Nothing
  MMQ ((_,_,mn):_)            [] -> Just mn
  MMQ           [] ((_,_,mn'):_) -> Just mn'
  MMQ ((_,_,mn):_) ((_,_,mn'):_) -> Just (min mn mn')

-- | Pushes left, pops right, discards popped point.
pushPop :: Ord p => p -> MinMaxQueue p -> MinMaxQueue p
pushPop p mmq =
  case pop mmq of
    Nothing        -> push p empty
    Just (_, mmq') -> push p mmq'

-- | Removes a single element from the end of the queue, ignoring the empty
--   case.
removeEnd :: Ord p => MinMaxQueue p -> MinMaxQueue p
removeEnd mmq =
  case pop mmq of
    Nothing        -> empty
    Just (_, mmq') -> mmq'

instance Default (MinMaxQueue a) where
  def = MMQ [] []
