module Buffer.Backend.Queue
  ( queueBuffer
  ) where

import           Buffer.Internal.Types

-- This buffer just retains the order of points without doing anything special
-- with them. You can think of it as an identity buffer of sorts.
queueBuffer :: Buffer a
queueBuffer = Buffer
  { _put    = (:)
  , _flush  = id
  , _empty  = []
  , _buffer = []
  }
