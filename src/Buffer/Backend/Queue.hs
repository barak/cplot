module Buffer.Backend.Queue
  ( queueBuffer
  ) where

import           Buffer.Internal.Types

-- this buffer just retains the order of points without doing anything special
-- with them
queueBuffer :: Buffer a
queueBuffer = Buffer
  { putBuf   = (:)
  , drainBuf = id
  , emptyBuf = []
  , buffer   = []
  }
