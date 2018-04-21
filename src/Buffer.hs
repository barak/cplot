{-# LANGUAGE RecordWildCards #-}

module Buffer
  ( Buffer
  , put
  , flush
  ) where

import Buffer.Internal.Types

-- | Put a single element into the buffer.
put :: e -> Buffer e -> Buffer e
put e Buffer{..} =
  Buffer _put
         _flush
         _empty
         (_put e _buffer)

-- | 'Flush' a buffer to a list. Need to return the empty buffer too,
--   since we cannot produce ourselves a generic empty buffer.
flush :: Buffer e -> (Buffer e, [e])
flush Buffer{..} =
  ( Buffer _put
           _flush
           _empty
           _empty
  , _flush _buffer )
