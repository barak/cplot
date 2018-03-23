{-# LANGUAGE RecordWildCards #-}

module Buffer
  ( Buffer
  , put
  , flush
  ) where

import Buffer.Internal.Types

put :: e -> Buffer e -> Buffer e
put e Buffer{..} =
  Buffer _put
         _flush
         _empty
         (_put e _buffer)

flush :: Buffer e -> (Buffer e, [e])
flush Buffer{..} =
  ( Buffer _put
           _flush
           _empty
           _empty
  , _flush _buffer )
