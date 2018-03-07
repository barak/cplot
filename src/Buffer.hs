-- USAGE:
--   Import this module and any backends you need

{-# LANGUAGE RecordWildCards #-}

module Buffer
  ( Buffer
  , put
  , drain
  ) where

import Buffer.Internal.Types

put :: e -> Buffer e -> Buffer e
put e Buffer{..} = Buffer putBuf drainBuf emptyBuf (putBuf e buffer)

drain :: Buffer e -> (Buffer e, [e])
drain Buffer{..} = (Buffer putBuf drainBuf emptyBuf emptyBuf, drainBuf buffer)
