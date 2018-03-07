{-# LANGUAGE ExistentialQuantification #-}

module Buffer.Internal.Types where

data Buffer e = forall buf. Buffer
  { putBuf   :: e -> buf e -> buf e
  , drainBuf :: buf e -> [e]
  , emptyBuf :: buf e
  , buffer   :: buf e
  }
