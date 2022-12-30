module Network.Protocol (
  Protocol(..)
, Compression(..)
, Encryption(..)
) where

-- This is a bunch of ptr chasing since we cannot unbox this.
-- Maybe instead flatten this at some point. But after network benchmarks are in place
-- Idea being: Make this an unboxed sum. Should compile into one tag int + whatever encryption needs later on + 64 bits for the compression threshold
data Protocol = Protocol Compression Encryption
  deriving stock Show

data Compression = NoCompression | Threshold Int
  deriving stock Show

data Encryption = NoEncryption
  deriving stock Show
