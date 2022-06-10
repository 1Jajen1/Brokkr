module Network.Protocol (
  Protocol(..)
, Compression(..)
, Encryption(..)
) where

data Protocol = Protocol Compression Encryption
  deriving stock Show

data Compression = NoCompression | Threshold Int
  deriving stock Show

data Encryption = NoEncryption
  deriving stock Show
