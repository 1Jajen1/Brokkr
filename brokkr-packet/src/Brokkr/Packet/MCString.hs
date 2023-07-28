{-# LANGUAGE MagicHash #-}
module Brokkr.Packet.MCString (
  MCString(..), mkMCString
) where

import Brokkr.Packet.Binary
import GHC.TypeLits (Nat, KnownNat, natVal)
import Data.Text.Internal (Text(..))
import Mason.Builder qualified as Mason
import Data.Text.Encoding qualified as Text
import GHC.Exts
import FlatParse.Basic qualified as Flatparse
import Control.Monad
import Data.Proxy
import Data.ByteString qualified as BS

-- | Network string type. Encoded as a utf8 bytestring with the size as a VarInt prefixed
-- The size is only checked on reading. Writing assumes a correct size, so make sure
-- newtypes around this, or via deriving has the correct size!
newtype MCString (maxSize :: Nat) = UnsafeMCString Text

instance ToBinary (MCString maxSize) where
  put (UnsafeMCString text) =
    let bs = Text.encodeUtf8 text
        bsL = BS.length bs
    in   put (VarInt $ fromIntegral bsL) <> Mason.byteString bs -- TODO When mason has a more decent version for builders use that...
  {-# INLINE put #-}

instance KnownNat maxSize => FromBinary (MCString maxSize) where
  with f = with $ \(VarInt len) -> do
    when (len < 0 || len > fromIntegral (natVal (Proxy @maxSize))) $ Flatparse.err (InvalidStringSize $ fromIntegral len)
    let !(I# len#) = fromIntegral len
    -- takeUnsafe# skips the negative length check. We already do that, so this is fine
    bs <- Flatparse.takeUnsafe# len#
    case Text.decodeUtf8' bs of
      Left e -> Flatparse.err $ InvalidUtf8 e
      Right t -> f (UnsafeMCString t)
  {-# INLINE with #-}

mkMCString :: forall maxSize . KnownNat maxSize => Text -> Maybe (MCString maxSize)
mkMCString t@(Text _ _ tLen)
  | tLen <= fromIntegral (natVal $ Proxy @maxSize) = Just $ UnsafeMCString t
  | otherwise = Nothing
