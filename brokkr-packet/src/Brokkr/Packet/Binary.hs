{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Brokkr.Packet.Binary (
  ToBinary(..)
, FromBinary(..)
, VarInt(..)
, PacketParseError(..)
, Showable(..)
, UUID
, HexByteString(..)
, putEnum, withEnum
) where

import Brokkr.NBT
import Brokkr.PackedVector.Internal
import Brokkr.VarInt (VarInt(..), withVarInt, putVarInt, VarIntDecodeError)

import Control.Exception (Exception)

import Data.Bits

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS
import Data.ByteString.Unsafe qualified as BS

import Data.Int

import Data.Text.Encoding.Error (UnicodeException)

import Data.Kind (Type)

import Data.Proxy

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Storable as S

import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.Word

import FlatParse.Basic qualified as Flatparse

import Foreign.Ptr (plusPtr)
import Foreign.Storable

import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.Exts
import GHC.TypeLits
import GHC.Generics hiding (prec)

import Mason.Builder qualified as Mason
import Data.Void

class ToBinary a where
  put :: a -> Mason.Builder

instance ToBinary Void where
  put _ = mempty

instance ToBinary Int where
  put i = Mason.int64BE (fromIntegral i)
  {-# INLINE put #-}
instance ToBinary Int8 where
  put = Mason.int8
  {-# INLINE put #-}
instance ToBinary Int16 where
  put = Mason.int16BE
  {-# INLINE put #-}
instance ToBinary Int32 where
  put = Mason.int32BE
  {-# INLINE put #-}
instance ToBinary Int64 where
  put = Mason.int64BE
  {-# INLINE put #-}

instance ToBinary Word where
  put i = Mason.word64BE (fromIntegral i)
  {-# INLINE put #-}
instance ToBinary Word8 where
  put = Mason.word8
  {-# INLINE put #-}
instance ToBinary Word16 where
  put = Mason.word16BE
  {-# INLINE put #-}
instance ToBinary Word32 where
  put = Mason.word32BE
  {-# INLINE put #-}
instance ToBinary Word64 where
  put = Mason.word64BE
  {-# INLINE put #-}

instance ToBinary Float where
  put = Mason.floatBE
  {-# INLINE put #-}
instance ToBinary Double where
  put = Mason.doubleBE
  {-# INLINE put #-}

instance ToBinary Bool where
  put = putEnum @Int8
  {-# INLINE put #-}

instance ToBinary VarInt where
  put = putVarInt
  {-# INLINE put #-}

instance ToBinary UUID where
  put uuid =
    let (a,b) = UUID.toWords64 uuid
    in put a <> put b
  {-# INLINE put #-}

instance ToBinary ByteString where
  put bs = Mason.byteString bs
  {-# INLINE put #-}

instance ToBinary a => ToBinary (V.Vector a) where
  put vec = V.foldMap (\a -> put a) vec
  {-# INLINE put #-}

instance (U.Unbox a, ToBinary a) => ToBinary (U.Vector a) where
  put vec = U.foldMap (\a -> put a) vec
  {-# INLINE put #-}

instance Storable a => ToBinary (S.Vector a) where
  put v =
    let (fp, szW) = S.unsafeToForeignPtr0 v
    in Mason.byteString (BS.BS (coerce fp) $ szW * sizeOf (undefined @_ @a))
  {-# INLINE put #-}

instance (KnownNat sz, KnownNat bitSz) => ToBinary (PackedVector ('Static sz) ('Static bitSz) a) where
  put (PVec_SS fp) =
    let elsPerWord = 64 `quot` fromIntegral (natVal (Proxy @bitSz))
        nrOfWords = (fromIntegral (natVal (Proxy @sz)) + elsPerWord - 1) `quot` elsPerWord
    in Mason.byteString (BS.BS (coerce fp) $ nrOfWords * 8)
  {-# INLINE put #-}

instance ToBinary NBT where
  put = putNBT
  {-# INLINE put #-}

instance ToBinary a => ToBinary (Maybe a) where
  put Nothing = put False
  put (Just x) = put True <> put x
  {-# INLINE put #-}

-- FromBinary

data PacketParseError =
    InvalidStringSize Int
  | InvalidUtf8 UnicodeException
  | InvalidEnumValue String Int
  | InvalidPacketId String Int
  | InvalidArraySize Int
  | InvalidFixedArraySize Int Int
  | InvalidNBT NBTError
  | InvalidCompressedPacketThreshold Int Int
  | InvalidCompressedPacketSize Int Int
  | FailedExtraBytes Showable HexByteString
  | FailedGeneric HexByteString
  | VarIntError VarIntDecodeError
  | InvalidPacketSize Int
  deriving stock Show

data Showable = forall a . Show a => Showable a

instance Show Showable where
  showsPrec prec (Showable a) = showsPrec prec a

instance Exception PacketParseError

class FromBinary a where
  with :: (a -> Flatparse.ParserT st PacketParseError r) -> Flatparse.ParserT st PacketParseError r
  with f = get >>= f
  {-# INLINE with #-} 
  -- 'get' is usually fine if you actually need to allocate 'a'
  -- if you want ghc to more eagerly unbox 'a', use 'with'
  --
  -- All single constructor types with strict fields should use 'with'
  -- Examples are all primitive wrappers 'Int/Word', 'ByteString'
  -- 'Text', most small data types.   
  get :: Flatparse.ParserT st PacketParseError a
  get = with pure
  {-# INLINE get #-}

  {-# MINIMAL with|get #-}

instance FromBinary Void where
  get = Flatparse.empty

instance FromBinary Int where
  with f = Flatparse.withAnyInt (f . swapBE)
  {-# INLINE with #-}
instance FromBinary Int8 where
  with = Flatparse.withAnyInt8
  {-# INLINE with #-}
instance FromBinary Int16 where
  with f = Flatparse.withAnyInt16 (f . swapBE16)
  {-# INLINE with #-}
instance FromBinary Int32 where
  with f = Flatparse.withAnyInt32 (f . swapBE32)
  {-# INLINE with #-}
instance FromBinary Int64 where
  with f = Flatparse.withAnyInt64 (f . swapBE64)
  {-# INLINE with #-}

instance FromBinary Word where
  with f = Flatparse.withAnyWord (f . swapBE)
  {-# INLINE with #-}
instance FromBinary Word8 where
  with = Flatparse.withAnyWord8
  {-# INLINE with #-}
instance FromBinary Word16 where
  with f = Flatparse.withAnyWord16 (f . swapBE16)
  {-# INLINE with #-}
instance FromBinary Word32 where
  with f = Flatparse.withAnyWord32 (f . swapBE32)
  {-# INLINE with #-}
instance FromBinary Word64 where
  with f = Flatparse.withAnyWord64 (f . swapBE64)
  {-# INLINE with #-}

instance FromBinary Float where
  with f = with $ f . castWord32ToFloat
  {-# INLINE with #-}
instance FromBinary Double where
  with f = with $ f . castWord64ToDouble
  {-# INLINE with #-}

instance FromBinary Bool where
  with f = Flatparse.withAnyInt8 $ \case
    0 -> f False
    1 -> f True
    n -> Flatparse.err $ InvalidEnumValue "Bool" (fromIntegral n)
  {-# INLINE with #-}

instance FromBinary VarInt where
  with = withVarInt VarIntError
  {-# INLINE with #-}
  get = withVarInt VarIntError pure
  {-# INLINE get #-}

instance FromBinary UUID where
  with f = with $ \a -> with $ \b -> f (UUID.fromWords64 a b)
  {-# INLINE with #-}

instance FromBinary NBT where
  get = Flatparse.ParserT $ \fp eob s st ->
    case parseNBT of
      Flatparse.ParserT g -> case g fp eob s st of
        Flatparse.OK# st' res s' -> Flatparse.OK# st' res s'
        Flatparse.Fail# st' -> Flatparse.Fail# st'
        Flatparse.Err# st' e -> Flatparse.Err# st' $ InvalidNBT e
  {-# INLINE get #-}

instance FromBinary a => FromBinary (Maybe a) where
  with f = with $ \case
    False -> f Nothing
    True  -> with (f . Just)
  {-# INLINE with #-} 

instance (KnownNat sz, KnownNat bitSz) => FromBinary (PackedVector ('Static sz) ('Static bitSz) a) where
  with f = do
    BS.BS fp _ <- Flatparse.take# sz
    f (coerce fp)
    where
      elsPerWord = 64 `quot` fromIntegral (natVal (Proxy @bitSz))
      !(I# sz) = 8 * (fromIntegral (natVal (Proxy @sz)) + elsPerWord - 1) `quot` elsPerWord
  {-# INLINE with #-}

swapBE16 :: (Integral a) => a -> a 
swapBE32 :: (Integral a) => a -> a 
swapBE64 :: (Integral a) => a -> a 
swapBE   :: (Integral a) => a -> a
#ifdef WORDS_BIGENDIAN
swapBE16 = id
swapBE32 = id
swapBE64 = id
swapBE   = id
#else
swapBE16 = fromIntegral . byteSwap16 . fromIntegral
swapBE32 = fromIntegral . byteSwap32 . fromIntegral
swapBE64 = fromIntegral . byteSwap64 . fromIntegral
swapBE   = fromIntegral . byteSwap64 . fromIntegral 
#endif

-- Sometimes useful to display bytestrings
newtype HexByteString = HexBS ByteString

instance Show HexByteString where
  show (HexBS bs) = show $ toHex bs

-- https://stackoverflow.com/questions/10099921/efficiently-turn-a-bytestring-into-a-hex-representation
maxLen :: Int
maxLen = maxBound `quot` 2

hexDig :: Word8 -> Word8
hexDig d
    | d < 10    = d + 48
    | otherwise = d + 87

toHex :: ByteString -> ByteString
toHex bs
    | len > maxLen = error "too long to convert"
    | otherwise    = BS.unsafeCreate nl (go 0)
      where
        len = BS.length bs
        nl  = 2*len
        go i p
            | i == len  = return ()
            | otherwise = case BS.unsafeIndex bs i of
                            w -> do poke p (hexDig $ w `shiftR` 4)
                                    poke (p `plusPtr` 1) (hexDig $ w .&. 0xF)
                                    go (i+1) (p `plusPtr` 2)

putEnum :: forall int a . (ToBinary int, Num int) => a -> Mason.Builder
{-# INLINE putEnum #-}
putEnum a = put @int (fromIntegral (I# (dataToTag# a)))

-- TODO tagToEnum# cannot be applied in a polymorphic setting, so we cannot fully derive this
-- but this is good enough
withEnum :: forall int a r st .
  (KnownNat (Card (Rep a)), KnownSymbol (TyName (Rep a)), FromBinary int, Integral int)
  => (Int# -> a) -> (a -> Flatparse.ParserT st PacketParseError r)
  -> Flatparse.ParserT st PacketParseError r
{-# INLINE withEnum #-}
withEnum toTag f = with @int $ \n' ->
  let !n@(W# n#) = fromIntegral n'
      highBound = fromIntegral $ natVal (Proxy @(Card (Rep a)))
  in if n <= highBound
    then f (toTag (word2Int# n#))
    else Flatparse.err $ InvalidEnumValue (symbolVal (Proxy @(TyName (Rep a)))) (fromIntegral n)

type family Card (f :: Type -> Type) :: Nat where
  Card (D1 _ f) = Card f
  Card (C1 _ _) = 1
  Card (f :+: g) = Card f + Card g

type family TyName (f :: Type -> Type) :: Symbol where
  TyName (D1 ('MetaData name _ _ _) _) = name
