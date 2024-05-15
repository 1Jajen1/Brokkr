{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MagicHash #-}
module Brokkr.NBT.Codec.Internal (
  NBTCodec(..)
, CodecContext(..)
, Code
-- api
, dimapCodec
, rmapCodec
, rmapEitherCodec
, lmapCodec
, (<$#>)
, pureC
, (<*#>)
-- class
, HasCodec(..)
, ViaSizedInt(..)
-- combinators
, compound
, requiredField
, requiredFieldVia
, optionalField
, optionalFieldVia
, (.=)
, utf8String
, unsafeIntArray
, unsafeLongArray
, ListFor
, ViaList(..)
-- simplify a codec
, simplifyCodec
) where

import Brokkr.NBT

import Control.DeepSeq
import Control.Monad.ST.Strict (runST)

import Data.Bool

import Data.ByteString (ByteString)

import Data.Coerce
import Data.Int
import Data.Kind (Type)

import Data.Text (Text)

import Data.Primitive

import Data.Vector qualified as V
import Data.Vector.Storable qualified as S

import Language.Haskell.TH qualified as TH

-- import GHC.Exts (unsafeCoerce#)

data CodecContext = Compound | Value

-- | A template haskell based nbt codec
--
-- The context signifies whether we are parsing values or
-- parts of a compound.
--
-- Based on 'autodocodec' but adapted for template-haskell use.
data NBTCodec (c :: CodecContext) i o where
  TagCodec :: NBTCodec Value Tag Tag

  ByteCodec   :: Maybe Text -> NBTCodec Value Int8   Int8
  ShortCodec  :: Maybe Text -> NBTCodec Value Int16  Int16
  IntCodec    :: Maybe Text -> NBTCodec Value Int32  Int32
  LongCodec   :: Maybe Text -> NBTCodec Value Int64  Int64

  FloatCodec  :: Maybe Text -> NBTCodec Value Float  Float
  DoubleCodec :: Maybe Text -> NBTCodec Value Double Double
  
  StringCodec :: Maybe Text -> NBTCodec Value NBTString NBTString

  ByteArrayCodec :: Maybe Text -> NBTCodec Value (S.Vector Int8) (S.Vector Int8)
  IntArrayCodec  :: Maybe Text -> NBTCodec Value (S.Vector (BigEndian Int32)) (S.Vector (BigEndian Int32))
  LongArrayCodec :: Maybe Text -> NBTCodec Value (S.Vector (BigEndian Int64)) (S.Vector (BigEndian Int64))

  ListCodec :: ListFor (ListFor o) ~ SmallArray (ListFor o) => Maybe Text -> NBTCodec Value i o -> NBTCodec Value (ListFor i) (ListFor o)
  
  CompoundCodec :: ListFor o ~ SmallArray o => Maybe Text -> NBTCodec Compound i o -> NBTCodec Value i o

  RequiredKeyCodec :: NBTString -> NBTCodec Value i o -> Maybe Text -> NBTCodec Compound i o
  
  OptionalKeyCodec :: NBTString -> NBTCodec Value i o -> Maybe Text -> NBTCodec Compound (Maybe i) (Maybe o)

  PureCodec :: Code o -> NBTCodec Compound i o
  ApCodec :: NBTCodec Compound i (o -> o') -> NBTCodec Compound i o -> NBTCodec Compound i o'

  RmapCodec :: Code (o -> o') -> NBTCodec c i o -> NBTCodec c i o'
  LmapCodec :: Code (i' -> i) -> NBTCodec c i o -> NBTCodec c i' o

  RmapEitherCodec :: Code (o -> Either NBTError o') -> NBTCodec c i o -> NBTCodec c i o'

type family ListFor i :: Type where
  ListFor Int8 = S.Vector Int8
  ListFor Int16 = S.Vector (BigEndian Int16)
  ListFor Int32 = S.Vector (BigEndian Int32)
  ListFor Int64 = S.Vector (BigEndian Int64)
  ListFor Float = S.Vector (BigEndian Float)
  ListFor Double = S.Vector (BigEndian Double)
  ListFor a = SmallArray a

newtype ViaList a = ViaList a
  deriving newtype (Show, Eq, NFData)

-- Api

-- | Map over both the input and output type of a 'NBTCodec'
dimapCodec :: Code (o -> o') -> Code (i' -> i) -> NBTCodec c i o -> NBTCodec c i' o'
dimapCodec r l = rmapCodec r . lmapCodec l

-- | Map over the input type of 'NBTCodec'
lmapCodec :: Code (i' -> i) -> NBTCodec c i o -> NBTCodec c i' o
lmapCodec = LmapCodec

-- | Map over the output type of 'NBTCodec'
rmapCodec :: Code (o -> o') -> NBTCodec c i o -> NBTCodec c i o'
rmapCodec = RmapCodec

-- | Map over the output type of 'NBTCodec' but allows failing the parse
rmapEitherCodec :: Code (o -> Either NBTError o') -> NBTCodec c i o -> NBTCodec c i o'
rmapEitherCodec = RmapEitherCodec

infixl 4 <$#>
-- | Operator version of 'rmapCodec'. Effectively 'fmap' but over template-haskell
(<$#>) :: Code (o -> o') -> NBTCodec c i o -> NBTCodec c i o'
(<$#>) = rmapCodec

-- | 'pure' but for template-haskell
pureC :: Code o -> NBTCodec Compound i o
pureC = PureCodec

infixl 4 <*#>
-- | Operator version of 'ApCodec'. Effectively '<*>' but over template-haskell
(<*#>) :: NBTCodec Compound i (o -> o') -> NBTCodec Compound i o -> NBTCodec Compound i o'
(<*#>) = ApCodec

-- | Typeclass for types which have a 'NBTCodec'
class HasCodec a where
  codec :: NBTCodec Value a a

instance HasCodec Bool where
  codec = dimapCodec [|| (/= 0) ||] [|| bool 0 1 ||] $ codec @Int8

instance HasCodec Int8 where
  codec = ByteCodec Nothing
instance HasCodec Int16 where
  codec = ShortCodec Nothing
instance HasCodec Int32 where
  codec = IntCodec Nothing
instance HasCodec Int64 where
  codec = LongCodec Nothing
instance HasCodec Float where
  codec = FloatCodec Nothing
instance HasCodec Double where
  codec = DoubleCodec Nothing
instance HasCodec NBTString where
  codec = StringCodec Nothing
instance HasCodec Text where
  codec = dimapCodec [|| toText ||] [|| fromText ||] $ StringCodec Nothing
  -- TODO Check what happens if this is the first argument. It may break...
  -- codecDef = Just [|| T.empty ||]

instance HasCodec (S.Vector Int8) where
  codec = ByteArrayCodec Nothing

instance HasCodec (S.Vector (BigEndian Int32)) where
  codec = IntArrayCodec Nothing
instance HasCodec (S.Vector Int32) where
  codec = dimapCodec [|| arrSwapBE @_ @Int32 ||] [|| arrSwapBE @Int32 ||] codec

instance HasCodec (S.Vector (BigEndian Int64)) where
  codec = LongArrayCodec Nothing
instance HasCodec (S.Vector Int64) where
  codec = dimapCodec [|| arrSwapBE @_ @Int64 ||] [|| arrSwapBE @Int64 ||] codec

instance HasCodec Tag where
  codec = TagCodec

instance HasCodec (ViaList (S.Vector Int8)) where
  codec = dimapCodec [|| ViaList ||] [|| \(ViaList v) -> v ||] $ ListCodec Nothing (codec @Int8)
instance HasCodec (ViaList (S.Vector (BigEndian Int16))) where
  codec = dimapCodec [|| ViaList ||] [|| \(ViaList v) -> v ||] $ ListCodec Nothing (codec @Int16)
instance HasCodec (ViaList (S.Vector (BigEndian Int32))) where
  codec = dimapCodec [|| ViaList ||] [|| \(ViaList v) -> v ||] $ ListCodec Nothing (codec @Int32)
instance HasCodec (ViaList (S.Vector (BigEndian Int64))) where
  codec = dimapCodec [|| ViaList ||] [|| \(ViaList v) -> v ||] $ ListCodec Nothing (codec @Int64)
instance HasCodec (ViaList (S.Vector (BigEndian Float))) where
  codec = dimapCodec [|| ViaList ||] [|| \(ViaList v) -> v ||] $ ListCodec Nothing (codec @Float)
instance HasCodec (ViaList (S.Vector (BigEndian Double))) where
  codec = dimapCodec [|| ViaList ||] [|| \(ViaList v) -> v ||] $ ListCodec Nothing (codec @Double)

-- TODO ViaList instances for byteswapped

instance (ListFor a ~ SmallArray a, HasCodec a) => HasCodec (SmallArray a) where
  codec = ListCodec Nothing (codec @a)

-- TODO: I have this really evil idea to optimize this:
-- data Vector a = Vector (Array# a) Int# Int#
-- data SmallArray a = SmallArray (SmallArray# a)
--
-- Now at runtime Array# ~ SmallArray# and the offset and size is also fixed
-- I wrote an initial version of that below, but this needs tests and benchmarks first
instance (ListFor a ~ SmallArray a, HasCodec a) => HasCodec (V.Vector a) where
  -- codec = dimapCodec
  --   [|| \(sm@(SmallArray sm#) :: SmallArray x) -> V.unsafeFromArraySlice @x (Array (unsafeCoerce# sm#)) 0 (sizeofSmallArray sm) ||]
  --   [|| \(v :: V.Vector x) -> case V.toArray v of (Array a) -> SmallArray @x (unsafeCoerce# a) ||]
  --   $ ListCodec Nothing (codec @a)
  codec = dimapCodec [|| \sm -> V.generate (sizeofSmallArray sm) $ indexSmallArray sm ||] [|| \v ->
      runST $ do
        sm <- newSmallArray (V.length v) (error "smallArr:fromVec:empty" :: a)
        V.ifoldl' (\m i x -> m >> writeSmallArray sm i x) (pure ()) v
        unsafeFreezeSmallArray sm
    ||] $ ListCodec Nothing (codec @a)

newtype ViaSizedInt a = ViaSizedInt Int

instance (HasCodec a, Integral a) => HasCodec (ViaSizedInt a) where
  codec = dimapCodec [|| ViaSizedInt . fromIntegral ||] [|| \(ViaSizedInt n) -> fromIntegral n ||] (codec @a)

-- This more general instance will throw you into the depths of hell
-- TODO Retry this but without using coerce and explicitly doing the wrapping
-- instance (HasCodec a, Integral a, Integral b) => HasCodec (ViaIntegral a b) where
--   codec = dimapCodec (TH.unsafeCodeCoerce [| coerce . fromIntegral |]) (TH.unsafeCodeCoerce [| fromIntegral . coerce |]) (codec @a)
--   codecDef = Just [|| ViaIntegral 0 ||]

-- Combinators

-- | Shorthand for creating a value codec from a compound codec
compound :: ListFor o ~ SmallArray o => Text -> NBTCodec Compound i o -> NBTCodec Value i o
compound = CompoundCodec . Just

-- | Parse a required field given a 'NBTString' key
--
-- @
--    compound "MyCompound" $ [|| MyObj ||]
--      <$#> requiredField "arg1" .= [|| \(MyObj i _) -> i ||]
--      <*#> requiredField "arg2" .= [|| \(MyObj _ j) -> j ||]
-- @
requiredField :: HasCodec a => NBTString -> NBTCodec Compound a a
requiredField key = RequiredKeyCodec key codec Nothing

-- | Parse a required field given a 'NBTString' key
--
-- Parses via the representation of a newtype
--
-- @
--    compound "MyCompound" $ [|| MyObj ||]
--      <$#> requiredFieldVia @Int32 @NewtypeInt32 "arg1" .= [|| \(MyObj i) -> i ||]
-- @
requiredFieldVia :: forall a b . (HasCodec a, Coercible a b) => NBTString -> NBTCodec Compound b b
requiredFieldVia key = RequiredKeyCodec key (dimapCodec [|| coerce ||] [|| coerce ||] $ codec @a) Nothing

-- | Parse an optional field given a 'NBTString' key
--
-- @
--    compound "MyCompound" $ [|| MyObj ||]
--      <$#> optionalField "arg1" .= [|| \(MyObj i) -> i ||]
-- @
optionalField :: HasCodec a => NBTString -> NBTCodec Compound (Maybe a) (Maybe a)
optionalField key = OptionalKeyCodec key codec Nothing

-- | Parse an optional field given a 'NBTString' key
--
-- Parses via the representation of a newtype
--
-- @
--    compound "MyCompound" $ [|| MyObj ||]
--      <$#> optionalFieldVia @Int32 @NewtypeInt32 "arg1" .= [|| \(MyObj i) -> i ||]
-- @
optionalFieldVia :: forall a b . (HasCodec a, Coercible a b) => NBTString -> NBTCodec Compound (Maybe b) (Maybe b)
optionalFieldVia key = OptionalKeyCodec key (dimapCodec [|| coerce ||] [|| coerce ||] $ codec @a) Nothing

infixr 8 .=
-- | Operator version of 'lmapCodec'
--
-- Used to complete compound codecs
--
-- @
--    compound "MyCompound" $ [|| MyObj ||]
--      <$#> optionalFieldVia @Int32 @NewtypeInt32 "arg1" .= [|| \(MyObj i) -> i ||]
-- @
(.=) :: NBTCodec Compound i o -> Code (i' -> i) -> NBTCodec Compound i' o
(.=) = flip lmapCodec

-- | Read 'NBTString' and convert into a utf8 'ByteString'
--
-- If the underlying data is already valid utf8 this method will not copy. (Currently this is a lie. It will always copy)
utf8String :: Maybe Text -> NBTCodec Value ByteString ByteString
utf8String = dimapCodec [|| toUtf8 ||] [|| fromUtf8 ||] . StringCodec

-- | Read 'IntArray' and byteswap in place
--
-- Warning: This will modify the input bytestring!
unsafeIntArray :: Maybe Text -> NBTCodec Value (S.Vector Int32) (S.Vector Int32)
unsafeIntArray = dimapCodec [|| unsafeArrSwapBE @_ @Int32 ||] [|| unsafeArrSwapBE @Int32 ||] . IntArrayCodec

-- | Read 'LongArray' and byteswap in place
--
-- Warning: This will modify the input bytestring!
unsafeLongArray :: Maybe Text -> NBTCodec Value (S.Vector Int64) (S.Vector Int64)
unsafeLongArray = dimapCodec [|| unsafeArrSwapBE @_ @Int64 ||] [|| unsafeArrSwapBE @Int64 ||] . LongArrayCodec

type Code a = TH.Code TH.Q a

-- | Fold over the codec and simplify some patterns
simplifyCodec :: forall i x . NBTCodec Compound i x -> NBTCodec Compound i x
simplifyCodec (LmapCodec l i) = constantFold (LmapCodec l (simplifyCodec i))
simplifyCodec (RmapCodec r i) = constantFold (RmapCodec r (simplifyCodec i))
simplifyCodec (ApCodec ff fa) = constantFold (ApCodec (simplifyCodec ff) (simplifyCodec fa))
simplifyCodec p = constantFold p
-- simplifyCodec p = p

constantFold :: forall i x . NBTCodec Compound i x -> NBTCodec Compound i x

-- Try to collapse <*> and try to combine constant expressions
-- We try to move rmap further inside, closer towards constants and lmap outwards
-- We also reassociate <*> towards the left

-- f <$> pure a = pure (f a)
constantFold (RmapCodec f (PureCodec a)) = PureCodec [|| $$(f) $$(a) ||]
-- lmap f (pure a) = pure (f a)
constantFold (LmapCodec _ (PureCodec a)) = PureCodec [|| $$(a) ||]
-- f <$> (g <$> fa) = (f . g) <$> fa
constantFold (RmapCodec f (RmapCodec g fa)) = RmapCodec [|| $$(f) . $$(g) ||] fa
-- lmap f . lmap g = lmap (g . f)
constantFold (LmapCodec f (LmapCodec g fa)) = LmapCodec [|| $$(g) . $$(f) ||] fa
-- pure f <*> fa = f <$> fa
constantFold (ApCodec (PureCodec f) fa) = constantFold $ RmapCodec f fa
-- fl <*> (fr <*> fa) = (((.) <$> fl) <*> fr) <*> fa
constantFold (ApCodec fl (ApCodec fr fa)) = constantFold $ ApCodec (constantFold $ ApCodec (constantFold $ RmapCodec [|| (.) ||] fl) fr) fa
-- f <$> (fl <*> fa) = ((. f) <$> fl) <*> g
constantFold (RmapCodec f (ApCodec ff fa)) = constantFold $ ApCodec (constantFold $ RmapCodec [|| (.) $$(f) ||] ff) fa
-- f <$> (lmap r fa) = lmap r (f <$> fa)
constantFold (RmapCodec f (LmapCodec r fa)) = constantFold $ LmapCodec r (constantFold $ RmapCodec f fa)

constantFold p = p
