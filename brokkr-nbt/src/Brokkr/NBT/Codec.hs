{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingStrategies #-}
module Brokkr.NBT.Codec (
  NBTCodec
, CodecContext(..)
-- api
, dimapCodec
, rmapCodec
, lmapCodec
, (<$#>)
, pureC
, (<*#>)
-- class
, HasCodec(..)
-- combinators
, compound
, requiredField
, optionalField
, (.=)
, utf8String
, unsafeIntArray
, unsafeLongArray
-- gen
, genParser
, genNBTReader
) where

import Brokkr.NBT.ByteOrder
import Brokkr.NBT.Internal
import Brokkr.NBT.NBTString

import Control.Monad

import Data.Bool (bool)

import Data.ByteString (ByteString)

import Data.Int

import Data.Text (Text)

import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S

import Data.Word

import FlatParse.Basic qualified as FP

import GHC.Exts (Int#)
import GHC.Float

import Language.Haskell.TH qualified as TH

import Brokkr.NBT.Slice

data CodecContext = Compound | Value

-- | A template haskell based nbt codec
data NBTCodec (c :: CodecContext) i o where
  TagCodec :: NBTCodec Value Tag Tag

  ByteCodec   :: Maybe Text -> NBTCodec Value Int8   Int8
  ShortCodec  :: Maybe Text -> NBTCodec Value Int16  Int16
  IntCodec    :: Maybe Text -> NBTCodec Value Int32  Int32
  LongCodec   :: Maybe Text -> NBTCodec Value Int64  Int64

  FloatCodec  :: Maybe Text -> NBTCodec Value Float  Float
  DoubleCodec :: Maybe Text -> NBTCodec Value Double Double
  
  StringCodec :: Maybe Text -> NBTCodec Value NBTString NBTString

  ByteArrayCodec :: Maybe Text -> NBTCodec Value (S.Vector Int8)    (S.Vector Int8)
  IntArrayCodec  :: Maybe Text -> NBTCodec Value (S.Vector Int32BE) (S.Vector Int32BE)
  LongArrayCodec :: Maybe Text -> NBTCodec Value (S.Vector Int64BE) (S.Vector Int64BE)

  ListCodec :: Maybe Text -> NBTCodec Value i o -> NBTCodec Value (V.Vector i) (V.Vector o)
  
  CompoundCodec :: Maybe Text -> NBTCodec Compound i o -> NBTCodec Value i o

  RequiredKeyCodec :: NBTString -> NBTCodec Value i o -> Maybe Text -> NBTCodec Compound i o
  
  OptionalKeyCodec :: NBTString -> NBTCodec Value i o -> Maybe Text -> NBTCodec Compound (Maybe i) (Maybe o)

  PureCodec :: Code o -> NBTCodec Compound i o
  ApCodec :: NBTCodec Compound i (o -> o') -> NBTCodec Compound i o -> NBTCodec Compound i o'

  -- TODO Add another codec for Dimap that may fail on the output
  RmapCodec :: Code (o -> o') -> NBTCodec c i o -> NBTCodec c i o'
  LmapCodec :: Code (i' -> i) -> NBTCodec c i o -> NBTCodec c i' o

-- Api
dimapCodec :: Code (o -> o') -> Code (i' -> i) -> NBTCodec c i o -> NBTCodec c i' o'
dimapCodec r l = rmapCodec r . lmapCodec l

lmapCodec :: Code (i' -> i) -> NBTCodec c i o -> NBTCodec c i' o
lmapCodec = LmapCodec

rmapCodec :: Code (o -> o') -> NBTCodec c i o -> NBTCodec c i o'
rmapCodec = RmapCodec

infixl 4 <$#>
(<$#>) :: Code (o -> o') -> NBTCodec c i o -> NBTCodec c i o'
(<$#>) = rmapCodec

pureC :: Code o -> NBTCodec Compound i o
pureC = PureCodec

infixl 4 <*#>
(<*#>) :: NBTCodec Compound i (o -> o') -> NBTCodec Compound i o -> NBTCodec Compound i o'
(<*#>) = ApCodec

-- class
class HasCodec a where
  codec :: NBTCodec Value a a

instance HasCodec Bool where
  codec = dimapCodec [|| (/= 0) ||] [|| bool 1 0 ||] $ codec @Int8

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

instance HasCodec (S.Vector Int8) where
  codec = ByteArrayCodec Nothing

instance HasCodec (S.Vector Int32BE) where
  codec = IntArrayCodec Nothing
instance HasCodec (S.Vector Int32) where
  codec = dimapCodec [|| arrSwapBE32 ||] [|| arrSwapBE32 ||] codec

instance HasCodec (S.Vector Int64BE) where
  codec = LongArrayCodec Nothing
instance HasCodec (S.Vector Int64) where
  codec = dimapCodec [|| arrSwapBE64 ||] [|| arrSwapBE64 ||] codec

instance HasCodec a => HasCodec [a] where
  codec = dimapCodec [|| V.toList ||] [|| V.fromList ||] $ ListCodec Nothing codec

instance HasCodec a => HasCodec (V.Vector a) where
  codec = ListCodec Nothing codec

instance HasCodec Tag where
  codec = TagCodec

-- Combinators

compound :: Text -> NBTCodec Compound i o -> NBTCodec Value i o
compound = CompoundCodec . Just

requiredField :: HasCodec a => NBTString -> NBTCodec Compound a a
requiredField key = RequiredKeyCodec key codec Nothing

optionalField :: HasCodec a => NBTString -> NBTCodec Compound (Maybe a) (Maybe a)
optionalField key = OptionalKeyCodec key codec Nothing

infixr 8 .=
(.=) :: NBTCodec Compound i o -> Code (i' -> i) -> NBTCodec Compound i' o
(.=) = flip lmapCodec

-- | Read 'NBTString' and convert into a utf8 'ByteString'
--
-- If the underlying data is already valid utf8 this method will not allocate.
utf8String :: Maybe Text -> NBTCodec Value ByteString ByteString
utf8String = dimapCodec [|| toUtf8 ||] [|| fromUtf8 ||] . StringCodec

unsafeIntArray :: Maybe Text -> NBTCodec Value (S.Vector Int32) (S.Vector Int32)
unsafeIntArray = dimapCodec [|| arrSwapBE32 ||] [|| arrSwapBE32 ||] . IntArrayCodec

unsafeLongArray :: Maybe Text -> NBTCodec Value (S.Vector Int64) (S.Vector Int64)
unsafeLongArray = dimapCodec [|| arrSwapBE64 ||] [|| arrSwapBE64 ||] . LongArrayCodec

-- Generate a parser

-- | Generate a parser which reads binary 'NBT'
genParser :: forall i0 o st . NBTCodec Value i0 o -> Code (FP.ParserT st DecodeError o)
genParser c = [|| FP.anyWord8 >>= \t -> parseNBTString >> $$(go [] c) t ||]
  where
    go :: forall i x . [Text] -> NBTCodec Value i x -> Code (Word8 -> FP.ParserT st DecodeError x)
    go _ TagCodec = [|| parseTag ||]

    go path (ByteCodec  name) = [|| \t -> if t == 1 then FP.anyInt8    else FP.err $ invalidType path "byte"  name ||]
    go path (ShortCodec name) = [|| \t -> if t == 2 then FP.anyInt16be else FP.err $ invalidType path "short" name ||]
    go path (IntCodec   name) = [|| \t -> if t == 3 then FP.anyInt32be else FP.err $ invalidType path "int"   name ||]
    go path (LongCodec  name) = [|| \t -> if t == 4 then FP.anyInt64be else FP.err $ invalidType path "long"  name ||]

    go path (FloatCodec  name) = [|| \t -> if t == 5 then castWord32ToFloat  <$> FP.anyWord32be else FP.err $ invalidType path "float"  name ||]
    go path (DoubleCodec name) = [|| \t -> if t == 6 then castWord64ToDouble <$> FP.anyWord64be else FP.err $ invalidType path "double" name ||]

    go path (ByteArrayCodec name) = [|| \t -> if t == 7  then takeArray else FP.err $ invalidType path "byte array" name ||]
    go path (IntArrayCodec name)  = [|| \t -> if t == 11 then takeArray else FP.err $ invalidType path "int array"  name ||]
    go path (LongArrayCodec name) = [|| \t -> if t == 12 then takeArray else FP.err $ invalidType path "long array" name ||]

    go path (StringCodec name) = [|| \t -> if t == 8 then parseNBTString else FP.err $ invalidType path "string" name ||]

    go path (ListCodec name inner) = [|| \t -> if t == 9
      then do
        innerT <- FP.anyWord8
        len <- FP.anyInt32be
        V.replicateM (fromIntegral len) $ $$(go ("<list" <> maybe "" ("#"<>) name <> ">" : path) inner) innerT 
      else FP.err $ invalidType path "list" name ||]
    
    go path (CompoundCodec name inner) = [|| \t -> if t == 10
      then $$(goCompound ("<compound" <> maybe "" ("#"<>) name <> ">" : path) inner)
      else FP.err $ invalidType path "compound" name
      ||]
    
    go path (RmapCodec f inner) = [|| fmap $$(f) . $$(go path inner) ||]
    go path (LmapCodec _ inner) = go path inner

    goCompound :: forall i1 x0 . [Text] -> NBTCodec Compound i1 x0 -> Code (FP.ParserT st DecodeError x0)
    goCompound path inner'
      | PureCodec a <- inner = [|| skipCompound >> pure $$(a) ||]
      | [] <- keysToParsers = error "Codec for a compound that can never succeed" -- TODO Show codec (needs prettyprinter dep to look decent)
      | [(k,def,cont)] <- keysToParsers = [||
        let goRec = do
              t <- FP.anyWord8
              if t == 0
                then $$(maybe [|| FP.empty ||] (\x -> TH.unsafeCodeCoerce [| $(topLevelF) $(x) |]) def)
                else withNBTString $ \k' ->
                  if k == k'
                    then $$(TH.unsafeCodeCoerce $ cont topLevelF) t >>= \x -> skipCompound >> pure x
                    else skipTag t >> goRec
          in goRec
        ||]
      | otherwise = undefined
      where
        inner = simplifyCodec inner'

        -- [key,default,cont -> parser]
        keysToParsers = collectKeys inner

        collectKeys :: forall i x . NBTCodec Compound i x -> [(NBTString, Maybe (TH.Q TH.Exp), TH.Q TH.Exp -> TH.Q TH.Exp)]
        collectKeys (PureCodec _) = []
        collectKeys (RmapCodec f i) = (\(k,d,e) -> (k,d,\e' -> [| fmap $(TH.unTypeCode f) . $(e e') |])) <$> collectKeys i
        collectKeys (LmapCodec _ i) = collectKeys i
        collectKeys (RequiredKeyCodec key i _descr) =
          [(key, Nothing, \cont -> [| $(TH.unTypeCode $ go (toText key : path) i) >=> $(cont) |])]
        -- TODO Failure should map to Maybe instead!
        collectKeys (OptionalKeyCodec key i _descr) =
          [(key, Just [| Nothing |], \cont -> [| $(TH.unTypeCode $ go (toText key : path) i) >=> $(cont) |])]
        collectKeys (ApCodec ff fa) = collectKeys ff <> collectKeys fa

        topLevelF = extractTopLevelFunc inner

        extractTopLevelFunc :: forall i x . NBTCodec Compound i x -> TH.Q TH.Exp
        extractTopLevelFunc (ApCodec ff _) = extractTopLevelFunc ff
        extractTopLevelFunc (RmapCodec f _) = TH.unTypeCode f
        extractTopLevelFunc (LmapCodec _ i) = extractTopLevelFunc i
        extractTopLevelFunc RequiredKeyCodec{} = [| pure |]
        extractTopLevelFunc _ = error "Cannot extract!"

simplifyCodec :: forall i x . NBTCodec Compound i x -> NBTCodec Compound i x
simplifyCodec (LmapCodec l i) = constantFold (LmapCodec l (simplifyCodec i))
simplifyCodec (RmapCodec r i) = constantFold (RmapCodec r (simplifyCodec i))
simplifyCodec (ApCodec ff fa) = constantFold (ApCodec (simplifyCodec ff) (simplifyCodec fa))
simplifyCodec p = constantFold p

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

skipCompound :: FP.ParserT st e ()
{-# INLINE skipCompound #-}
skipCompound = do
  t <- FP.anyWord8
  if t == 0 then pure () else skipKey >> skipTag t >> skipCompound

skipKey :: FP.ParserT st e ()
{-# INLINE skipKey #-}
skipKey = FP.anyInt16be >>= void . FP.skip . fromIntegral

-- TODO I need to perform a heap check here to avoid hogging a thread.
-- This is 100% allocation free and thus very dangerous when applied to
-- user input, especially if that input came compressed.
skipTag :: Word8 -> FP.ParserT st e ()
{-# INLINE skipTag #-}
-- int8, int16, int32, int64, float, double
skipTag 1 = FP.skip 1
skipTag 2 = FP.skip 2
skipTag 3 = FP.skip 4
skipTag 4 = FP.skip 8
skipTag 5 = FP.skip 4
skipTag 6 = FP.skip 8
-- byte/int/long array
skipTag 7  = FP.anyInt32be >>= void . FP.skip . fromIntegral
skipTag 11 = FP.anyInt32be >>= void . FP.skip . fromIntegral
skipTag 12 = FP.anyInt32be >>= void . FP.skip . fromIntegral
-- strings
skipTag 8 = skipKey
-- lists
skipTag 9 = do
  t <- FP.anyWord8
  sz <- FP.anyInt32be
  let go  0 = pure ()
      go !n = skipTag t >> go (n - 1)
  go sz
-- compounds
skipTag 10 = skipCompound
-- anything else is bad
skipTag _ = FP.empty

data Trie

-- TODO remember to yield on skipTag calls to prevent exploits

-- | Generate a parser which reads already parsed 'NBT'
genNBTReader :: forall i0 o . NBTCodec Value i0 o -> Code (NBT -> Either DecodeError o)
genNBTReader c = [|| \(NBT _ t) -> $$(go [] c) t ||]
  where
    go :: forall i x . [Text] -> NBTCodec Value i x -> Code (Tag -> Either DecodeError x)
    go _ TagCodec   = [|| Right ||]

    go path (ByteCodec name) = [|| \case
      TagByte b -> Right b
      _ -> Left $ invalidType path "byte" name
      ||]
    go path (ShortCodec name) = [|| \case
      TagShort b -> Right b
      _ -> Left $ invalidType path "short" name
      ||]
    go path (IntCodec name) = [|| \case
      TagInt b -> Right b
      _ -> Left $ invalidType path "int" name
      ||]
    go path (LongCodec name) = [|| \case
      TagLong b -> Right b
      _ -> Left $ invalidType path "long" name
      ||]
    go path (FloatCodec name) = [|| \case
      TagFloat b -> Right b
      _ -> Left $ invalidType path "float" name
      ||]
    go path (DoubleCodec name) = [|| \case
      TagDouble b -> Right b
      _ -> Left $ invalidType path "double" name
      ||]
    
    go path (StringCodec name) = [|| \case
      TagString b -> Right b
      _ -> Left $ invalidType path "string" name
      ||]
    
    go path (ByteArrayCodec name) = [|| \case
      TagByteArray b -> Right b
      _ -> Left $ invalidType path "byte array" name
      ||]
    go path (IntArrayCodec name) = [|| \case
      TagIntArray b -> Right b
      _ -> Left $ invalidType path "int array" name
      ||]
    go path (LongArrayCodec name) = [|| \case
      TagLongArray b -> Right b
      _ -> Left $ invalidType path "long array" name
      ||]

    go path (ListCodec name inner) = [|| \case
      -- TagList v
      --   | V.null v -> Right V.empty
      --   | otherwise -> traverse ($$(go ("<list" <> maybe "" ("#"<>) name <> ">" : path) inner)) v
      _ -> Left $ invalidType path "list" name
      ||]
    
    go path (CompoundCodec name inner) = [|| \case
      TagCompound v -> $$(goCompound ("<compound" <> maybe "" ("#"<>) name <> ">" : path) inner) v
      _ -> Left $ invalidType path "compound" name
      ||]

    go path (RmapCodec f inner) = [|| fmap $$(f) . $$(go path inner) ||]
    go path (LmapCodec _ inner) = go path inner

    goCompound :: forall i x . [Text] -> NBTCodec Compound i x -> Code (Slice NBT -> Either DecodeError x)
    goCompound _ (PureCodec x) = [|| const (Right $$(x)) ||]

    goCompound path (ApCodec ff fa) = [|| \v -> $$(goCompound path ff) v <*> $$(goCompound path fa) v ||]

    goCompound path (RmapCodec f inner) = [|| fmap $$(f) . $$(goCompound path inner) ||]
    goCompound path (LmapCodec _ inner) = goCompound path inner

    goCompound path (RequiredKeyCodec key inner _descr) = [|| \slice ->
      case findWithIndexNBT key slice of
        (# _, (# (NBT _ t) | #) #) -> $$(go (toText key : path) inner) t
        (# _, (# | (##) #) #) -> Left . missingKey path $ toText key
      ||]

    goCompound path (OptionalKeyCodec key inner _descr) = [|| \slice ->
      case findWithIndexNBT key slice of
        (# _, (# (NBT _ t) | #) #) -> Just <$> $$(go (toText key : path) inner) t
        (# _, (# | (##) #) #) -> Right Nothing
      ||]

findWithIndexNBT :: NBTString -> Slice NBT -> (# Int#, (# NBT | (# #) #) #)
findWithIndexNBT = findWithIndex(\(NBT k _) -> k) 

newtype DecodeError = DecodeError Text
  deriving newtype Show

missingKey :: [Text] -> Text -> DecodeError
missingKey path key = DecodeError $ "Missing key " <> key <> "." <> showPath path

invalidType :: [Text] -> Text -> Maybe Text -> DecodeError
invalidType path ty Nothing = DecodeError $ "Invalid type. Expected " <> ty <> "." <> showPath path
invalidType path ty (Just name) = DecodeError $ "Invalid type for " <> name <> ". Expected " <> ty <> "." <> showPath path

showPath :: [Text] -> Text
showPath [] = " At path <root>"
showPath ys = " At path " <> x <> foldr (\y acc -> "." <> y <> acc) "" xs
  where (x:xs) = reverse ys

-- TH utils

type Code a = TH.Code TH.Q a


