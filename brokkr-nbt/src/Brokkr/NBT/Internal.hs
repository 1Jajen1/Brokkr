{-# LANGUAGE DerivingStrategies, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
module Brokkr.NBT.Internal (
  NBT(..)
, Tag(..)
, parseNBT
, parseTag
, putNBT
, putTag
) where

import Data.ByteString.Internal qualified as BS
import Data.Coerce
import Data.Int
import Data.Primitive
import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S
import Data.Word

import Brokkr.NBT.ByteOrder
import Brokkr.NBT.NBTString
import Brokkr.NBT.Slice

import GHC.Exts (runRW#, dataToTag#, Int(I#))
import GHC.Float (castWord32ToFloat,castWord64ToDouble)

import FlatParse.Basic qualified as FP

import Foreign.Storable qualified as S

import Mason.Builder qualified as B

data NBT = NBT {-# UNPACK #-} !NBTString !Tag
  deriving stock (Eq, Show)

data Tag where
  TagByte      :: {-# UNPACK #-} !Int8               -> Tag
  TagShort     :: {-# UNPACK #-} !Int16              -> Tag
  TagInt       :: {-# UNPACK #-} !Int32              -> Tag
  TagLong      :: {-# UNPACK #-} !Int64              -> Tag
  TagFloat     :: {-# UNPACK #-} !Float              -> Tag
  TagDouble    :: {-# UNPACK #-} !Double             -> Tag
  TagByteArray :: {-# UNPACK #-} !(S.Vector Int8)    -> Tag
  TagString    :: {-# UNPACK #-} !NBTString          -> Tag
  TagList      :: {-# UNPACK #-} !(V.Vector Tag)     -> Tag
  TagCompound  :: {-# UNPACK #-} !(Slice NBT)        -> Tag
  TagIntArray  :: {-# UNPACK #-} !(S.Vector Int32BE) -> Tag
  TagLongArray :: {-# UNPACK #-} !(S.Vector Int64BE) -> Tag
  deriving stock (Eq, Show)

-- Decode

parseNBT :: FP.ParserT st e NBT
parseNBT = do
  tagId <- FP.anyWord8
  name <- parseNBTString
  tag <- parseTag tagId
  pure $ NBT name tag

parseTag :: Word8 -> FP.ParserT st e Tag
{-# INLINE parseTag #-}
-- Primitive types
parseTag  1 = TagByte  <$> FP.anyInt8
parseTag  2 = TagShort <$> FP.anyInt16be
parseTag  3 = TagInt   <$> FP.anyInt32be
parseTag  4 = TagLong  <$> FP.anyInt64be
-- Floating point
parseTag  5 = TagFloat  . castWord32ToFloat  <$> FP.anyWord32be
parseTag  6 = TagDouble . castWord64ToDouble <$> FP.anyWord64be
-- Primtive array types
parseTag  7 = TagByteArray <$> takeArray
parseTag 11 = TagIntArray  <$> takeArray
parseTag 12 = TagLongArray <$> takeArray
-- Strings
parseTag  8 = TagString    <$> parseNBTString
-- lists
parseTag  9 = TagList      <$> parseList
  where
    parseList = do
      tagId <- FP.anyWord8
      len   <- FP.anyInt32be
      V.replicateM (fromIntegral len) (parseTag tagId >>= seq <*> pure)
    {-# INLINE parseList #-}
-- compounds
parseTag 10 = TagCompound  <$> parseCompound
  where
    parseCompound = localST $ do
      let initCap = 4
      !mut <- FP.liftST $ newSmallArray initCap (error "parse nbt comp init")
      !(mut', I# sz) <- go mut initCap 0
      !(SmallArray arr) <- FP.liftST $ unsafeFreezeSmallArray mut'
      pure $ Slice arr sz
      where
        go !mut !cap !sz = do
          tagId <- FP.anyWord8
          if tagId == 0
            then pure (mut,sz)
            else do
              name <- parseNBTString
              tag  <- parseTag tagId
              -- grow
              (mut', cap') <- if cap <= sz
                then FP.liftST $ do
                  let newCap = cap * 2
                  mut' <- newSmallArray newCap (error "parse nbt comp grow")
                  copySmallMutableArray mut' 0 mut 0 sz
                  pure (mut', newCap)
                else pure (mut, cap)
              -- insert sorted
              FP.liftST $ insertSorted mut' sz name tag
              go mut' cap' (sz + 1)
    {-# INLINE parseCompound #-}
    insertSorted !mut !sz !name !tag = go 0
      where
        -- TODO Benchmark against binary search. 
        go !n
          | n == sz   = writeSmallArray mut n $! NBT name tag
          | otherwise = do
            NBT k' _ <- readSmallArray mut n
            case compare name k' of
              EQ -> pure ()
              GT -> go (n + 1)
              LT -> do
                copySmallMutableArray mut (n + 1) mut n (sz - n)
                writeSmallArray mut n $! NBT name tag
    {-# INLINE insertSorted #-}
-- anything else fails
-- Note: We don't parse TagEnd, parseCompound and parseList
-- parse it seperately, so a TagEnd here would be invalid nbt
parseTag _  = FP.empty

takeArray :: forall st e a . S.Storable a => FP.ParserT st e (S.Vector a)
{-# INLINE takeArray #-}
takeArray = do
  len <- fromIntegral <$> FP.anyInt32be
  let sz = S.sizeOf (undefined :: a)
  BS.BS fp _ <- FP.take $ len * sz
  pure $ S.unsafeFromForeignPtr0 (coerce fp) len

-- TODO PR to flatparse
localST :: forall st e a . (forall s . FP.ParserST s e a) -> FP.ParserT st e a
{-# INLINE localST #-}
localST p = FP.ParserT $ \fp end curr st -> case runRW# (FP.runParserT# p fp end curr) of
  (# _, res #) -> (# st, res #)

-- Encode

putNBT :: NBT -> B.Builder
{-# INLINE putNBT #-}
putNBT (NBT key tag) =
     B.word8 (fromIntegral $ getTagId tag)
  <> putNBTString key
  <> putTag tag

-- Our datatype mimics NBT exactly, except we skip the end tag as it is never useful
-- to actually parse it. To offset dataToTag# starting at 0, we simply add 1
getTagId :: Tag -> Int
{-# INLINE getTagId #-}
getTagId !tag = (I# (dataToTag# tag)) + 1

putTag :: Tag -> B.Builder
{-# INLINE putTag #-}
putTag (TagByte i)   = B.int8 i
putTag (TagShort i)  = B.int16BE i
putTag (TagInt i)    = B.int32BE i
putTag (TagLong i)   = B.int64BE i
putTag (TagFloat i)  = B.floatBE i
putTag (TagDouble i) = B.doubleBE i

putTag (TagByteArray arr) = B.int32BE (fromIntegral sz) <> (B.byteString $ BS.BS (coerce fp) sz)
  where (fp, sz) = S.unsafeToForeignPtr0 arr

putTag (TagIntArray arr) = B.int32BE (fromIntegral sz) <> (B.byteString $ BS.BS (coerce fp) (sz * 4))
  where !(fp, sz) = S.unsafeToForeignPtr0 arr
putTag (TagLongArray arr) = B.int32BE (fromIntegral sz) <> (B.byteString $ BS.BS (coerce fp) (sz * 8))
  where !(fp, sz) = S.unsafeToForeignPtr0 arr

putTag (TagString str) = putNBTString str

putTag (TagList arr)
  | V.null arr = B.int8 0 <> B.int32BE 0
  | otherwise  =
    let tagId = fromIntegral . getTagId $ V.head arr
    in B.int8 tagId <> B.int32BE (fromIntegral $ V.length arr) <> V.foldMap (\t -> putTag t) arr 

putTag (TagCompound arr) = foldMap (\nbt -> putNBT nbt) arr <> B.int8 0
