{-# LANGUAGE DerivingStrategies, MagicHash, UnboxedTuples, UnliftedFFITypes #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all -fforce-recomp #-}
module Brokkr.NBT.Internal (
  NBT(..)
, Tag(..)
, parseNBT
, parseTag
, putNBT
, putTag
, takeArray
) where

import Data.ByteString.Internal qualified as BS
import Data.Coerce
import Data.Int
import Data.Primitive
import Data.Vector.Storable qualified as S

import Brokkr.NBT.ByteOrder
import Brokkr.NBT.NBTString
import Brokkr.NBT.Slice

import GHC.Exts
import GHC.Float (castWord32ToFloat,castWord64ToDouble)
import GHC.Word

import FlatParse.Basic qualified as FP

import Foreign.Storable qualified as S

import Mason.Builder qualified as B

-- | Named-Binary-Tag's (NBT)
--
-- Minecrafts generic data format, primarily used for file storage and other dynamic arguments in the protocol.
--
-- The original NBT specification can be found here: https://web.archive.org/web/20110723210920/http://www.minecraft.net/docs/NBT.txt
-- An updated version with documentation can be seen here: https://wiki.vg/NBT
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
  TagList      :: {-# UNPACK #-} !(SmallArray Tag)   -> Tag
  TagCompound  :: {-# UNPACK #-} !(Slice NBT)        -> Tag
  TagIntArray  :: {-# UNPACK #-} !(S.Vector Int32BE) -> Tag
  TagLongArray :: {-# UNPACK #-} !(S.Vector Int64BE) -> Tag
  deriving stock (Eq, Show)

-- Decode

-- | Parses arbitrary 'NBT'
--
-- Strings are 'NBTString' which is encoded in java modified utf8
--
-- Strings and storable vectors are slices into the original input
-- and thus keep the original input alive until they are copied.
--
-- The IntArray and LongArray use 'Int32BE' and 'Int64BE' respectively
-- 'Brokkr.NBT.ByteOrder' contains efficient methods to convert back
-- to 'Int32' and 'Int64'.
--
-- With these constraints the actual parse is (almost) minimal.
-- "Almost", because we store the 'NBT' in compound tags in sorted
-- order so that we can query them quickly.
parseNBT :: FP.ParserT st e NBT
parseNBT = do
  tagId <- FP.anyWord8
  name <- parseNBTString
  tag <- parseTag tagId
  pure $ NBT name tag

-- | Parse a single nbt tag given the tags type
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
parseTag  9 = parseList
  where
    parseList = do
      W8# tagId <- FP.anyWord8
      len@(I# len#) <- fromIntegral <$> FP.anyInt32be
      localST $ do
        SmallMutableArray mut <- FP.liftST $ newSmallArray len (error "parse nbt list")
        go mut 0# len# tagId
        arr <- FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut)
        pure $ TagList arr
    {-# INLINE parseList #-}
    go _   _ 0# _   = pure ()
    go mut i n  tid = do
      el <- parseTag (W8# tid)
      FP.liftST $ writeSmallArray (SmallMutableArray mut) (I# i) el
      go mut (i +# 1#) (n -# 1#) tid

-- compounds
parseTag 10 = parseCompound
  where
    parseCompound = localST $ do
      let initCap = 4#
      SmallMutableArray mut <- FP.liftST $ newSmallArray (I# initCap) (error "parse nbt comp init")
      go mut initCap 0# $ \mut' sz -> do
        (SmallArray arr) <- FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut')
        pure . TagCompound $ Slice arr sz
      where
        go :: SmallMutableArray# s NBT -> Int# -> Int# -> (SmallMutableArray# s NBT -> Int# -> FP.ParserST s e r) -> FP.ParserST s e r
        go mut cap sz f = do
          tagId <- FP.anyWord8
          if tagId == 0
            then f mut sz
            else withNBTString $ \name -> do
              tag <- parseTag tagId
              -- grow
              ensure mut cap sz $ \mut' cap' -> do
                -- insert sorted
                FP.liftST $ insertSorted (SmallMutableArray mut') (I# sz) name tag
                go mut' cap' (sz +# 1#) f
    {-# INLINE parseCompound #-}
    ensure mut cap sz f
      | isTrue# (cap <=# sz) =
        let newCap = cap *# 2#
        in FP.ParserT $ \fp curr end s ->
          case newSmallArray# newCap (error "parse nbt comp grow") s of
            (# s', mut' #) -> case copySmallMutableArray# mut 0# mut' 0# sz s' of
              s'' -> case f mut' newCap of
                FP.ParserT g -> g fp curr end s''
      | otherwise = f mut cap
    {-# INLINE ensure #-}
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

-- | Parse a 32 bit integer prefixed slice of memory as a storable vector
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

-- | Encode 'NBT' back to binary
putNBT :: NBT -> B.Builder
{-# INLINE putNBT #-}
putNBT (NBT key tag) =
     B.word8 (fromIntegral $ getTagId tag)
  <> putNBTString key
  <> putTag tag

-- | Get the tag type for our tag
--
-- Every NBT is prefixed by a single byte describing what constructor the following tag has.
--
-- This is obtained via 'tagId' which internally uses 'dataToTag#' and thus has a few restrictions:
-- - The argument has to be strict. 'tagId' thus needs the bang pattern.
-- - The constructor order defines the tag type id. Thus changes to the Tag datatype need to
--   respect the NBT-specifications order. 
--
-- Also 'dataToTag#' starts at '0', but since we exclude the end tag, we need to increment the
-- result by one.
getTagId :: Tag -> Int
{-# INLINE getTagId #-}
getTagId !tag = (I# (dataToTag# tag)) + 1

-- | Encode a single nbt tag
putTag :: Tag -> B.Builder
{-# INLINE putTag #-}
putTag (TagByte i)   = B.int8 i
putTag (TagShort i)  = B.int16BE i
putTag (TagInt i)    = B.int32BE i
putTag (TagLong i)   = B.int64BE i
putTag (TagFloat i)  = B.floatBE i
putTag (TagDouble i) = B.doubleBE i

putTag (TagByteArray arr) = B.int32BE (fromIntegral sz) <> B.byteString (BS.BS (coerce fp) sz)
  where (fp, sz) = S.unsafeToForeignPtr0 arr

putTag (TagIntArray arr) = B.int32BE (fromIntegral sz) <> B.byteString (BS.BS (coerce fp) (sz * 4))
  where !(fp, sz) = S.unsafeToForeignPtr0 arr
putTag (TagLongArray arr) = B.int32BE (fromIntegral sz) <> B.byteString (BS.BS (coerce fp) (sz * 8))
  where !(fp, sz) = S.unsafeToForeignPtr0 arr

putTag (TagString str) = putNBTString str

putTag (TagList arr)
  | len == 0 = B.int8 0 <> B.int32BE 0
  | otherwise = B.int8 tagId <> B.int32BE (fromIntegral len) <> foldMap (\t -> putTag t) arr
  where
    len = sizeofSmallArray arr
    tagId = fromIntegral . getTagId $ indexSmallArray arr 0

putTag (TagCompound arr) = foldMap (\nbt -> putNBT nbt) arr <> B.int8 0
