{-# LANGUAGE DerivingStrategies, MagicHash, UnboxedTuples, UnliftedFFITypes, LambdaCase, DeriveAnyClass #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all -fforce-recomp -ddump-cmm #-}
module Brokkr.NBT.Internal (
  NBT(..)
, Tag(..)
, parseNBT
, parseTag
, putNBT
, putTag
, takeArray
, withArray
, getTagId
) where

import Data.ByteString.Internal qualified as BS
import Data.Coerce
import Data.Int
import Data.Primitive
import Data.Vector.Storable qualified as S

import Brokkr.NBT.ByteOrder
import Brokkr.NBT.NBTString
import Brokkr.NBT.Slice
import Brokkr.NBT.NBTError

import Control.DeepSeq

import GHC.Exts
import GHC.Float
import GHC.Word
import GHC.Generics (Generic)

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
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

-- Note: Parsing performance
--
-- Lets first put up some requirements for the parser:
--
-- 1. Accept any valid NBT
-- 2. Reject all invalid NBT
-- 3. Provide fast key/value access in compounds
--
-- 1,2 specifically force handling modified utf8 and nbt of all sizes.
-- 3 is a requirement to have a *useful* result. NBT is generally queried or transformed. Both of which
--  require key/value access and whats the point of a fast parse if kv access is slow.
--
-- Performance ideas:
-- - Unlifted NBT. This avoids a pointer tag branch in the sorted insert loop.

-- | The "Tag" portion of Named-Binary-Tag (NBT)
-- 
-- Note: End tags are never parsed. This would be pointless and only clutter the datatype needlessly
--
-- Unless otherwise noted, numbers will be converted to host byte order. Except for arrays where the
-- newtypes 'Int32BE' and 'Int64BE' signify that the array is kept in big-endian order.
-- See 'Brokkr.NBT.ByteOrder' for easy and fast methods to convert these arrays.
--
-- Important: The constructor order matters. See 'getTagId'
data Tag where
  TagByte      :: {-# UNPACK #-} !Int8                         -> Tag
  TagShort     :: {-# UNPACK #-} !Int16                        -> Tag
  TagInt       :: {-# UNPACK #-} !Int32                        -> Tag
  TagLong      :: {-# UNPACK #-} !Int64                        -> Tag
  TagFloat     :: {-# UNPACK #-} !Float                        -> Tag
  TagDouble    :: {-# UNPACK #-} !Double                       -> Tag
  TagByteArray :: {-# UNPACK #-} !(S.Vector Int8)              -> Tag
  TagString    :: {-# UNPACK #-} !NBTString                    -> Tag
  -- It turns out that using a special list type for each kind is a waste
  TagList      :: {-# UNPACK #-} !(SmallArray Tag)             -> Tag -- TODO This is not parsing critical, but still is better strict
  TagCompound  :: {-# UNPACK #-} !(Slice NBT)                  -> Tag -- TODO Sorting requires access to the key, so this is a branch on the tag!
  TagIntArray  :: {-# UNPACK #-} !(S.Vector (BigEndian Int32)) -> Tag
  TagLongArray :: {-# UNPACK #-} !(S.Vector (BigEndian Int64)) -> Tag
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

-- Decode

-- | Parse arbitrary 'NBT'
--
-- Strings are 'NBTString' which is encoded in java modified utf8
--
-- Strings and storable vectors are slices into the original input
-- and thus keep the original input alive until they are copied.
--
-- The IntArray and LongArray use 'Int32BE' and 'Int64BE' respectively.
-- 'Brokkr.NBT.ByteOrder' contains efficient methods to convert back
-- to 'Int32' and 'Int64'.
--
-- With these constraints the actual parse is (almost) minimal.
-- "Almost", because we store the 'NBT' in compound tags in sorted
-- order so that we can query them quickly.
parseNBT :: FP.ParserT st NBTError NBT
parseNBT = do
  tagId <- FP.anyWord8
  name <- parseNBTString
  tag <- parseTag tagId
  pure $ NBT name tag

-- | Parse a single nbt tag given a tag type
parseTag :: Word8 -> FP.ParserT st NBTError Tag
{-# INLINE parseTag #-}
-- Primitive types
parseTag  1 = TagByte  <$> FP.anyInt8
parseTag  2 = TagShort <$> FP.anyInt16be
parseTag  3 = TagInt   <$> FP.anyInt32be
parseTag  4 = TagLong  <$> FP.anyInt64be
-- Floating point
parseTag  5 = TagFloat  . castWord32ToFloat  <$> FP.anyWord32be
parseTag  6 = TagDouble . castWord64ToDouble <$> FP.anyWord64be
-- Primitive array types
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
        -- TODO Check against stupidly large lists
        SmallMutableArray mut <- FP.liftST $ newSmallArray len $ error "parse nbt list"
        go mut 0# len# tagId
        arr <- FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut)
        pure $ TagList arr
    {-# INLINE parseList #-}
    go _   _ 0# _   = pure ()
    go mut i n  tid = do
      !el <- parseTag (W8# tid)
      FP.liftST $ writeSmallArray (SmallMutableArray mut) (I# i) el
      go mut (i +# 1#) (n -# 1#) tid

-- compounds
parseTag 10 = parseCompound
  where
    parseCompound :: FP.ParserT st NBTError Tag
    {-# INLINE parseCompound #-}
    parseCompound = localST $ do
      let initCap = 4#
      SmallMutableArray mut <- FP.liftST $ newSmallArray (I# initCap) (error "parse nbt comp init")
      go mut initCap 0#
      where
        go :: SmallMutableArray# s NBT -> Int# -> Int# -> FP.ParserST s NBTError Tag
        go mut cap sz = do
          tagId <- FP.anyWord8
          if tagId == 0
            then do
              (SmallArray arr) <- FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut)
              pure $ TagCompound $ Slice arr sz
            else withNBTString $ \name -> do
              tag <- parseTag tagId
              -- grow
              ensure mut cap sz $ \mut' cap' -> do
                -- insert sorted
                FP.liftST $ insertSorted (SmallMutableArray mut') (I# sz) name tag
                go mut' cap' (sz +# 1#)
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
        -- TODO Try sorting after inserting
        -- TODO Try branchless version
        insertSorted !mut !sz !name !tag = go_insert 0 sz
          where
            go_insert l u
              | l >= u = do
                copySmallMutableArray mut (u + 1) mut u (sz - u)
                writeSmallArray mut u $! NBT name tag
              | otherwise = do
                let mid = (u + l) `quot` 2
                NBT k' _ <- readSmallArray mut mid
                case compare name k' of
                  EQ -> pure ()
                  LT -> go_insert l mid
                  GT -> go_insert (mid + 1) u
        -- insertSorted !mut !sz !name !tag = go 0
        --   where
        --     go !n
        --       | n >= sz   = writeSmallArray mut n $! NBT name tag
        --       | otherwise = do
        --         NBT k' _ <- readSmallArray mut n
        --         case compare name k' of
        --           EQ -> pure ()
        --           GT -> go (n + 1)
        --           LT -> do
        --             copySmallMutableArray mut (n + 1) mut n (sz - n)
        --             writeSmallArray mut n $! NBT name tag
-- anything else fails
-- Note: We don't parse TagEnd, parseCompound and parseList
-- parse it separately, so a TagEnd here would be invalid nbt
parseTag _ = FP.empty
-- Throwing this error causes a 2x slowdown. TODO Investigate why
-- parseTag t = FP.err $ InvalidTagType (fromIntegral t)

-- | Parse a 32 bit integer prefixed slice of memory as a storable vector
takeArray :: forall a e st . S.Storable a => FP.ParserT st e (S.Vector a)
{-# INLINE takeArray #-}
takeArray = FP.withAnyWord32 $ \w -> do
  -- TODO Define this somewhere with cpp so that we
  -- skip this on big endian systems
  let !len = fromIntegral $ byteSwap32 w 
      !sz = S.sizeOf (undefined :: a)
  BS.BS fp _ <- FP.take (len * sz)
  pure $ S.unsafeFromForeignPtr0 (coerce fp) len

withArray :: forall a e st r . S.Storable a => (S.Vector a -> FP.ParserT st e r) -> FP.ParserT st e r
{-# INLINE withArray #-}
withArray f = FP.withAnyWord32 $ \w -> do
  -- TODO Define this somewhere with cpp so that we
  -- skip this on big endian systems
  let !len = fromIntegral $ byteSwap32 w 
      !sz = S.sizeOf (undefined :: a)
  BS.BS fp _ <- FP.take (len * sz)
  f $ S.unsafeFromForeignPtr0 (coerce fp) len

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
getTagId !tag = 1 + I# (dataToTag# tag)

-- | Encode a single nbt tag
putTag :: Tag -> B.Builder
{-# INLINE putTag #-}
putTag (TagByte i)   = B.int8 i
-- TODO Investigate why ghc performs 2,4,8 bytewise writes instead of a larger write all at once with byteswap
-- I think it no longer does on later versions (9.4+) which explains the large speedup
putTag (TagShort i)  = B.int16BE i
putTag (TagInt i)    = B.int32BE i
putTag (TagLong i)   = B.int64BE i
putTag (TagFloat i)  = B.floatBE i
putTag (TagDouble i) = B.doubleBE i

putTag (TagByteArray arr) = putArr arr
putTag (TagIntArray arr) = putArr arr
putTag (TagLongArray arr) = putArr arr 

putTag (TagString str) = putNBTString str

putTag (TagList arr)
  | sz == 0   = B.int8 0 <> B.int32BE 0
  | otherwise = B.int8 (fromIntegral $ getTagId t1) <> B.int32BE (fromIntegral sz) <> foldMap (\x -> putTag x) arr
  where
    sz = sizeofSmallArray arr
    t1 = indexSmallArray arr 0

putTag (TagCompound arr) = foldMap (\nbt -> putNBT nbt) arr <> B.int8 0

putArr :: forall a . S.Storable a => S.Vector a -> B.Builder
{-# INLINE putArr #-}
putArr v = B.int32BE (fromIntegral sz) <> B.byteString (BS.BS (coerce fp) (sz * szEl))
  where
    szEl = S.sizeOf (undefined :: a)
    !(fp, sz) = S.unsafeToForeignPtr0 v
