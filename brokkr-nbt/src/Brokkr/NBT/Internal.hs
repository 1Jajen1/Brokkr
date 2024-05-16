{-# LANGUAGE DerivingStrategies, MagicHash, UnboxedTuples, UnliftedFFITypes, LambdaCase, DeriveAnyClass #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all -fforce-recomp -ddump-cmm #-}
module Brokkr.NBT.Internal (
  NBT(..)
, Compound(..)
, MutCompound(..)
, newCompound
, sizeOfCompound
, foldCompoundSorted
, findWithIndex
, compoundFromList, compoundFromListAscending
, ensureCompound, unsafeFreezeCompound
, writeToCompound, moveCompound
, readCompoundKey
, KeyForeignPtrContents(..)
, Tag(..)
, parseNBT
, parseTag
, putNBT
, putTag
, takeArray
, withArray
, getTagId
, localST
) where

import Data.ByteString.Internal qualified as BS
import Data.Coerce
import Data.Int
import Data.Foldable
import Data.List (sortOn)
import Data.Primitive
import Data.Vector.Storable qualified as S

import Brokkr.NBT.ByteOrder
import Brokkr.NBT.NBTString
import Brokkr.NBT.NBTError
import Brokkr.NBT.NBTString.Internal -- TODO

import Control.DeepSeq
import Control.Monad.Primitive
import Control.Monad.ST.Strict

import GHC.Exts
import GHC.ForeignPtr
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
  TagCompound  :: {-# UNPACK #-} !Compound                     -> Tag
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
      fpco <- FP.ParserT $ \fpco _ curr st -> FP.OK# st fpco curr
      mut <- FP.liftST $ newCompound 8
      go fpco mut 0
      where
        go :: ForeignPtrContents -> MutCompound s -> Int -> FP.ParserST s NBTError Tag
        go !fpco !mut !sz = do
          tagId <- FP.anyWord8
          if tagId == 0
            then do
              comp <- FP.liftST $ unsafeFreezeCompound mut sz $ OneKeyFP fpco
              pure $ TagCompound comp
            else withNBTString $ \name -> do
              tag <- parseTag tagId
              -- grow
              ensureCompound mut sz $ \mut' -> do
                sz' <- FP.liftST $ insertSortedLinear fpco mut' sz name tag
                go fpco mut' sz'
        -- insertUnsorted _ !mut !sz !name !tag = writeToCompound mut sz name tag sz >> pure (sz + 1)
        -- TODO Try sorting after inserting
        -- TODO Try branchless versions
        -- insertSortedHybrid :: PrimMonad m => ForeignPtrContents -> MutCompound (PrimState m) -> Int -> NBTString -> Tag -> m ()
        -- {-# INLINE insertSortedHybrid #-}
        -- insertSortedHybrid !fpco !mut !sz !name !tag
        --   | sz < 64   = insertSortedLinear fpco mut sz name tag
        --   | otherwise = insertSortedBinary fpco mut sz name tag
        -- insertSortedBinary :: PrimMonad m => ForeignPtrContents -> MutCompound (PrimState m) -> Int -> NBTString -> Tag -> m ()
        -- {-# INLINE insertSortedBinary #-}
        -- insertSortedBinary !fpco !mut !sz !name !tag = go_insert 0 sz
        --   where
        --     go_insert l u
        --       | l >= u = do
        --         moveCompound mut u sz
        --         writeToCompound mut sz name tag u
        --       | otherwise = do
        --         let mid = (u + l) `quot` 2
        --         k' <- readCompoundKey mut mid fpco
        --         case compare name k' of
        --           EQ -> pure ()
        --           LT -> go_insert l mid
        --           GT -> go_insert (mid + 1) u
        insertSortedLinear :: PrimMonad m => ForeignPtrContents -> MutCompound (PrimState m) -> Int -> NBTString -> Tag -> m Int
        {-# INLINE insertSortedLinear #-}
        insertSortedLinear !fpco !mut !sz !name !tag = go_insert 0
          where
            go_insert !n
              | n >= sz   = writeToCompound mut sz name tag sz >> pure (sz + 1)
              | otherwise = do
                k' <- readCompoundKey mut n fpco
                case compare name k' of
                  EQ -> pure sz
                  GT -> go_insert (n + 1)
                  LT -> do
                    moveCompound mut n sz
                    writeToCompound mut sz name tag n
                    pure (sz + 1)

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

putTag (TagCompound arr) = foldCompound (\nbt -> putNBT nbt) arr <> B.int8 0

putArr :: forall a . S.Storable a => S.Vector a -> B.Builder
{-# INLINE putArr #-}
putArr v = B.int32BE (fromIntegral sz) <> B.byteString (BS.BS (coerce fp) (sz * szEl))
  where
    szEl = S.sizeOf (undefined :: a)
    !(fp, sz) = S.unsafeToForeignPtr0 v

data Compound = Compound
  {-# UNPACK #-} !Int               -- Number of elements
  {-# UNPACK #-} !KeyForeignPtrContents
  {-# UNPACK #-} !(PrimArray Int)   -- Keys. Stored as Int# + Addr# for size and ptr to data
  {-# UNPACK #-} !(SmallArray Tag)  -- Actual tags
  {-# UNPACK #-} !(PrimArray Int32) -- Sorting
  deriving stock Generic

data KeyForeignPtrContents
  = OneKeyFP !ForeignPtrContents
  | ManyKeyFP {-# UNPACK #-} !(SmallArray ForeignPtrContents)

instance Eq Compound where
  l == r = foldCompoundSorted (pure @[]) l == foldCompoundSorted pure r

instance NFData Compound where
  {-# INLINE rnf #-}
  rnf !_ = () -- enough because everything inside is strict. Even the tags are with the writes from this module

instance Show Compound where
  show c = "Compound " <> show (foldCompoundSorted (pure @[]) c) 

data MutCompound s = MutCompound
  {-# UNPACK #-} !(MutablePrimArray s Int)
  {-# UNPACK #-} !(SmallMutableArray s Tag)
  {-# UNPACK #-} !(MutablePrimArray s Int32)

-- TODO Yeah this one sucks...
compoundFromList :: [NBT] -> Compound
{-# INLINE compoundFromList #-}
compoundFromList = compoundFromListAscending . sortOn (\(NBT k _) -> k)

compoundFromListAscending :: [NBT] -> Compound
{-# INLINE compoundFromListAscending #-}
compoundFromListAscending xs0 = runST $ do
  mut <- newCompound len
  mutFpcos <- newSmallArray len (error "compoundFromList")
  forM_ (zip [0..] xs0) $ \(ind, NBT name@(NBTString (ModifiedUtf8 (BS.BS (ForeignPtr _ fpco) _))) tag) -> do
    writeSmallArray mutFpcos ind fpco
    writeToCompound mut ind name tag ind
  fpcos <- unsafeFreezeSmallArray mutFpcos
  unsafeFreezeCompound mut len $ ManyKeyFP fpcos
  where
    len = length xs0

newCompound :: PrimMonad m => Int -> m (MutCompound (PrimState m))
{-# INLINE newCompound #-}
newCompound sz = MutCompound <$> newPrimArray (sz * 2) <*> newSmallArray sz (error "newCompound::empty") <*> newPrimArray sz

readCompoundKey :: PrimMonad m => MutCompound (PrimState m) -> Int -> ForeignPtrContents -> m NBTString
{-# INLINE readCompoundKey #-}
readCompoundKey (MutCompound keys _ sorted) i0 fpco = do
  i <- fromIntegral <$> readPrimArray sorted i0
  sz <- readPrimArray keys $ i * 2
  I# arr <- readPrimArray keys $ i * 2 + 1
  pure . NBTString . ModifiedUtf8 $ BS.BS (ForeignPtr (int2Addr# arr) fpco) sz

writeToCompound :: PrimMonad m => MutCompound (PrimState m) -> Int -> NBTString -> Tag -> Int -> m ()
{-# INLINE writeToCompound #-}
writeToCompound (MutCompound keys tags sorted) ind (NBTString (ModifiedUtf8 (BS.BS (ForeignPtr addr _) sz))) !tag !sortInd = do
  writePrimArray keys (ind * 2    ) sz
  writePrimArray keys (ind * 2 + 1) $ I# (addr2Int# addr)
  writeSmallArray tags ind tag
  writePrimArray sorted sortInd $ fromIntegral ind

ensureCompound :: MutCompound s -> Int -> (MutCompound s -> FP.ParserST s e r) -> FP.ParserST s e r
{-# INLINE ensureCompound #-}
ensureCompound old@(MutCompound keys (SmallMutableArray arr) sorted) sz f = do
  cap <- FP.liftST . primitive $ \s -> case getSizeofSmallMutableArray# arr s of (# s', i #) -> (# s', I# i #)
  if cap > sz then f old else do
    let newCap = cap * 2

    keys' <- FP.liftST $ newPrimArray $ newCap * 2
    FP.liftST $ copyMutablePrimArray keys' 0 keys 0 $ cap * 2

    arr' <- FP.liftST $ newSmallArray newCap (error "ensureCompound::empty")
    FP.liftST $ copySmallMutableArray arr' 0 (SmallMutableArray arr) 0 cap

    sorted' <- FP.liftST $ newPrimArray newCap
    FP.liftST $ copyMutablePrimArray sorted' 0 sorted 0 cap

    f $ MutCompound keys' arr' sorted'

moveCompound :: PrimMonad m => MutCompound (PrimState m) -> Int -> Int -> m ()
{-# INLINE moveCompound #-}
moveCompound (MutCompound _ _ sorted) from sz = copyMutablePrimArray sorted (from + 1) sorted from (sz - from)

unsafeFreezeCompound :: PrimMonad m => MutCompound (PrimState m) -> Int -> KeyForeignPtrContents -> m Compound
{-# INLINE unsafeFreezeCompound #-}
unsafeFreezeCompound (MutCompound keys tags sorted) sz fpcos =
  Compound sz fpcos <$> unsafeFreezePrimArray keys <*> unsafeFreezeSmallArray tags <*> unsafeFreezePrimArray sorted

foldCompound :: Monoid m => (NBT -> m) -> Compound -> m
{-# INLINE foldCompound #-}
foldCompound f (Compound sz fpcos keys tags _) = go 0
  where
    go n
      | n >= sz   = mempty
      | otherwise =
        let keySz = indexPrimArray keys $ n * 2
            !fpco = case fpcos of OneKeyFP x -> x; ManyKeyFP arr -> indexSmallArray arr n
            !(I# addr) = indexPrimArray keys $ n * 2 + 1
            tag = indexSmallArray tags n
        in f (NBT (NBTString (ModifiedUtf8 (BS.BS (ForeignPtr (int2Addr# addr) fpco) keySz))) tag) <> go (n + 1)

foldCompoundSorted :: Monoid m => (NBT -> m) -> Compound -> m
{-# INLINE foldCompoundSorted #-}
foldCompoundSorted f (Compound sz fpcos keys tags sorted) = go 0
  where
    go n
      | n >= sz   = mempty
      | otherwise =
        let i = fromIntegral $ indexPrimArray sorted n
            !fpco = case fpcos of OneKeyFP x -> x; ManyKeyFP arr -> indexSmallArray arr i
            keySz = indexPrimArray keys $ i * 2
            !(I# addr) = indexPrimArray keys $ i * 2 + 1
            tag = indexSmallArray tags i
        in f (NBT (NBTString (ModifiedUtf8 (BS.BS (ForeignPtr (int2Addr# addr) fpco) keySz))) tag) <> go (n + 1)

sizeOfCompound :: Compound -> Int
{-# INLINE sizeOfCompound #-}
sizeOfCompound (Compound _ _ _ tags _) = sizeofSmallArray tags

findWithIndex :: Compound -> NBTString -> (# NBT | (# #) #)
{-# INLINE findWithIndex #-}
findWithIndex (Compound sz fpcos keys tags sorted) key = goFindBinary 0 sz
  where
    goFindBinary l u
      | l >= u = (# | (# #) #)
      | otherwise =
        let mid = (l + u) `quot` 2
            i = fromIntegral $ indexPrimArray sorted mid
            !fpco = case fpcos of OneKeyFP x -> x; ManyKeyFP arr -> indexSmallArray arr i
            keySz = indexPrimArray keys $ i * 2
            !(I# keyAddr) = indexPrimArray keys $ i * 2 + 1
            key' = NBTString (ModifiedUtf8 (BS.BS (ForeignPtr (int2Addr# keyAddr) fpco) keySz))
        in case compare key' key of
          LT -> goFindBinary (mid + 1) u
          GT -> goFindBinary l mid
          EQ -> let tag = indexSmallArray tags i in (# NBT key tag | #)
