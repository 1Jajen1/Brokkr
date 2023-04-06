{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash, UnboxedTuples #-}
module IO.Chunk (
  Chunk(..)
, ChunkSection(..)
, Biomes(..)
, BlockStates(..)
, LightVector(..)
, countBlocks
, numSections
) where

import Brokkr.NBT
import Brokkr.NBT.Internal (NBT(..), Tag(..))
import Brokkr.NBT.NBTString.Internal (NBTString(..))
import Brokkr.NBT.Slice (Slice)
import Brokkr.NBT.Slice qualified as Slice

import Brokkr.Registry.Biome

-- TODO
import Brokkr.BlockState.Internal.BlockState
-- TODO
import Brokkr.BlockState.Internal.Conversion

import Chunk.Heightmap
import Chunk.Position

import Control.DeepSeq

import Data.Coerce
import Data.Foldable
import Data.Int
import Data.List (sortOn)

import qualified Data.ByteString as BS

import qualified Data.HashMap.Strict as HM

import Data.Maybe (fromMaybe)

import Data.Primitive

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Data.Word (Word64)

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Primitive as Prim
import qualified Data.Vector.Primitive.Mutable as PrimM

import Foreign.ForeignPtr (ForeignPtr)

import GHC.Generics
import GHC.TypeLits

import Util.Binary
import Util.NBT          qualified as OldNBT
import Util.NBT.Internal qualified as OldNBT

import Util.Vector.Packed (PackedVector, DynamicNat(..))
import qualified Util.Vector.Packed as P

import Util.PalettedVector

import GHC.Exts (Int#)

-- Parsed snapshot of a chunk on disk
-- Used to serialize and deserialize chunks to disk
data Chunk = Chunk {
  position       :: {-# UNPACK #-} !ChunkPosition
, lowestYSection :: {-# UNPACK #-} !Int
, sections       :: {-# UNPACK #-} !(V.Vector ChunkSection)
, heightmaps     :: {-# UNPACK #-} !Heightmaps
}
  deriving stock (Show, Generic)
  deriving anyclass NFData
  -- deriving (FromBinary) via OldNBT.BinaryNBT Chunk

data ChunkSection = ChunkSection {
  y          :: {-# UNPACK #-} !Int
, blockLight ::                !(Maybe LightVector)
, skyLight   ::                !(Maybe LightVector)
, blocks     ::                !BlockStates
, biomes     ::                !Biomes
, blockCount :: {-# UNPACK #-} !Int -- TODO Move block counting later on
}
  deriving stock (Eq, Show, Generic)
  deriving anyclass NFData

type BlockPaletteMaxBitsize = 1 + Log2 (HighestBlockStateId - 1) 
type BiomePaletteMaxBitsze  = 6

type SectionSize = 4096
type BiomeSectionSize = 64

newtype LightVector = LightVector (PackedVector ('Static 4096) ('Static 4))
  deriving stock Show
  deriving newtype (Eq, NFData, ToBinary)

newtype BlockStates = BlockStates (PalettedVector SectionSize BlockPaletteMaxBitsize)
  deriving stock Show
  deriving newtype (Eq, NFData, ToBinary)

newtype Biomes = Biomes (PalettedVector BiomeSectionSize BiomePaletteMaxBitsze)
  deriving stock Show
  deriving newtype (Eq, NFData, ToBinary)

--
instance HasCodec Chunk where
  codec = compound "chunk" $ [|| Chunk ||]
    <$#> chunkPosition              .= [|| position ||]
    <*#> requiredFieldVia @(ViaSizedInt Int32) "yPos"
                                    .= [|| lowestYSection ||]
    <*#> chunkSections              .= [|| sections ||]
    <*#> requiredField "Heightmaps" .= [|| heightmaps ||]
    where
      chunkPosition = [|| ChunkPos ||]
        <$#> requiredFieldVia @(ViaSizedInt Int32) "xPos" .= [|| \(ChunkPos x _) -> x ||]
        <*#> requiredFieldVia @(ViaSizedInt Int32) "zPos" .= [|| \(ChunkPos _ z) -> z ||]
      -- Sorting the list here has little to no impact whatsoever. TODO Measure again
      -- TODO Handle all y sections correctly...
      chunkSections = [|| \(xs :: SmallArray ChunkSection) -> V.fromListN (sizeofSmallArray xs) . sortOn y . filter (\ChunkSection{y} -> y >= -4) $ toList xs ||]
        <$#> requiredField "sections" .= [|| \xs -> smallArrayFromListN (V.length xs) $ toList xs ||]

instance OldNBT.FromNBT Chunk where
  parseNBT = OldNBT.withCompound $ \obj -> do
    !xPos <- fromIntegral @Int32 <$> obj OldNBT..: "xPos"
    !zPos <- fromIntegral @Int32 <$> obj OldNBT..: "zPos"
    let !position = ChunkPos xPos zPos

    !lowestYSection <- fromIntegral @Int32 <$> obj OldNBT..: "yPos"

    -- TODO This may contain gaps (can it?)
    !sectionsUnordered <- obj OldNBT..: "sections"
    let !sections = V.fromListN (V.length sectionsUnordered) . sortOn y . filter (\ChunkSection{y} -> y >= -4) $ V.toList sectionsUnordered

    !heightmaps <- obj OldNBT..: "Heightmaps"

    pure Chunk{..}
  {-# INLINE parseNBT #-}
  {-# SCC parseNBT #-}

instance HasCodec ChunkSection where
  codec = compound "chunk section" $ [|| \mblocks y blockLight skyLight mbiomes ->
      let blocks = fromMaybe (BlockStates $ SingleValue airId) mblocks
          biomes = fromMaybe (Biomes . SingleValue $ 0) mbiomes -- "TODO"
      in ChunkSection y blockLight skyLight blocks biomes $ countBlocks blocks
      ||]
    <$#> optionalField "block_states" .= [|| Just . blocks ||] -- TODO Writing optional
    <*#> requiredFieldVia @(ViaSizedInt Int8) "Y"
                                      .= [|| y ||]
    <*#> optionalField "BlockLight"   .= [|| blockLight ||]
    <*#> optionalField "SkyLight"     .= [|| skyLight ||]
    <*#> optionalField "biomes"       .= [|| Just . biomes ||]
    where airId = coerce Air -- TODO Hack to satisfy TH generated code

instance HasCodec LightVector where
  codec = viaCodec $ dimapCodec
    [|| LightVector . P.unsafeStaticFromForeignPtr . coerce @(ForeignPtr Int8) @(ForeignPtr Word64) . fst . S.unsafeToForeignPtr0 @Int8 ||]
    [|| error "TODO" ||] -- TODO Encoding side of this. Probably after switching to my new packed vector
    codec

instance OldNBT.FromNBT ChunkSection where
  parseNBT = OldNBT.withCompound $ \obj -> do
    !y <- fromIntegral @Int8 <$> obj OldNBT..: "Y"

    !blockLight <- fmap LightVector <$> obj OldNBT..:? "BlockLight" -- TODO Make sure the Maybe is fully forced
    !skyLight <- fmap LightVector <$> obj OldNBT..:? "SkyLight"

    !blocks <- obj OldNBT..: "block_states"

    let !blockCount = countBlocks blocks

    !biomes <- obj OldNBT..: "biomes"
    pure ChunkSection{..}
  {-# INLINE parseNBT #-}
  {-# SCC parseNBT #-}

instance HasCodec BlockStates where
  -- This is ~60% of parsing a chunk
  codec = compound "block states" $ [|| \palette mblocks ->
    if Prim.length palette == 1
      then BlockStates . SingleValue . coerce $ Prim.unsafeHead palette
      else
        let (blockStatesFptr, numEntries) = S.unsafeToForeignPtr0 @Int64BE $ fromMaybe (error "TODO") mblocks -- TODO error
            -- numEntries = bitsPerVal * 4096 / 64 => numEntries = bitsPerValue * 64 => numEntries / 64 = bitsPerValue
            bitsPerVal = numEntries `div` 64
        in if | bitsPerVal < 4 -> error "TODO" -- Errors
              | bitsPerVal < 9 ->
                BlockStates . Indirect palette . P.unsafeDynamicFromForeignPtr bitsPerVal $ coerce blockStatesFptr
              | otherwise -> BlockStates . Global . P.unsafeStaticFromForeignPtr $ coerce blockStatesFptr
    ||]
    <$#> (toPalette <$#> requiredField "palette") .= [|| error "TODO" ||]
    <*#> optionalField "data" .= [|| error "TODO" ||]
    where
      toPalette :: Code (SmallArray NBTBlockState -> Prim.Vector Int)
      toPalette = [|| \arr ->
        let lookupTag :: NBTBlockState -> Int 
            -- We don't need to sort here because Slice is already sorted
            -- TODO We are deep into unsafe behavior here, the nbt library is mine
            --  but still, we rely on internals here

            -- This function alone is ~30% of parsing a chunk, so ~50% of parsing blockstates. Fun.
            -- Ideas:
            -- - Pass 'SmallArray Property' 'data Property = Prop {-# UNPACK #-} !ByteString {-# UNPACK #-} !ByteString
            --   instead of the list of (probably lazy) pairs
            -- - partially inline propsToId. Specifically we want to inline the hashing part and keep the
            --   lookup or at least the literal out of line
            -- - Inlinening the hashing could fuse with toList and produce much cleaner code as it avoids
            --   the needless copy of SmallList or creation of the intermediate list
            -- - Use an actual hashtable instead of binary search?
            --   Write the literal as a flat array of (key,blockState)
            --   We need log_2(22k) ~ 14 steps to reach a value. A linear probing hashtable should
            --   be less. We can still use the perfect hash since we store keys
            lookupTag (NBTBlockState (NBTString name) mProps) = propsToId name props
              where
                props :: [(BS.ByteString, BS.ByteString)]
                props = maybe mempty toKvList mProps
                toKvList (TagCompound slice) = (\(NBT (NBTString n) v) -> (n, fromTagString v)) <$> toList slice
                toKvList _ = error "TODO errors"
                fromTagString (TagString (NBTString str)) = str
                fromTagString _ = error "TODO errors"
        in Prim.create $ do
            mvec <- PrimM.unsafeNew $ sizeofSmallArray arr
            -- This was the first reliably way to get ghc to unbox 'n'
            -- 'fromListN . fmap . toList' didn't fuse
            -- 'foldl' f (pure 0)' didn't unbox
            let go !n
                  | n >= sizeofSmallArray arr = pure ()
                  | otherwise =
                    let el = indexSmallArray arr n
                    in PrimM.write mvec n (lookupTag el) >> go (n + 1) 
            go 0
            pure mvec
        ||]

data NBTBlockState = NBTBlockState {-# UNPACK #-} !NBTString !(Maybe Tag)
  deriving stock Show

instance HasCodec NBTBlockState where
  codec = compound "block state" $ [|| NBTBlockState ||]
    <$#> requiredField "Name" .= [|| \(NBTBlockState n _) -> n ||]
    <*#> optionalField "Properties" .= [|| \(NBTBlockState _ mprops) -> mprops ||]

instance OldNBT.FromNBT BlockStates where
  parseNBT = OldNBT.withCompound $ \obj -> do
    palette <- fmap lookupTag <$> obj OldNBT..: "palette"

    if | V.length palette == 1 -> pure . BlockStates . SingleValue . coerce $ V.unsafeHead palette 
       | otherwise -> do
         (blockStates, len) <- S.unsafeToForeignPtr0 @Int64BE <$> obj OldNBT..: "data"
         let bitsPerVal = len `div` 64 -- TODO Why does this work?
         if | bitsPerVal < 4 -> error "TODO" -- TODO Error messages
            | bitsPerVal < 9 -> 
              let !intPalette = Prim.generate (V.length palette) $ (coerce . (V.!) palette) 
              in pure . BlockStates . Indirect intPalette . P.unsafeDynamicFromForeignPtr bitsPerVal $ coerce blockStates
            | otherwise -> pure . BlockStates . Global . P.unsafeStaticFromForeignPtr $ coerce blockStates
    where
      -- TODO This can further be optimized:
      -- Currently this does a lookup (propsToId) into a ~22k sorted array based on the hash of (BlockName, BlockProps)
      -- The hash function is fast, but maybe we can have the data in a less pointer heavy format to boost it further
      -- The array can be shrunk down to fewer elements if I do two lookups, one for BlockName and one for the props
      -- Also we have to sort props, that as well can be faster if its not in list form, we can and should just do it
      -- over a mutable vector
      lookupTag :: OldNBT.Tag -> BlockState
      lookupTag (OldNBT.TagCompound m) = maybe (error $ show m) id $ do
        OldNBT.TagString name <- case findWithIndexNBT "Name" m of
          (# _, (# NBT _ t | #) #) -> Just t
          (# _, (# | _ #) #) -> Nothing
        props <- case findWithIndexNBT "Properties" m of
          (# _, (# NBT _ (OldNBT.TagCompound props) | #) #) -> pure . fmap (fmap (\(OldNBT.TagString bs) -> bs)) . fmap (\(NBT k t) -> (k,t)) $ toList props
          (# _, (# | _ #) #) -> pure mempty
          _ -> Nothing
        -- We don't need to sort here because Slice is already sorted
        -- TODO We are deep into unsafe behavior here, the nbt library is mine
        --  but still, we rely on internals here
        pure . BlockState $ propsToId (coerce name) $ coerce props
      lookupTag _ = error "Wut?"
      {-# SCC lookupTag #-}
  {-# INLINE parseNBT #-}
  {-# SCC parseNBT #-}

findWithIndexNBT :: NBTString -> Slice NBT -> (# Int#, (# NBT | (# #) #) #)
findWithIndexNBT = Slice.findWithIndex(\(NBT k _) -> k) 

biomeMap :: HM.HashMap BS.ByteString Int
biomeMap = HM.fromList $ zip (T.encodeUtf8 . T.pack . fst <$> all_biome_settings) [0..]

instance HasCodec Biomes where
  codec = compound "biomes" $ [|| \palette mbiomes ->
    if Prim.length palette == 1
      then Biomes . SingleValue . coerce $ Prim.unsafeHead palette
      else
        let (blockStatesFptr, numEntries) = S.unsafeToForeignPtr0 @Int64BE $ fromMaybe (error "missing biomes") mbiomes -- TODO
            -- numEntries = bitsPerVal * 4096 / 64 => numEntries = bitsPerValue * 64 => numEntries / 64 = bitsPerValue
            bitsPerVal = if numEntries == 4 then 3 else numEntries
        in if bitsPerVal < 4 then
              let !intPalette = Prim.generate (Prim.length palette) (coerce $ (Prim.!) palette)
              in Biomes . Indirect intPalette . P.unsafeDynamicFromForeignPtr bitsPerVal $ coerce blockStatesFptr
            else
              Biomes . Global . P.unsafeStaticFromForeignPtr $ coerce blockStatesFptr
    ||]
    <$#> (toPalette <$#> requiredField "palette") .= [|| error "TODO" ||]
    <*#> optionalField "data" .= [|| error "TODO" ||]
    where
      toPalette :: Code (SmallArray NBTString -> Prim.Vector Int)
      toPalette = [|| \arr ->
        let lookupTag :: NBTString -> Int
            -- TODO Make sure I encode biome names in this map as modified utf8
            lookupTag (NBTString name) = fromMaybe (error $ show $ NBTString name) $ HM.lookup (BS.drop 10 name) biomeMap
        in Prim.fromListN (sizeofSmallArray arr) . fmap lookupTag $ toList arr 
        ||]

instance OldNBT.FromNBT Biomes where
  parseNBT = OldNBT.withCompound $ \obj -> do
    !intPalette <- lookupBiomes <$> obj OldNBT..: "palette"

    if | Prim.length intPalette == 1 -> pure . Biomes . SingleValue $ Prim.unsafeHead intPalette 
       | otherwise -> do
         (biomes, len) <- S.unsafeToForeignPtr0 @Int64BE <$> obj OldNBT..: "data"
         let bitsPerVal = if len == 4 then 3 else len -- TODO
         if | bitsPerVal < 4 -> pure . Biomes . Indirect intPalette . P.unsafeDynamicFromForeignPtr bitsPerVal $ coerce biomes
            | otherwise -> pure . Biomes . Global . P.unsafeStaticFromForeignPtr $ coerce biomes
    where
      lookupBiomes v = Prim.generate (V.length v) $ \i -> lookupBiome (v V.! i)
      lookupBiome :: OldNBT.Tag -> Int
      lookupBiome (OldNBT.TagString name) = fromMaybe (error $ show name) $ HM.lookup (BS.drop 10 $ coerce name) biomeMap
      lookupBiome _ = error "WUT=!"
      {-# SCC lookupBiome #-}
  {-# INLINE parseNBT #-}
  {-# SCC parseNBT #-}

countBlocks :: BlockStates -> Int
countBlocks (BlockStates (SingleValue val))
  | isAir $ BlockState val = 0
  | otherwise              = fromIntegral $ natVal @SectionSize undefined
countBlocks (BlockStates (Global vec)) 
  = sz - P.countElems (Prim.unsafeCast . Prim.fromList $ coerce @_ @[Int] [Air, VoidAir, CaveAir]) vec
  where !sz = fromIntegral $ natVal @SectionSize undefined
countBlocks (BlockStates (Indirect palette vec))
  | Prim.null airs = sz
  | otherwise = sz - P.countElems (Prim.unsafeCast airs) vec
  where
    !airs = Prim.findIndices (isAir . BlockState) palette
    !sz = fromIntegral $ natVal @SectionSize undefined
{-# SCC countBlocks #-}

isAir :: BlockState -> Bool
isAir Air     = True
isAir VoidAir = True
isAir CaveAir = True
isAir _       = False
{-# INLINE isAir #-}

numSections :: Int
numSections = 24 -- TODO config, also this depends on the dimension settings?!
