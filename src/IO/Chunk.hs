{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE MultiWayIf #-}
module IO.Chunk (
  Chunk(..)
, ChunkSection(..)
, Biomes(..)
, BlockStates(..)
, countBlocks
) where

import Registry.Biome

import Block.Internal.BlockState
import Block.Internal.Conversion

import Chunk.Heightmap
import Chunk.Position

import Control.DeepSeq

import Data.Coerce
import Data.Int
import Data.List (sortOn)

import qualified Data.ByteString as BS

import qualified Data.HashMap.Strict as HM

import Data.Maybe (fromMaybe)

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import qualified Data.Vector as V
import qualified Data.Vector.Storable as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Primitive as Prim

import GHC.Generics
import GHC.TypeLits

import Util.Binary
import Util.NBT
import Util.NBT.Internal

import Util.Vector.Packed (PackedVector, DynamicNat(..))
import qualified Util.Vector.Packed as P

import Util.PalettedVector

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
  deriving (FromBinary) via BinaryNBT Chunk

type NibbleVector = PackedVector ('Static 4096) ('Static 4)

data ChunkSection = ChunkSection {
  y          :: {-# UNPACK #-} !Int
, blockLight ::                !(Maybe NibbleVector)
, skyLight   ::                !(Maybe NibbleVector)
, blocks     ::                !BlockStates
, biomes     ::                !Biomes
, blockCount :: {-# UNPACK #-} !Int
}
  deriving stock (Show, Generic)
  deriving anyclass NFData

type BlockPaletteMaxBitsize = 1 + Log2 (HighestBlockStateId - 1) 
type BiomePaletteMaxBitsze  = 6

type SectionSize = 4096
type BiomeSectionSize = 64

newtype BlockStates = BlockStates (PalettedVector SectionSize BlockPaletteMaxBitsize)
  deriving stock Show
  deriving newtype (NFData, ToBinary)

newtype Biomes = Biomes (PalettedVector BiomeSectionSize BiomePaletteMaxBitsze)
  deriving stock Show
  deriving newtype (NFData, ToBinary)

--
instance FromNBT Chunk where
  parseNBT = withCompound $ \obj -> do
    !xPos <- fromIntegral @Int32 <$> obj .: "xPos"
    !zPos <- fromIntegral @Int32 <$> obj .: "zPos"
    let !position = ChunkPos xPos zPos

    !lowestYSection <- fromIntegral @Int32 <$> obj .: "yPos"

    !sections <- obj .: "sections"

    !heightmaps <- obj .: "Heightmaps"

    pure Chunk{..}
  {-# INLINE parseNBT #-}
  {-# SCC parseNBT #-}
  
instance FromNBT ChunkSection where
  parseNBT = withCompound $ \obj -> do
    !y <- fromIntegral @Int8 <$> obj .: "Y"

    !blockLight <- obj .:? "BlockLight" -- TODO Make sure the Maybe is fully forced
    !skyLight <- obj .:? "SkyLight"

    !blocks <- obj .: "block_states"

    let !blockCount = countBlocks blocks

    !biomes <- obj .: "biomes"
    pure ChunkSection{..}
  {-# INLINE parseNBT #-}
  {-# SCC parseNBT #-}

instance FromNBT BlockStates where
  parseNBT = withCompound $ \obj -> do
    palette <- fmap lookupTag <$> obj .: "palette"

    if | V.length palette == 1 -> pure . BlockStates . SingleValue . coerce $ V.unsafeHead palette 
       | otherwise -> do
         (blockStates, len) <- S.unsafeToForeignPtr0 @Int64 <$> obj .: "data"
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
      lookupTag :: Tag -> BlockState
      lookupTag (TagCompound m) = maybe (error $ show m) id $ do
        TagString name <- HM.lookup "Name" m
        props <- case HM.lookup "Properties" m of
          Just (TagCompound props) -> pure . fmap (fmap (\(TagString bs) -> bs)) $ HM.toList props
          Nothing -> pure mempty
          _ -> Nothing
        pure . BlockState $ propsToId (coerce name) $ coerce $ sortOn fst props
      lookupTag _ = error "Wut?"
      {-# SCC lookupTag #-}
  {-# INLINE parseNBT #-}
  {-# SCC parseNBT #-}

biomeMap :: HM.HashMap BS.ByteString Int
biomeMap = HM.fromList $ zip (T.encodeUtf8 . T.pack . fst <$> all_biome_settings) [0..]

instance FromNBT Biomes where
  parseNBT = withCompound $ \obj -> do
    !intPalette <- lookupBiomes <$> obj .: "palette"

    if | Prim.length intPalette == 1 -> pure . Biomes . SingleValue $ Prim.unsafeHead intPalette 
       | otherwise -> do
         (biomes, len) <- S.unsafeToForeignPtr0 @Int64 <$> obj .: "data"
         let bitsPerVal = if len == 4 then 3 else len -- TODO
         if | bitsPerVal < 4 -> pure . Biomes . Indirect intPalette . P.unsafeDynamicFromForeignPtr bitsPerVal $ coerce biomes
            | otherwise -> pure . Biomes . Global . P.unsafeStaticFromForeignPtr $ coerce biomes
    where
      lookupBiomes v = Prim.generate (V.length v) $ \i -> lookupBiome (v V.! i)
      lookupBiome :: Tag -> Int
      lookupBiome (TagString name) = fromMaybe (error $ show name) $ HM.lookup (BS.drop 10 $ coerce name) biomeMap
      lookupBiome _ = error "WUT=!"
      {-# SCC lookupBiome #-}
  {-# INLINE parseNBT #-}
  {-# SCC parseNBT #-}

countBlocks :: BlockStates -> Int
countBlocks (BlockStates (SingleValue val))
  | isAir $ BlockState val = 0
  | otherwise              = fromIntegral $ natVal @SectionSize undefined
countBlocks (BlockStates (Global vec)) 
  = sz - P.countElems (coerce $ Prim.fromList $ coerce @_ @[Int] [Air, VoidAir, CaveAir]) vec
  where !sz = fromIntegral $ natVal @SectionSize undefined
countBlocks (BlockStates (Indirect palette vec))
  | Prim.null airs = sz
  | otherwise = sz - P.countElems (coerce airs) vec
  where
    !airs = Prim.findIndices (\x -> isAir $ BlockState x) palette
    !sz = fromIntegral $ natVal @SectionSize undefined
{-# SCC countBlocks #-}

isAir :: BlockState -> Bool
isAir Air     = True
isAir VoidAir = True
isAir CaveAir = True
isAir _       = False
{-# INLINE isAir #-}
