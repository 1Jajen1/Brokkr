{-# LANGUAGE PatternSynonyms  #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RecordWildCards #-}
module Brokkr.Chunk.Internal (
  Chunk(..)
, ChunkSection(..)
, ChunkPosition
, pattern ChunkPos
, HeightMaps
, getBlock
, setBlock
, fromIOChunk
, BlockStates(..)
, Biomes(..)
, LightVector(..)
, force, forceSection
) where

import Brokkr.Anvil.Chunk qualified as IO
import Brokkr.Anvil.Chunk.Section qualified as IO

import Brokkr.BlockState
import Brokkr.BlockState.Internal.Conversion qualified as Conv

import Brokkr.Chunk.BlockStates
import Brokkr.Chunk.HeightMap
import Brokkr.Chunk.Position

import Brokkr.NBT.Internal qualified as NBT
import Brokkr.NBT.NBTString.Internal

import Brokkr.PackedVector (PackedVector, DynamicNat(..))
import Brokkr.PackedVector qualified as PV
import Brokkr.PackedVector.Mutable qualified as MPV

import Brokkr.Util.PalettedVector (PalettedVector)
import Brokkr.Util.PalettedVector qualified as Palette

import Control.Monad.ST.Strict (runST)

import Data.Bits
import Data.Coerce
import Data.Foldable
import Data.Int

import Data.Primitive.PrimArray (sizeofPrimArray)

import Data.Proxy

import Data.Vector         qualified as V
import Data.Vector.Mutable qualified as MV

import Data.Vector.Storable qualified as S

import Foreign.C.Types (CUChar(..), CInt(..), CULong(..), CSize(..))

import GHC.Base (quotInt)
import GHC.ForeignPtr (unsafeWithForeignPtr)
import GHC.Generics (Generic)
import GHC.Ptr
import GHC.TypeLits
import GHC.IO (unsafePerformIO)

import Debug.Trace
import Control.Monad

data Chunk dimHeights = Chunk {
  position       :: {-# UNPACK #-} !ChunkPosition
, sections       :: {-# UNPACK #-} !(V.Vector ChunkSection)
, heightMaps     :: !(HeightMaps dimHeights)
}

getBlock :: Chunk dimHeights -> Int -> Int -> Int -> BlockState
getBlock Chunk{sections} chunkX y chunkZ =
  let (yInd, sectionY) = quotRem y 16
      section = sections V.! yInd
  in sectionGetBlock section chunkX sectionY chunkZ

setBlock :: Chunk dimHeights -> Int -> Int -> Int -> BlockState -> Chunk dimHeights
setBlock c@Chunk{sections} chunkX y chunkZ state =
  let (yInd, sectionY) = quotRem y 16
      section = sections V.! yInd
      newSection = sectionSetBlock section chunkX sectionY chunkZ state
  in c { sections = V.modify (\mv -> MV.write mv yInd newSection) sections}

-- Apply all delayed updates.
-- Only blocks for now, biomes and others will probably be done directly
-- as they are either rare or cheap
force :: Chunk dimHeights -> Chunk dimHeights
force c = c { sections = fmap forceSection (sections c) }

data ChunkSection = ChunkSection {
  blocks :: {-# UNPACK #-} !BlockStates
, biomes :: !Biomes
, skyLight   :: !(Maybe LightVector)
, blockLight :: !(Maybe LightVector)
}
  deriving stock (Show, Eq, Generic)

forceSection :: ChunkSection -> ChunkSection
forceSection cs = cs { blocks = applyBlockUpdate (blocks cs) }

newtype LightVector = LightVector (PackedVector ('Static IO.SectionSize) ('Static 4) Int)
  deriving stock Show
  deriving newtype Eq

sectionGetBlock :: ChunkSection -> Int -> Int -> Int -> BlockState
sectionGetBlock ChunkSection{blocks = BlockStates blockUpd _ blocks} x y z =
  let posInd = x `unsafeShiftL` 8 .|. z `unsafeShiftL` 4 .|. y
      fromArray = coerce $ Palette.unsafeIndex posInd blocks
  -- TODO Have some threshold for when it makes sense to apply all updates
  in case blockUpd of
    NoUpdate -> fromArray
    SingleBlock pos b
      | pos == posInd -> b
      | otherwise -> fromArray
    MultiBlock upds -> findBlockChange posInd upds
      (\_ind el -> el)
      $ const fromArray

shouldForce :: BlockUpdate -> Bool
shouldForce NoUpdate = False
shouldForce (SingleBlock _ _) = False
-- TODO Find better limits
shouldForce (MultiBlock upds) = sizeofPrimArray upds > 4

sectionSetBlock :: ChunkSection -> Int -> Int -> Int -> BlockState -> ChunkSection
sectionSetBlock sect@ChunkSection{blocks = BlockStates upd numNonAir arr} x y z state =
  let upd' = upd <> mkBlockChange x y z state
      blockStates  = BlockStates upd' numNonAir arr
  in if shouldForce upd'
    then sect { blocks = applyBlockUpdate blockStates }
    else sect { blocks = blockStates }

-- Biomes

type BiomePaletteMaxBitSize  = 6
type BiomeSectionSize = 64

newtype Biomes = Biomes (PalettedVector BiomeSectionSize BiomePaletteMaxBitSize)
  deriving stock Show
  deriving newtype Eq

emptySection :: ChunkSection
emptySection = ChunkSection {
  blocks = BlockStates mempty 0 (Palette.SingleValue $ coerce Air)  -- TODO Use correct air
, biomes = Biomes $ Palette.SingleValue 0 -- TODO
, skyLight = Nothing
, blockLight = Nothing -- TODO
}

-- from IO chunk
-- TODO List:
-- - Implement bit size conversion: Indirect to global that is
-- - Palette lookups
-- - HeightMap conversion
-- - SIMD block count
fromIOChunk :: forall dimHeight . (KnownNat dimHeight, KnownNat (ToHeightMapSize dimHeight)) => IO.Chunk -> Chunk dimHeight
fromIOChunk IO.Chunk{chunkSections = ioSections, heightMaps = ioHeightMaps, ..} =
  let sections = runST $ do
        mv <- MV.unsafeNew (fromIntegral (natVal (Proxy @dimHeight)) `quotInt` 16)
        -- mv <- MV.unsafeNew (2 + fromIntegral (natVal (Proxy @dimHeight)) `quotInt` 16)
        MV.set mv emptySection
        -- V.forM_ (traceShowId ioSections) $ \s -> do
        V.forM_ ioSections $ \s -> do
          when (fromIntegral (IO.sectionY s) >= coerce @_ @Int32 lowestYSection) $
            MV.unsafeWrite mv (fromIntegral @Int32 (coerce lowestYSection) * (-1) + fromIntegral (IO.sectionY s)) $ fromIOSection s
          -- MV.unsafeWrite mv (fromIntegral @Int32 (coerce lowestYSection) * (-1) + 1 + fromIntegral (IO.sectionY s)) $ fromIOSection s
        V.unsafeFreeze mv
      heightMaps = HeightMaps { motionBlocking = fromHeightMap @dimHeight <$> IO.motionBlocking ioHeightMaps }
      position = case chunkPosition of IO.ChunkPosition x z -> ChunkPosition (fromIntegral x) (fromIntegral z)
  in Chunk{..}

fromHeightMap :: forall dimHeight . KnownNat (ToHeightMapSize dimHeight) => IO.HeightMap -> HeightMap dimHeight
fromHeightMap (IO.HeightMap pv) = runST $ do
  newHM <- MPV.new @('Static 256) @('Static (ToHeightMapSize dimHeight))
  mpv <- PV.unsafeThaw pv
  MPV.unsafeCopy newHM mpv
  HeightMap  <$> PV.unsafeFreeze newHM

fromIOSection :: IO.ChunkSection -> ChunkSection
fromIOSection IO.ChunkSection{sectionBiomes = ioBiomes, sectionBlocks = ioBlocks, sectionSkyLight = ioSkyLight, sectionBlockLight = ioBlockLight} =
  let biomeEntryToId (IO.BiomeEntry bid) = 0 -- TODO error "TODO biomes"
      biomes = Biomes $ case ioBiomes of
        Nothing -> Palette.SingleValue (coerce Air) -- TODO Which air do I use? Air|VoidAir|CaveAir
        Just (IO.SingleBiome entry) -> Palette.SingleValue (biomeEntryToId entry)
        Just (IO.FullBiomes (IO.BiomePalette palette) biomes)
          | V.length palette > 1 `unsafeShiftL` biomesMaxIndirectSize
            -> error "Convert to global palette"
          | otherwise
            -- We copy the vector with 'force' to avoid retaining the initial bytestring
            -> Palette.Indirect (S.generate (V.length palette) (biomeEntryToId . V.unsafeIndex palette)) (PV.force biomes)
      blockEntryToId IO.BlockPaletteEntry{..} =
        -- TODO Handle errors
        let props = maybe [] (\case NBT.TagCompound slice -> (\(NBT.NBT k (NBT.TagString v)) -> (k, v)) <$> toList slice) blockProperties
            hs = Conv.hashProps (coerce blockName) (coerce props)
        in fromIntegral $ Conv.propsToId (coerce blockName) (coerce props) hs
      blocks0 = case ioBlocks of
        Nothing -> Palette.SingleValue 0 -- TODO
        Just (IO.SingleBlockState entry) -> Palette.SingleValue (blockEntryToId entry)
        Just (IO.FullBlockStates (IO.BlockPalette palette) blockStates)
          | V.length palette > 1 `unsafeShiftL` blockStatesMaxIndirectSize
            -> error "Convert to global palette"
          | otherwise
            -- We copy the vector with 'force' to avoid retaining the initial bytestring
            -> Palette.Indirect (S.generate (V.length palette) (blockEntryToId . V.unsafeIndex palette)) (PV.force blockStates)
      blocks = BlockStates mempty (countBlocks blocks0) blocks0 
      skyLight = coerce ioSkyLight
      blockLight = coerce ioBlockLight
  in ChunkSection{..}

biomesMaxIndirectSize, blockStatesMaxIndirectSize :: Int
biomesMaxIndirectSize = 3
blockStatesMaxIndirectSize = 8

-- TODO Do we need to force updates here? Not really I think, we need to recalculate on update anyway
-- TODO Actually do that. Recalculate once we update
-- I should probably move this too, and make this more generic to search for any block
countBlocks :: (KnownNat sz, KnownNat globSz) => PalettedVector sz globSz -> Int
countBlocks (Palette.SingleValue val)
  | isAir $ BlockState val = 0
  | otherwise              = fromIntegral $ natVal @IO.SectionSize undefined
countBlocks (Palette.Global vec)
  = sz - countElems (S.unsafeCast . S.fromListN 3 $ coerce @_ @[Int] [Air, VoidAir, CaveAir]) vec
  where !sz = fromIntegral $ natVal @IO.SectionSize undefined
countBlocks (Palette.Indirect palette vec)
  | S.null airs = sz
  | otherwise = sz - countElems (S.unsafeCast airs) vec
  where
    !airs = S.findIndices (isAir . BlockState) palette
    !sz = fromIntegral $ natVal @IO.SectionSize undefined
{-# SCC countBlocks #-}

isAir :: BlockState -> Bool
isAir Air     = True
isAir VoidAir = True
isAir CaveAir = True
isAir _       = False
{-# INLINE isAir #-}

-- SIMD element count
-- TODO So this needs a small rework:
-- - I need to make the primvector a storable vector
-- - Make sure the vectors are both alive across the c-call
countElems :: PV.PVector v a => S.Vector Word -> v a -> Int
countElems els pv =
  let len = PV.length pv
      perWord = 64 `quotInt` bSz
      wLen = (len + perWord - 1) `quotInt` perWord
      bSz = PV.bitSize pv
      fp = PV.unsafeBacking pv
      (fpEls, elsSz) = S.unsafeToForeignPtr0 els
  in unsafePerformIO $ unsafeWithForeignPtr fpEls $ \ptrEls ->
        unsafeWithForeignPtr fp $ \ptr ->
          pure . fromIntegral $ c_countElems (fromIntegral bSz) (fromIntegral len) (coerce ptr) (fromIntegral wLen) (coerce ptrEls) (fromIntegral elsSz)
{-# INLINE countElems #-}

foreign import ccall unsafe "countElems" c_countElems :: CUChar -> CSize -> Ptr CUChar -> CSize -> Ptr CULong -> CSize -> CInt
