{-# LANGUAGE RecordWildCards #-}
module Brokkr.Network.Util.Chunk (
  mkChunkPacket
) where

import Brokkr.Chunk.HeightMap
import Brokkr.Chunk.Internal
import Brokkr.Chunk.Position

import Brokkr.Packet.ServerToClient.Play qualified as Play

import Brokkr.PackedVector.Internal qualified as PV

import Brokkr.Util.BitSet
import Brokkr.Util.PalettedVector

import Data.Coerce
import Data.Proxy
import Data.Monoid

import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Vector.Storable qualified as S

import GHC.Base (quotInt)
import GHC.TypeLits

byteSize :: BitSet -> Int
byteSize (BitSet v) = varIntSize (S.length v) + 8 * S.length v

mkChunkPacket :: forall dimHeight . KnownNat dimHeight => Chunk dimHeight -> (Int, Play.PlayPacket dimHeight)
mkChunkPacket Chunk{..} = (minPacketSz, Play.ChunkDataAndUpdateLight
  (fromIntegral cX) (fromIntegral cZ) (coerce heightMaps)
  sectionData mempty Play.TrustEdges lightData)
  where
    lightData = Play.LightData
      (coerce skyLightBitSet) (coerce blockLightBitSet)
      (coerce $ complement skyLightBitSet) (coerce $ complement blockLightBitSet)
      (coerce skyLightArr)
      (coerce blockLightArr)
    sectionData = Play.SectionData (fromIntegral sectionsByteSize) (fmap toNetworkSection sections)
    ChunkPosition cX cZ = position

    toNetworkSection ChunkSection{blocks = BlockStates _ nonAir blocks, biomes = Biomes biomes,..} = Play.Section
      (Play.BlockCount $ fromIntegral nonAir)
      (Play.BlockStates $ case blocks of
        SingleValue x -> Play.Direct $ fromIntegral x
        Indirect pal x -> Play.Indirect (Play.Palette $ U.generate (S.length pal) $ \i -> fromIntegral $ S.unsafeIndex pal i) x
        Global x -> Play.Global x
        )
      (Play.Biomes $ case biomes of
        SingleValue x -> Play.Direct $ fromIntegral x
        Indirect pal x -> Play.Indirect (Play.Palette $ U.generate (S.length pal) $ \i -> fromIntegral $ S.unsafeIndex pal i) x
        Global x -> Play.Global x
        ) 

    numSections = fromIntegral (natVal (Proxy @dimHeight)) `quotInt` 16
    !(!skyLightBitSet, !skyLightArr) = makeLightBitSetAndArray skyLight
    !(!blockLightBitSet, !blockLightArr) = makeLightBitSetAndArray blockLight

    makeLightBitSetAndArray sel = V.ifoldl'
      (\acc@(bSet, vec) y sect -> maybe acc (\x -> (set bSet y, V.snoc vec x)) (sel sect))
      -- (emptyBitSet $ numSections + 2, mempty)
      (emptyBitSet numSections, mempty)
      sections

    !minPacketSz =
        1 -- PacketId
      + 4 + 4 -- Chunkposition
      + 1 + 2 -- NBT Compound header empty name
      + 1 + 2 + 15 + 4 + 37 * 8 -- NBT Long array with a single heightmap with 256 9 bit values, so floor (64 / 9) = 7 and ceil (256 / 7) = 37 -- TODO Derive from dimHeight 
      + 1 -- End tag
      + varIntSize sectionsByteSize -- Size prefix for the sections data
      + sectionsByteSize -- sections data
      + 1 -- No block entities for now -- TODO
      + 1 -- Trust edges bool -- TODO
      -- TODO This can be done smarter ... But the problem is java bitsets are sent as 0 if empty... and if we ever go beyond one long we might face more differences...
      + byteSize skyLightBitSet -- Skylight bitset
      + byteSize blockLightBitSet -- Blocklight bitset
      + byteSize (complement skyLightBitSet) -- Complement Skylight bitset
      + byteSize (complement blockLightBitSet) -- Complement Blocklight bitset
      + 1 + varIntSize 2048 + 2048 * V.length skyLightArr -- Skylight, varInt prefixed
      + 1 + varIntSize 2048 + 2048 * V.length blockLightArr -- BlockLight, varInt prefixed
    -- This needs to be exact, minPacketSz could use an approximation
    !sectionsByteSize = V.foldl' (\acc s -> acc + sectionByteSize s) 0 sections
    sectionByteSize ChunkSection{blocks = BlockStates _ _ blocks, ..} = 2 + paletteContainerSz (coerce blocks) + paletteContainerSz (coerce biomes)
    paletteContainerSz (Global v) = 1 + varIntSize numLongs + 8 * numLongs
      where
        !bSz = PV.bitSize v
        !len = PV.length v
        !perWord = 64 `quotInt` bSz
        !numLongs = (len + perWord - 1) `quotInt` perWord
    paletteContainerSz (SingleValue v) = 1 + varIntSize v + 1
    paletteContainerSz (Indirect p v)  = 1 + varIntSize (S.length p) + paletteByteSz + varIntSize numLongs + 8 * numLongs
      where
        !paletteByteSz = getSum $ S.foldMap (Sum . varIntSize) p
        !bSz = PV.bitSize v
        !len = PV.length v
        !perWord = 64 `quotInt` bSz
        !numLongs = (len + perWord - 1) `quotInt` perWord

-- TODO Move
varIntSize :: Int -> Int
varIntSize i
  | i < 0         = 5
  | i < 128       = 1
  | i < 16384     = 2
  | i < 2097152   = 3
  | i < 268435456 = 4
  | otherwise     = 5
