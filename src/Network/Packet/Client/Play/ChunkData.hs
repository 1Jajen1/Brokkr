{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
module Network.Packet.Client.Play.ChunkData (
  ChunkData
, mkChunkData
) where

import Chunk.Heightmap
import Chunk.Position

import Control.Monad.ST (runST)

import Data.Coerce
import Data.Int
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Primitive as Prim
import Data.Monoid (Sum(..))

import qualified IO.Chunk as IO

import Network.Util.VarNum

import Util.BitSet (BitSet)
import qualified Util.BitSet as BitSet
import Util.Binary
import Util.NBT
import Util.PalettedVector
import Util.Vector.Packed (PackedVector, DynamicNat(..))
import qualified Util.Vector.Packed as PV

type NibbleVector = PackedVector ('Static 4096) ('Static 4)

data ChunkData = ChunkData !Int !ChunkPosition !Heightmaps !(Vector IO.ChunkSection) !BitSet !BitSet !(Vector NibbleVector) !(Vector NibbleVector)
  deriving stock Show

mkChunkData :: IO.Chunk -> (Int, ChunkData)
mkChunkData IO.Chunk{..} = (minPacketSz, ChunkData sectionsByteSize position heightmaps sections skyLightBitSet blockLightBitSet skyLightArr blockLightArr)
  where
    !(!skyLightBitSet, !skyLightArr) = makeLightBitSetAndArray IO.skyLight 
    !(!blockLightBitSet, !blockLightArr) = makeLightBitSetAndArray IO.blockLight

    -- TODO This is needlessly slow (immutable bitset is modified over and over again instead of a mutable one) and it assumes a few constants that may not be valid
    -- -4 is the lowest chunk section y value (is it? o.O)
    -- 26 is the number of chunk sections for 320 up and -64 down worlds
    makeLightBitSetAndArray sel = V.foldl'
      (\acc@(bSet, vec) sect -> maybe acc (\x -> (BitSet.set bSet $ (IO.y sect) + 5, V.snoc vec x)) (sel sect))
      (BitSet.emptyBitSet $ IO.numSections + 2, mempty)
      sections

    !minPacketSz =
        1 -- PacketId
      + 4 + 4 -- Chunkposition
      + 1 + 2 -- NBT Compound header empty name
      + 1 + 2 + 15 + 4 + 37 * 8 -- NBT Long array with a single heightmap with 256 9 bit values, so floor (64 / 9) = 7 and ceil (256 / 7) = 37
      + 1 -- End tag
      + (varIntSize sectionsByteSize) -- Size prefix for the sections data
      + sectionsByteSize -- sections data
      + 1 -- No block entities for now -- TODO
      + 1 -- Trust edges bool -- TODO
      -- TODO This can be done smarter ... But the problem is java bitsets are sent as 0 if empty... and if we ever go beyond one long we might face more differences...
      + BitSet.byteSize skyLightBitSet -- Skylight bitset
      + BitSet.byteSize blockLightBitSet -- Blocklight bitset
      + BitSet.byteSize (BitSet.complement skyLightBitSet) -- Complement Skylight bitset
      + BitSet.byteSize (BitSet.complement blockLightBitSet) -- Complement Blocklight bitset
      + 1 + (varIntSize 2048) + 2048 * V.length skyLightArr -- Skylight, varInt prefixed
      + 1 + (varIntSize 2048) + 2048 * V.length blockLightArr -- BlockLight, varInt prefixed
    -- This needs to be exact, minPacketSz could use an approximation
    !sectionsByteSize = V.foldl' (\acc s -> acc + sectionByteSize s) 0 sections
    sectionByteSize IO.ChunkSection{..} = 2 + paletteContainerSz (coerce blocks) + paletteContainerSz (coerce biomes)
    paletteContainerSz (Global v) = 1 + (varIntSize numLongs) + 8 * numLongs
      where
        !bSz = runST $ PV.bitSz <$> PV.unsafeThaw v
        !len = runST $ PV.length <$> PV.unsafeThaw v
        !numLongs = PV.nrWords bSz len
    paletteContainerSz (SingleValue v) = 1 + varIntSize v + 1
    paletteContainerSz (Indirect p v) = 1 + (varIntSize $ Prim.length p) + paletteByteSz + (varIntSize numLongs) + 8 * numLongs
      where
        !paletteByteSz = getSum $ Prim.foldMap (\el -> Sum $ varIntSize $ el) p
        !bSz = runST $  PV.bitSz <$> PV.unsafeThaw v
        !len = runST $  PV.length <$> PV.unsafeThaw v
        !numLongs = PV.nrWords bSz len

-- data ChunkData = ChunkData !Int !ChunkPosition !Heightmaps !(Vector ChunkSection) !BitSet !BitSet !(Vector NibbleVector) !(Vector NibbleVector)
--   deriving stock (Show, Generic)
--   deriving anyclass NFData
-- 
-- mkChunkData :: Chunk -> (Int, ChunkData)
-- mkChunkData !Chunk{..} = (minPacketSz, ChunkData sectionsByteSize _position _heightmaps _sections skyLightBitSet blockLightBitSet skyLightArr blockLightArr)
--   where
--      -- TODO This is ugly and likely slow Convert to one mutable method and get the constant 26 somewhere else
--     !(!skyLightBitSet, !skyLightArr) = V.foldl' (\acc@(bSet, vec) ChunkSection{..} -> maybe acc (\x -> (BitSet.set bSet $ y + 1, V.snoc vec x)) skyLight) (BitSet.emptyBitSet 26, mempty) _sections
--     !(!blockLightBitSet, !blockLightArr) = V.foldl' (\acc@(bSet, vec) ChunkSection{..} -> maybe acc (\x -> (BitSet.set bSet $ y + 1, V.snoc vec x)) blockLight) (BitSet.emptyBitSet 26, mempty) _sections
--     !minPacketSz = -- TODO replace VarInt sizes with varIntSize and make sure it is evaluated at compile time!
--         1 -- PacketId
--       + 4 + 4 -- Chunkposition
--       + 1 + 2 -- NBT Compound header empty name
--       + 1 + 2 + 15 + 4 + 37 * 8 -- NBT Long array with a single heightmap with 256 9 bit values, so floor (64 / 9) = 7 and ceil (256 / 7) = 37
--       + 1 -- End tag
--       + (varIntSize sectionsByteSize) -- Size prefix for the sections data
--       + sectionsByteSize -- sections data
--       + 1 -- No block entities for now -- TODO
--       + 1 -- Trust edges bool -- TODO
--       -- TODO This can be done smarter ... But the problem is java bitsets are sent as 0 if empty... and if we ever go beyond one long we might face more differences...
--       + BitSet.byteSize skyLightBitSet -- Skylight bitset
--       + BitSet.byteSize blockLightBitSet -- Blocklight bitset
--       + BitSet.byteSize (BitSet.complement skyLightBitSet) -- Complement Skylight bitset
--       + BitSet.byteSize (BitSet.complement blockLightBitSet) -- Complement Blocklight bitset
--       + 1 + (varIntSize 2048) + 2048 * V.length skyLightArr -- Skylight, varInt prefixed
--       + 1 + (varIntSize 2048) + 2048 * V.length blockLightArr -- BlockLight, varInt prefixed
--     
--     -- This needs to be exact, minPacketSz could use an approximation
--     !sectionsByteSize = V.foldl' (\acc s -> acc + sectionByteSize s) 0 _sections
--     sectionByteSize ChunkSection{..} = 2 + paletteContainerSz (coerce blocks) + paletteContainerSz (coerce biomes)
--     paletteContainerSz (Global v) = 1 + (varIntSize numLongs) + 8 * numLongs
--       where
--         !bSz = runST $ PV.bitSz <$> PV.unsafeThaw v
--         !len = runST $ PV.length <$> PV.unsafeThaw v
--         !numLongs = PV.nrWords bSz len
--     paletteContainerSz (SingleValue v) = 1 + varIntSize v + 1
--     paletteContainerSz (Indirect p v) = 1 + (varIntSize $ S.length p) + paletteByteSz + (varIntSize numLongs) + 8 * numLongs
--       where
--         !paletteByteSz = getSum $ S.foldMap (\el -> Sum $ varIntSize $ el) p
--         !bSz = runST $  PV.bitSz <$> PV.unsafeThaw v
--         !len = runST $  PV.length <$> PV.unsafeThaw v
--         !numLongs = PV.nrWords bSz len
-- 
instance ToBinary ChunkData where
  put (ChunkData sectionsByteSize (ChunkPos x z) heightmaps sections  skyLightMask blockLightMask skyLight blockLight) =
       put (fromIntegral @_ @Int32 x) <> put (fromIntegral @_ @Int32 z)
    <> put (BinaryNBT heightmaps)
    <> put (VarInt . fromIntegral $ sectionsByteSize)
    <> sectionsBytes
    <> put @VarInt 0 -- TODO Block entities
    <> put True -- Trust edges??
    <> put skyLightMask
    <> put blockLightMask
    <> put (BitSet.complement skyLightMask)
    <> put (BitSet.complement blockLightMask)
    <> put (VarInt . fromIntegral $ V.length skyLight)
    <> V.foldMap (\i -> put (VarInt 2048) <> put i) skyLight
    <> put (VarInt . fromIntegral $ V.length blockLight)
    <> V.foldMap (\i -> put (VarInt 2048) <> put i) blockLight
    where
      !sectionsBytes = V.foldMap putSection sections -- Do we send all here? Including the y = -1 section?
      putSection IO.ChunkSection{blockCount,blocks,biomes} =
           put (fromIntegral @_ @Int16 blockCount)
        <> put blocks
        <> put biomes
  {-# INLINE put #-}
