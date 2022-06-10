{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
module Network.Packet.Client.Play.ChunkData (
  ChunkData
, mkChunkData
) where

import Util.Binary
import Chunk.Internal
import Chunk.Section
import Util.NBT
import Data.Int
import Network.Util.VarNum
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as U
import Data.Coerce
import qualified Util.Vector.Packed as PV
import Control.Monad.ST (runST)
import Util.Vector.Packed
import Util.BitSet (BitSet)
import qualified Util.BitSet as BitSet
import Data.Semigroup

type NibbleVector = PackedVector ('Static 4096) ('Static 4)

data ChunkData = ChunkData !Int !ChunkPosition !Heightmaps !(Vector ChunkSection) !BitSet !BitSet !(Vector NibbleVector) !(Vector NibbleVector)
  deriving stock Show

mkChunkData :: Chunk -> (Int, ChunkData)
mkChunkData Chunk{..} = (minPacketSz, ChunkData sectionsByteSize position heightmaps sections skyLightBitSet blockLightBitSet skyLightArr blockLightArr)
  where
     -- TODO This is ugly and likely slow Convert to one mutable method and get the constant 26 somewhere else
    (skyLightBitSet, skyLightArr) = V.foldl' (\acc@(bSet, vec) ChunkSection{..} -> maybe acc (\x -> (BitSet.set bSet $ y + 1, V.snoc vec x)) skyLight) (BitSet.emptyBitSet 26, mempty) sections
    (blockLightBitSet, blockLightArr) = V.foldl' (\acc@(bSet, vec) ChunkSection{..} -> maybe acc (\x -> (BitSet.set bSet $ y + 1, V.snoc vec x)) blockLight) (BitSet.emptyBitSet 26, mempty) sections
    minPacketSz = -- TODO replace VarInt sizes with varIntSize and make sure it is evaluated at compile time!
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
      
    sectionsByteSize = V.foldl' (\acc s -> acc + sectionByteSize s) 0 sections
    sectionByteSize ChunkSection{..} = 2 + paletteContainerSz (coerce blocks) + paletteContainerSz (coerce biomes)
    paletteContainerSz (Global v) = 1 + (varIntSize numLongs) + 8 * numLongs
      where
        bSz = runST $  PV.bitSz <$> PV.unsafeThaw v
        len = runST $  PV.length <$> PV.unsafeThaw v
        numLongs = PV.nrWords bSz len
    paletteContainerSz (SingleValue v) = 1 + varIntSize v + 1
    paletteContainerSz (Indirect p v) = 1 + (varIntSize $ U.length p) + paletteByteSz + (varIntSize numLongs) + 8 * numLongs
      where
        paletteByteSz = getSum $ U.foldMap (\el -> Sum $ varIntSize $ el) p
        bSz = runST $  PV.bitSz <$> PV.unsafeThaw v
        len = runST $  PV.length <$> PV.unsafeThaw v
        numLongs = PV.nrWords bSz len

instance ToBinary ChunkData where
  put (ChunkData sectionsByteSize (ChunkPos x z) heightmaps sections  skyLightMask blockLightMask skyLight blockLight) =
       put (fromIntegral @_ @Int32 x) <> put (fromIntegral @_ @Int32 z)
    <> put (BinaryNBT heightmaps)
    <> put (VarInt . fromIntegral $ sectionsByteSize)
    <> sectionsBytes
    <> put @VarInt 0 -- TODO Block entities
    <> put False -- Trust edges?
    <> put skyLightMask
    <> put blockLightMask
    <> put (BitSet.complement skyLightMask)
    <> put (BitSet.complement blockLightMask)
    <> put (VarInt . fromIntegral $ V.length skyLight)
    <> V.foldMap (\i -> put (VarInt 2048) <> put i) skyLight
    <> put (VarInt . fromIntegral $ V.length blockLight)
    <> V.foldMap (\i -> put (VarInt 2048) <> put i) blockLight
    where
      sectionsBytes = V.foldMap putSection sections -- Do we send all here? Including the y = -1 section?
      putSection ChunkSection{blockCount,blocks,biomes} =
           put (fromIntegral @_ @Int16 blockCount)
        <> put blocks
        <> put biomes
