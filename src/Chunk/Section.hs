{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiWayIf #-}
--{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
{-# LANGUAGE DeriveAnyClass #-}
module Chunk.Section (
  ChunkSection(..)
, PalettedVector(..)
) where

import Util.Vector.Packed (PackedVector, DynamicNat(..))
import qualified Util.Vector.Packed as P

import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as U
import GHC.TypeLits
import Util.NBT.Internal
import Data.Int
import qualified Data.Vector.Storable as S
import Data.Coerce (coerce)
import qualified Data.Vector as V
import Block.Internal.BlockState
import Block.Internal.Conversion
import qualified Data.HashMap.Strict as HM
import qualified Data.Map.Strict as M
import GHC.Generics (Generic)
import Control.DeepSeq

type NibbleVector = PackedVector ('Static 4096) ('Static 4)

data ChunkSection = ChunkSection {
  y          :: {-# UNPACK #-} !Int
, blockLight ::                !(Maybe NibbleVector)
, skyLight   ::                !(Maybe NibbleVector)
, blocks     ::                !BlockStates
, biomes     ::                !Biomes
, blockCount :: {-# UNPACK #-} !Int
}
  deriving stock Generic
  deriving anyclass NFData

-- TODO Generate at compile time since 1 + Log2 is not actually correct if the HighestBlockStateId is a perfect power of 2 
type BlockPaletteMaxBitsize = 1 + Log2 HighestBlockStateId 
type BiomePaletteMaxBitsze  = 6

type SectionSize = 4096
type BiomeSectionSize = 64

newtype BlockStates = BlockStates (PalettedVector SectionSize BlockPaletteMaxBitsize)
  deriving newtype NFData

newtype Biomes = Biomes (PalettedVector BiomeSectionSize BiomePaletteMaxBitsze)
  deriving newtype NFData

data PalettedVector (sz :: Nat) (globalBitSz :: Nat) =
    Global      {-# UNPACK #-} !(PackedVector ('Static sz) ('Static globalBitSz))
  | SingleValue {-# UNPACK #-} !Int
  | Indirect    {-# UNPACK #-} !(Vector Int) {-# UNPACK #-} !(PackedVector ('Static sz) 'Dynamic)

instance NFData (PalettedVector sz maxBitSz) where
  rnf (Global vec) = rnf vec
  rnf (SingleValue w) = rnf w
  rnf (Indirect p vec) = rnf p `seq` rnf vec

instance FromNBT ChunkSection where
  parseNBT = withCompound $ \obj -> do
    y <- fromIntegral @Int8 <$> obj .: "Y"

    !blockLight <- obj .:? "BlockLight" -- TODO Make sure the Maybe is fully forced
    !skyLight <- obj .:? "SkyLight"

    blocks <- obj .: "block_states"

    let blockCount = countBlocks blocks

    biomes <- obj .: "biomes"
    pure ChunkSection{..}

instance FromNBT NibbleVector where
  parseNBT tag = do
    (vec, _) <- S.unsafeToForeignPtr0 <$> (parseNBT @(S.Vector Int8) tag)
    pure $ P.unsafeStaticFromForeignPtr $ coerce vec

instance FromNBT BlockStates where
  parseNBT = withCompound $ \obj -> do
    palette <- (fmap lookupTag) <$> obj .: "palette"

    if | V.length palette == 1 -> pure . BlockStates . SingleValue . coerce $ V.unsafeHead palette 
       | otherwise -> do
         (blockStates, len) <- S.unsafeToForeignPtr0 @Int64 <$> obj .: "data"
         let bitsPerVal = len `div` 64
         if | bitsPerVal < 4 -> error "TODO" -- TODO Error messages
            | bitsPerVal < 9 -> 
              let !intPalette = U.generate (V.length palette) $ (coerce . (V.!) palette) 
              in pure . BlockStates . Indirect intPalette . P.unsafeDynamicFromForeignPtr bitsPerVal $ coerce blockStates
            | otherwise -> pure . BlockStates . Global . P.unsafeStaticFromForeignPtr $ coerce blockStates
    where
      lookupTag :: Tag -> BlockState
      lookupTag (TagCompound m) = maybe (error $ show m) id $ do
        TagString name <- M.lookup "Name" m
        props <- case M.lookup "Properties" m of
          Just (TagCompound props) -> pure props
          Nothing -> pure mempty
          _ -> Nothing
        f <- HM.lookup name propsToId
        pure . BlockState . f $ fmap (\(TagString s) -> s) props
      lookupTag _ = error "Wut?"

instance FromNBT Biomes where
  parseNBT = withCompound $ \_ -> do
    pure . Biomes $ SingleValue 0 -- TODO

countBlocks :: BlockStates -> Int
countBlocks (BlockStates (SingleValue val))
  | isAir $ BlockState val = 0
  | otherwise              = fromIntegral $ natVal @SectionSize undefined
countBlocks (BlockStates (Global vec)) 
  = P.countElems (fromIntegral @Int . coerce <$> [Air, VoidAir, CaveAir]) vec
countBlocks (BlockStates (Indirect palette vec))
  | null airs = fromIntegral $ natVal @SectionSize undefined
  | otherwise = P.countElems (fromIntegral <$> airs) vec
  where
    !airs = U.toList $ U.findIndices (\x -> isAir $ BlockState x) palette

isAir :: BlockState -> Bool
isAir Air     = True
isAir VoidAir = True
isAir CaveAir = True
isAir _       = False
