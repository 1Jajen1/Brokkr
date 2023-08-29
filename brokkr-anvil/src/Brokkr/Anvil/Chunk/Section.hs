{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Brokkr.Anvil.Chunk.Section (
  ChunkSection(..)
, SectionSize, SectionBiomes
, BlockStates(..)
, BlockPalette(..)
, BlockPaletteEntry(..)
, Biomes(..)
, BiomePalette(..)
, BiomeEntry(..)
, BlockLight(..)
, SkyLight(..)
) where

import Brokkr.PackedVector (PackedVector, DynamicNat(..))
import Brokkr.PackedVector.Internal qualified as PV

import Brokkr.NBT
import Brokkr.NBT.Codec
import Brokkr.NBT.Class qualified as Class

import Data.Maybe
import Data.Coerce
import Data.Functor
import Data.Int
import Data.Proxy

import Data.Vector qualified as V
import Data.Vector.Storable qualified as S

import GHC.Base (quotInt)
import GHC.TypeLits

type SectionSize = 4096
type SectionBiomes = 64

data ChunkSection = ChunkSection {
  sectionY      :: !Int8
, sectionBlocks :: !(Maybe BlockStates)
, sectionBiomes :: !(Maybe Biomes)
, sectionBlockLight :: !(Maybe BlockLight)
, sectionSkyLight   :: !(Maybe SkyLight)
}
  deriving stock (Eq, Show)

data BlockStates =
    SingleBlockState !BlockPaletteEntry
  | FullBlockStates  !BlockPalette !(PackedVector ('Static SectionSize) 'Dynamic Int)
  deriving stock (Eq, Show)

newtype BlockPalette = BlockPalette (V.Vector BlockPaletteEntry)
  deriving stock (Eq, Show)
  deriving newtype (Class.FromNBT, Class.ToNBT)

data BlockPaletteEntry = BlockPaletteEntry {
  blockName       :: !NBTString
, blockProperties :: !(Maybe Tag)
}
  deriving stock (Eq, Show)

data Biomes =
    SingleBiome !BiomeEntry
  | FullBiomes  !BiomePalette !(PackedVector ('Static SectionBiomes) 'Dynamic Int)
  deriving stock (Eq, Show)

newtype BiomePalette = BiomePalette (V.Vector BiomeEntry)
  deriving stock (Eq, Show)
  deriving newtype (Class.FromNBT, Class.ToNBT)

newtype BiomeEntry = BiomeEntry NBTString
  deriving stock (Eq, Show)
  deriving newtype (Class.FromNBT, Class.ToNBT)

newtype BlockLight = BlockLight (PackedVector ('Static 4096) ('Static 4) Int)
  deriving stock (Eq, Show)

newtype SkyLight   = SkyLight   (PackedVector ('Static 4096) ('Static 4) Int)
  deriving stock (Eq, Show)

instance HasCodec ChunkSection where
  codec = compound "Chunk section" $ [|| ChunkSection ||]
    <$#> requiredField "Y" .= [|| sectionY ||]
    <*#> optionalField "block_states" .= [|| sectionBlocks ||]
    <*#> optionalField "biomes" .= [|| sectionBiomes ||]
    <*#> optionalField "BlockLight" .= [|| sectionBlockLight ||]
    <*#> optionalField "SkyLight" .= [|| sectionSkyLight ||]

instance HasCodec BlockStates where
  codec = compound "block_states" $ [|| \palette -> \case
      Nothing -> SingleBlockState (V.head palette)
      Just d -> case S.unsafeToForeignPtr0 d of
        (fp, nrOfWords) ->
          let sz = fromIntegral $ natVal (Proxy @SectionSize)
              elsPerWord = (sz + nrOfWords - 1) `quotInt` nrOfWords
              bitSz = 64 `quotInt` elsPerWord
          in FullBlockStates (BlockPalette palette) $ PV.unsafeFromForeignPtr @('Static SectionSize) @'Dynamic (coerce fp) bitSz
    ||]
    <$#> requiredField "palette" .= [|| \case
        SingleBlockState entry -> V.singleton entry
        FullBlockStates palette _ -> coerce palette
      ||]
    <*#> (optionalField @(S.Vector (BigEndian Int64)) "data" .= [|| \case
        SingleBlockState _ -> Nothing
        FullBlockStates _ pv ->
          let sz    = PV.length  pv
              bitSz = PV.bitSize pv
              elsPerWord = 64 `quotInt` bitSz
              nrOfWords = (sz + elsPerWord - 1) `quotInt` elsPerWord
          in Just $ S.unsafeFromForeignPtr0 (coerce $ PV.unsafeBacking pv) nrOfWords
      ||])

instance HasCodec Biomes where
  codec = compound "biomes" $ [|| \palette -> \case
      Nothing -> SingleBiome (V.head palette)
      Just d -> case S.unsafeToForeignPtr0 d of
        (fp, nrOfWords) ->
          let sz = fromIntegral $ natVal (Proxy @SectionBiomes)
              elsPerWord = (sz + nrOfWords - 1) `quotInt` nrOfWords
              bitSz = 64 `quotInt` elsPerWord
          in FullBiomes (BiomePalette palette) $ PV.unsafeFromForeignPtr @('Static SectionBiomes) @'Dynamic (coerce fp) bitSz
    ||]
    <$#> requiredFieldVia @(V.Vector NBTString) "palette" .= [|| \case
        SingleBiome entry -> V.singleton entry
        FullBiomes palette _ -> coerce palette
      ||]
    <*#> (optionalField @(S.Vector (BigEndian Int64)) "data" .= [|| \case
        SingleBiome _ -> Nothing
        FullBiomes _ pv ->
          let sz    = PV.length  pv
              bitSz = PV.bitSize pv
              elsPerWord = 64 `quotInt` bitSz
              nrOfWords = (sz + elsPerWord - 1) `quotInt` elsPerWord
          in Just $ S.unsafeFromForeignPtr0 (coerce $ PV.unsafeBacking pv) nrOfWords
      ||])

instance HasCodec BlockPaletteEntry where
  codec = compound "Block palette entry" $ [|| BlockPaletteEntry ||]
    <$#> requiredField "Name" .= [|| blockName ||]
    <*#> optionalField "Properties" .= [|| blockProperties ||]

instance HasCodec BlockLight where
  codec = dimapCodec
    [|| \v ->
      let (fp, _) = S.unsafeToForeignPtr0 v
      in BlockLight $ PV.unsafeFromForeignPtr @('Static 4096) @('Static 4) (coerce fp)
      ||]
    [|| \(BlockLight pv) -> S.unsafeFromForeignPtr0 (coerce $ PV.unsafeBacking pv) $ 4096 `quot` (2 * 8) ||]
    $ codec @(S.Vector Int8)

instance HasCodec SkyLight where
  codec = dimapCodec
    [|| \v ->
      let (fp, _) = S.unsafeToForeignPtr0 v
      in SkyLight $ PV.unsafeFromForeignPtr @('Static 4096) @('Static 4) (coerce fp)
      ||]
    [|| \(SkyLight pv) -> S.unsafeFromForeignPtr0 (coerce $ PV.unsafeBacking pv) $ 4096 `quot` (2 * 8) ||]
    $ codec @(S.Vector Int8)

instance Class.ToNBT ChunkSection where
  toNBT ChunkSection{..} =
    Class.compound $ [
        "Y" Class..= sectionY
      ] <> catMaybes [
        ("BlockLight" Class..=) <$> sectionBlockLight
      , ("SkyLight" Class..=) <$> sectionSkyLight
      , ("block_states" Class..=) <$> sectionBlocks
      , ("biomes" Class..=) <$> sectionBiomes
      ]

instance Class.FromNBT ChunkSection where
  fromNBT _ = Class.withCompound "ChunkSection" $ \c -> do
    sectionY <- c Class..: "Y"
    sectionBlocks <- c Class..:? "block_states"
    sectionBiomes <- c Class..:? "biomes"
    sectionBlockLight <- c Class..:? "BlockLight"
    sectionSkyLight   <- c Class..:? "SkyLight"
    pure ChunkSection{..}

instance Class.ToNBT BlockStates where
  toNBT (SingleBlockState x) =
    Class.compound [ "palette" Class..= V.singleton x ]
  toNBT (FullBlockStates palette pv) =
    let sz    = PV.length  pv
        bitSz = PV.bitSize pv
        elsPerWord = 64 `quotInt` bitSz
        nrOfWords = (sz + elsPerWord - 1) `quotInt` elsPerWord
        vec = S.unsafeFromForeignPtr0 @(BigEndian Int64) (coerce $ PV.unsafeBacking pv) nrOfWords
    in Class.compound [ "palette" Class..= palette, "data" Class..= vec ]

instance Class.FromNBT BlockStates where
  fromNBT _ = Class.withCompound "block_states" $ \c -> do
    palette <- c Class..: "palette"
    c Class..:? "data" >>= \case
      Nothing -> pure . SingleBlockState $ V.head palette
      Just v  -> case S.unsafeToForeignPtr0 @(BigEndian Int64) v of
        (fp, nrOfWords) ->
          let sz = fromIntegral $ natVal (Proxy @SectionSize)
              elsPerWord = (sz + nrOfWords - 1) `quotInt` nrOfWords
              bitSz = 64 `quotInt` elsPerWord
          in pure $ FullBlockStates (BlockPalette palette) $ PV.unsafeFromForeignPtr @('Static SectionSize) @'Dynamic (coerce fp) bitSz
    
instance Class.ToNBT BlockPaletteEntry where
  toNBT BlockPaletteEntry{..} =
    Class.compound $ [ "Name" Class..= blockName ] <> case blockProperties of
      Nothing -> []
      Just x -> [ "Properties" Class..= x ]

instance Class.FromNBT BlockPaletteEntry where
  fromNBT _ = Class.withCompound "Palette entry" $ \c -> do
    blockName <- c Class..: "Name"
    blockProperties <- c Class..:? "Properties"
    pure BlockPaletteEntry{..}

instance Class.ToNBT Biomes where
  toNBT (SingleBiome x) =
    Class.compound [ "palette" Class..= V.singleton x ]
  toNBT (FullBiomes palette pv) =
    let sz    = PV.length  pv
        bitSz = PV.bitSize pv
        elsPerWord = 64 `quotInt` bitSz
        nrOfWords = (sz + elsPerWord - 1) `quotInt` elsPerWord
        vec = S.unsafeFromForeignPtr0 @(BigEndian Int64) (coerce $ PV.unsafeBacking pv) nrOfWords
    in Class.compound [ "palette" Class..= palette, "data" Class..= vec ]

instance Class.FromNBT Biomes where
  fromNBT _ = Class.withCompound "biomes" $ \c -> do
    palette <- c Class..: "palette"
    c Class..:? "data" >>= \case
      Nothing -> pure . SingleBiome $ V.head palette
      Just v  -> case S.unsafeToForeignPtr0 @(BigEndian Int64) v of
        (fp, nrOfWords) ->
          let sz = fromIntegral $ natVal (Proxy @SectionBiomes)
              elsPerWord = (sz + nrOfWords - 1) `quotInt` nrOfWords
              bitSz = 64 `quotInt` elsPerWord
          in pure $ FullBiomes (BiomePalette palette) $ PV.unsafeFromForeignPtr @('Static SectionBiomes) @'Dynamic (coerce fp) bitSz

instance Class.ToNBT BlockLight where
  toNBT (BlockLight pv) =
    Class.toNBT . S.unsafeFromForeignPtr0 @Int8 (coerce $ PV.unsafeBacking pv) $ 4096 `quot` (2 * 8)

instance Class.FromNBT BlockLight where
  fromNBT nm t = Class.fromNBT nm t <&> \v ->
      let (fp, _) = S.unsafeToForeignPtr0 @Int8 v
      in BlockLight $ PV.unsafeFromForeignPtr @('Static 4096) @('Static 4) (coerce fp)

instance Class.ToNBT SkyLight where
  toNBT (SkyLight pv) =
    Class.toNBT . S.unsafeFromForeignPtr0 @Int8 (coerce $ PV.unsafeBacking pv) $ 4096 `quot` (2 * 8)

instance Class.FromNBT SkyLight where
  fromNBT nm t = Class.fromNBT nm t <&> \v ->
      let (fp, _) = S.unsafeToForeignPtr0 @Int8 v
      in SkyLight $ PV.unsafeFromForeignPtr @('Static 4096) @('Static 4) (coerce fp)
