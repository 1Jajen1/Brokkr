{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiWayIf #-}
--{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
{-# LANGUAGE DeriveAnyClass #-}
module Chunk.Section (
  ChunkSection(..)
, PalettedVector(..)
, emptySection
, BlockStates(..)
, Biomes(..)
) where

import Util.Vector.Packed (PackedVector, DynamicNat(..))
import qualified Util.Vector.Packed as P

import GHC.TypeLits
import Util.NBT.Internal
import Data.Int
import qualified Data.Vector.Storable as S
import Data.Coerce (coerce)
import qualified Data.Vector as V
import Block.Internal.BlockState
import Block.Internal.Conversion
import qualified Data.HashMap.Lazy as HM
import GHC.Generics (Generic)
import Control.DeepSeq
import Util.Binary
import Data.Proxy
import Network.Util.VarNum
import Control.Monad.ST (runST)
import qualified Data.Text as T
import Data.Maybe (fromMaybe)
import Registry.Biome
import Unsafe.Coerce (unsafeCoerce)
import qualified Data.ByteString as BS
import qualified Data.Text.Encoding as T
import Data.List (sortOn)

type NibbleVector = PackedVector ('Static 4096) ('Static 4)

-- TODO Try and compress blocks/light by using octtrees, maybe just if the palette is small as a heuristic (compress them, calc byte size and compare to arr version)
-- if its smaller use the octtree otherwise stick with the array. 
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

-- TODO Generate at compile time since 1 + Log2 is not actually correct if the HighestBlockStateId is a perfect power of 2 
type BlockPaletteMaxBitsize = 1 + Log2 HighestBlockStateId 
type BiomePaletteMaxBitsze  = 6

type SectionSize = 4096
type BiomeSectionSize = 64

newtype BlockStates = BlockStates (PalettedVector SectionSize BlockPaletteMaxBitsize)
  deriving stock Show
  deriving newtype (NFData, ToBinary)

newtype Biomes = Biomes (PalettedVector BiomeSectionSize BiomePaletteMaxBitsze)
  deriving stock Show
  deriving newtype (NFData, ToBinary)

data PalettedVector (sz :: Nat) (globalBitSz :: Nat) =
    Global      {-# UNPACK #-} !(PackedVector ('Static sz) ('Static globalBitSz))
  | SingleValue {-# UNPACK #-} !Int
  | Indirect    {-# UNPACK #-} !(S.Vector Int) {-# UNPACK #-} !(PackedVector ('Static sz) 'Dynamic)

instance (KnownNat sz, KnownNat mBSz) => Show (PalettedVector sz mBSz) where
  show (Global v)      = "Global " <> show v
  show (SingleValue v) = "Single " <> show v
  show (Indirect p v)  = "Indirect " <> show p <> show v 

instance NFData (PalettedVector sz maxBitSz) where
  rnf (Global vec) = rnf vec
  rnf (SingleValue w) = rnf w
  rnf (Indirect p vec) = rnf p `seq` rnf vec

instance (KnownNat sz, KnownNat mBSz) => ToBinary (PalettedVector sz mBSz) where
  put (Global v) =
       put (fromIntegral @_ @Int8 bitSz)
    <> put (VarInt $ fromIntegral numInt64)
    <> put v
    where
      bitSz = fromIntegral $ natVal (Proxy @BlockPaletteMaxBitsize)
      vLen = runST $ P.length <$> P.unsafeThaw v
      numInt64 = P.nrWords bitSz vLen
  put (SingleValue v) = put (0 :: Int8) <> put (VarInt $ fromIntegral v) <> put (VarInt 0) -- No data
  put (Indirect p v) =
       put (fromIntegral @_ @Int8 bitSz)
    <> put (VarInt . fromIntegral $ S.length p)
    <> S.foldMap (\x -> put $ VarInt $ fromIntegral x) p
    <> put (VarInt $ fromIntegral numInt64)
    <> put v
    where
      bitSz = runST $ P.bitSz <$> P.unsafeThaw v
      vLen = runST $ P.length <$> P.unsafeThaw v
      numInt64 = P.nrWords bitSz vLen
  {-# INLINE put #-}

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
              let !intPalette = S.generate (V.length palette) $ (coerce . (V.!) palette) 
              in pure . BlockStates . Indirect intPalette . P.unsafeDynamicFromForeignPtr bitsPerVal $ coerce blockStates
            | otherwise -> pure . BlockStates . Global . P.unsafeStaticFromForeignPtr $ coerce blockStates
    where
      -- TODO This can further be optimized:
      -- Currently this does a lookup into a ~22k sorted array based on the hash of (BlockName, BlockProps)
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

instance FromNBT Biomes where
  parseNBT = withCompound $ \obj -> do
    !intPalette <- lookupBiomes <$> obj .: "palette"

    if | S.length intPalette == 1 -> pure . Biomes . SingleValue $ S.unsafeHead intPalette 
       | otherwise -> do
         (biomes, len) <- S.unsafeToForeignPtr0 @Int64 <$> obj .: "data"
         let bitsPerVal = if len == 4 then 3 else len -- TODO
         if | bitsPerVal < 4 -> pure . Biomes . Indirect intPalette . P.unsafeDynamicFromForeignPtr bitsPerVal $ coerce biomes
            | otherwise -> pure . Biomes . Global . P.unsafeStaticFromForeignPtr $ coerce biomes
    where
      lookupBiomes v = S.generate (V.length v) $ \i -> lookupBiome (v V.! i)
      lookupBiome :: Tag -> Int
      lookupBiome (TagString name) = fromMaybe (error $ show name) $ HM.lookup (BS.drop 10 $ coerce name) biomeMap
      lookupBiome _ = error "WUT=!"
      {-# SCC lookupBiome #-}
  {-# INLINE parseNBT #-}
  {-# SCC parseNBT #-}

biomeMap :: HM.HashMap BS.ByteString Int
biomeMap = HM.fromList $ zip (T.encodeUtf8 . T.pack . fst <$> all_biome_settings) [0..]

countBlocks :: BlockStates -> Int
countBlocks (BlockStates (SingleValue val))
  | isAir $ BlockState val = 0
  | otherwise              = fromIntegral $ natVal @SectionSize undefined
countBlocks (BlockStates (Global vec)) 
  = sz - P.countElems (unsafeCoerce $ S.fromList $ coerce @_ @[Int] [Air, VoidAir, CaveAir]) vec
  where !sz = fromIntegral $ natVal @SectionSize undefined
countBlocks (BlockStates (Indirect palette vec))
  | S.null airs = sz
  -- | U.length airs == 1 = P.countElem (airs U.! 0) vec
  | otherwise = sz - P.countElems (unsafeCoerce airs) vec
  where
    !airs = S.findIndices (\x -> isAir $ BlockState x) palette
    !sz = fromIntegral $ natVal @SectionSize undefined
{-# SCC countBlocks #-}

isAir :: BlockState -> Bool
isAir Air     = True
isAir VoidAir = True
isAir CaveAir = True
isAir _       = False
{-# INLINE isAir #-}

emptySection :: Int -> ChunkSection
emptySection y = ChunkSection {
  y
, blockLight = Nothing
, skyLight   = Nothing
, blocks     = BlockStates $ SingleValue 0
, biomes     = Biomes $ SingleValue 0
, blockCount = 0
}
