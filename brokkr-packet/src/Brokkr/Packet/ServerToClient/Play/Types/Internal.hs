{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
module Brokkr.Packet.ServerToClient.Play.Types.Internal (
  Angle(..)
, Dismount(..)
, SpawnAngle(..)
, Hardcore(..)
, GameMode(..)
, PreviousGameMode(..)
, DimensionType(..), mkDimensionType
, HashedSeed(..)
, MaxPlayers(..)
, ViewDistance(..)
, SimulationDistance(..)
, ReducedDebugInfo(..)
, EnableRespawnScreen(..)
, IsDebug(..)
, IsFlat(..)
, DeathLocation(..)
, HeightMaps(..)
, ToHeightMapSize
, HeightMap(..)
, SectionData(..)
, Section(..)
, BlockCount(..)
, BlockStates(..)
, Biomes(..)
, PalettedVector(..)
, Palette(..)
, BlockEntity(..)
, TrustEdges(..)
, LightData(..)
, SkyLight(..)
, BlockLight(..)
, module Brokkr.Packet.ServerToClient.Play.Types.Codec
, PosBitField(..)
, pattern PosBitField
, PlayerInfoUpdates(..)
, pattern InfoAddPlayer
, pattern InfoGameMode
, pattern InfoListed
, combinePlayerInfo
, PlayerInfoRemoves(..)
) where

-- TODO
import Brokkr.NBT
import Brokkr.NBT.Internal -- TODO
import Brokkr.NBT.Slice qualified as Slice -- TODO
import Brokkr.NBT.Codec

import Brokkr.PackedVector.Internal (PackedVector(..), DynamicNat(..), unsafeFromForeignPtr)

import Brokkr.Packet.Binary
import Brokkr.Packet.SizePrefixed
import Brokkr.Packet.Common.Internal
import Brokkr.Packet.ServerToClient.Play.Types.Codec

import Brokkr.Registry.BiomeSettings as Biome
import Brokkr.Registry.Dimension as Dimension

import Data.Bits

import Data.ByteString.Internal qualified as BS

import Data.Coerce
import Data.Int
import Data.Text (Text)
import Data.Proxy
import Data.Word
import Data.Vector qualified as V
import Data.Vector.Unboxed qualified as U
import Data.Vector.Storable qualified as S

import FlatParse.Basic qualified as Flatparse

import GHC.Base (quotInt)
import GHC.Exts
import GHC.Generics (Generic)
import GHC.Int (Int8(..))
import GHC.TypeLits

import Unsafe.Coerce qualified as Unsafe (unsafeCoerce)
import qualified FlatParse.Basic as FP
import Data.Type.Bool
import Data.Monoid
import Data.Void (Void)

newtype Angle = Angle Word8
  deriving stock Show
  deriving newtype Eq
  deriving newtype (ToBinary, FromBinary)

data Dismount = NoDismount | Dismount
  deriving stock (Eq, Show, Generic)

instance ToBinary Dismount where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary Dismount where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

newtype SpawnAngle = SpawnAngle Float
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

data Hardcore = NotHardcore | Hardcore
  deriving stock (Eq, Show, Generic)

instance ToBinary Hardcore where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary Hardcore where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data GameMode = Survival | Creative | Adventure | Spectator
  deriving stock (Eq, Show, Generic)

instance ToBinary GameMode where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary GameMode where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data PreviousGameMode = PreviousGameModeUndefined | PreviousGameModeKnown !GameMode
  deriving stock Eq

instance ToBinary PreviousGameMode where
  put PreviousGameModeUndefined = put @Int8 (-1)
  put (PreviousGameModeKnown gm) = put gm
  {-# INLINE put #-}

instance FromBinary PreviousGameMode where
  with f = with @Int8 $ \case
    (-1) -> f PreviousGameModeUndefined
    (I8# n) -> f $ PreviousGameModeKnown $ tagToEnum# (int8ToInt# n)
  {-# INLINE with #-}

instance Show PreviousGameMode where
  showsPrec _ PreviousGameModeUndefined    = showString "Undefined"
  showsPrec prec (PreviousGameModeKnown m) = showsPrec prec m

newtype DimensionType = UnsafeDimensionType Text
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via Identifier

mkDimensionType :: Text -> Maybe DimensionType
mkDimensionType = coerce . mkIdentifier

newtype HashedSeed = HashedSeed Int64
  deriving stock Show
  deriving newtype Eq
  deriving newtype (ToBinary, FromBinary)

newtype MaxPlayers = MaxPlayers Int32
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via VarInt

newtype ViewDistance = ViewDistance Int32
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via VarInt

newtype SimulationDistance = SimulationDistance Int32
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via VarInt

data ReducedDebugInfo = NoReducedDebugInfo | ReducedDebugInfo
  deriving stock (Eq, Show, Generic)

instance ToBinary ReducedDebugInfo where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary ReducedDebugInfo where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data EnableRespawnScreen = DisableRespawnScreen | EnableRespawnScreen
  deriving stock (Eq, Show, Generic)

instance ToBinary EnableRespawnScreen where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary EnableRespawnScreen where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data IsDebug = IsNotDebug | IsDebug
  deriving stock (Eq, Show, Generic)

instance ToBinary IsDebug where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary IsDebug where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data IsFlat = IsNotFlat | IsFlat
  deriving stock (Eq, Show, Generic)

instance ToBinary IsFlat where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary IsFlat where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data DeathLocation = DeathLocation !DimensionName !Position
  deriving stock (Eq, Show)

instance ToBinary DeathLocation where
  put (DeathLocation dm loc) = put dm <> put loc
  {-# INLINE put #-}

instance FromBinary DeathLocation where
  with f = with $ \nm -> with $ \loc -> f (DeathLocation nm loc)
  {-# INLINE with #-}

newtype HeightMaps (dimHeight :: Nat) = HeightMaps {
  motionBlocking :: Maybe (HeightMap dimHeight)
}
  deriving stock Eq

deriving stock instance KnownNat (ToHeightMapSize dimHeight) => Show (HeightMaps dimHeight)

type ToHeightMapSize dimHeight = Log2 (dimHeight + (2 ^ Log2 dimHeight) - 1)

newtype HeightMap (dimHeight :: Nat) = HeightMap (PackedVector ('Static 256) ('Static (ToHeightMapSize dimHeight)) Int)
  deriving newtype Eq

deriving stock instance KnownNat (ToHeightMapSize dimHeight) => Show (HeightMap dimHeight)

instance KnownNat (ToHeightMapSize dimHeight) => ToBinary (HeightMaps dimHeight) where
  put HeightMaps{motionBlocking} =
    put $ NBT "" (TagCompound $ Slice.fromList $ maybe [] (\(HeightMap pv) -> [NBT "MOTION_BLOCKING" $ TagLongArray $ toVec pv]) motionBlocking)
    where
      toVec pv =
        let elsPerWord = 64 `quotInt` fromIntegral (natVal (Proxy @(ToHeightMapSize dimHeight)))
            nrOfWords = (256 + elsPerWord - 1) `quot` elsPerWord
        in  S.unsafeFromForeignPtr0 (coerce pv) nrOfWords
  {-# INLINE put #-}

instance KnownNat (ToHeightMapSize dimHeight) => FromBinary (HeightMaps dimHeight) where
  get = get >>= \(NBT _ t) -> case t of
    TagCompound sl -> case Slice.findWithIndex (\(NBT tag _) -> tag) "MOTION_BLOCKING" sl of
      (# _, (# | _ #) #) -> pure $ HeightMaps { motionBlocking = Nothing }
      (# _, (# NBT _ (TagLongArray v) | #) #) ->
        let (mbFp, _) = S.unsafeToForeignPtr0 v
        in pure $ HeightMaps {
          motionBlocking = Just . HeightMap $ unsafeFromForeignPtr @('Static 256) @('Static (ToHeightMapSize dimHeight)) (coerce mbFp)
        }
      (# _, (# _ | #) #) -> Flatparse.err . InvalidNBT $ InvalidType "LongArray"
    _ -> Flatparse.err . InvalidNBT $ InvalidType "Compound"

-- | Invariant: the vector with the sections has exactly (Div dimHeight 16) elements
data SectionData (dimHeight :: Nat) = SectionData !Int32 !(V.Vector Section)
  deriving stock (Eq, Show)

instance ToBinary (SectionData dimHeight) where
  put (SectionData sz arr) = put (VarInt sz) <> put arr
  {-# INLINE put #-}

instance (KnownNat (Div dimHeight 16)) => FromBinary (SectionData dimHeight) where
  get = with @VarInt $ \n ->
    SectionData (fromIntegral n) <$> Flatparse.isolate (fromIntegral n)
      (V.replicateM (fromIntegral (natVal (Proxy @(Div dimHeight 16)))) get >>= \v -> Flatparse.takeRest >> pure v)

data Section = Section !BlockCount !BlockStates !Biomes
  deriving stock (Eq, Show)

instance ToBinary Section where
  put (Section bc bs bi) = put bc <> put bs <> put bi
  {-# INLINE put #-}

instance FromBinary Section where
  get = Section <$> get <*> get <*> get

newtype BlockCount = BlockCount Int16
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

newtype BlockStates = BlockStates (PalettedVector 4096 15)
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

newtype Biomes = Biomes (PalettedVector 64 6)
  deriving stock Show
  deriving newtype (Eq, ToBinary, FromBinary)

data PalettedVector (size :: Nat) (maxBitSize :: Nat) =
    Direct !Int32
  | Indirect !Palette !(PackedVector ('Static size) 'Dynamic Int)
  | Global !(PackedVector ('Static size) ('Static maxBitSize) Int)

deriving stock instance (KnownNat sz, KnownNat mBitSz, KnownNat (Div 64 mBitSz)) => Show (PalettedVector sz mBitSz)
deriving stock instance (KnownNat sz, KnownNat mBitSz, KnownNat (Div 64 mBitSz)) => Eq (PalettedVector sz mBitSz)

instance (KnownNat sz, KnownNat maxBitSz) => ToBinary (PalettedVector sz maxBitSz) where
  put (Direct val) =
    put @Int8 0 <> put (VarInt val) <> put @VarInt 0 -- TODO Is the second 0 for no data correct?
  put (Indirect p (PVec_SD bitSz elsPerWord _ _ _ fp)) =
    put @Int8 (fromIntegral bitSz) <> put p <> put (VarInt $ fromIntegral nrOfWords) <> put (S.unsafeFromForeignPtr0 @Int64 (coerce fp) nrOfWords)
    where nrOfWords = (fromIntegral (natVal (Proxy @sz)) + elsPerWord - 1) `quot` elsPerWord
  put (Global pv) = put @Int8 (fromIntegral maxBitSz) <> put @VarInt (fromIntegral nrOfWords) <> put pv
    where
      maxBitSz :: Int = fromIntegral $ natVal (Proxy @maxBitSz)
      elsPerWord = 64 `quot` maxBitSz
      nrOfWords :: Int = (fromIntegral (natVal (Proxy @sz)) + elsPerWord - 1) `quot` elsPerWord
  {-# INLINE put #-}

instance (KnownNat sz, KnownNat maxBitSz) => FromBinary (PalettedVector sz maxBitSz) where
  get = with @VarInt $ \n ->
    if | n == 0
        -> with @VarInt $ \v -> Direct (coerce v) <$ get @VarInt
       | n == fromIntegral (natVal (Proxy @maxBitSz))
        -> Global <$> get
       | otherwise
        -> do
          palette <- coerce <$> get @(SizePrefixed VarInt (U.Vector Int32))
          _nrOfLongs <- get @VarInt
          let elsPerWord = 64 `quot` fromIntegral n
              sz = fromIntegral (natVal (Proxy @sz))
              nrOfWords = (sz + elsPerWord - 1) `quot` elsPerWord
              !(I# byteSz) = nrOfWords * 8
          BS.BS fp _ <- Flatparse.take# byteSz
          pure . Indirect palette $ unsafeFromForeignPtr @('Static sz) @'Dynamic (coerce fp) (fromIntegral n)

newtype Palette = Palette (U.Vector Int32)
  deriving stock (Eq, Show)

instance ToBinary Palette where
  put (Palette p) = put (VarInt $ fromIntegral $ U.length p) <> U.foldMap (\a -> put (VarInt a)) p
  {-# INLINE put #-}

data BlockEntity = BlockEntity {
  blockEntityX :: !Int8
, blockEntityY :: !Int16
, blockEntityZ :: !Int8
, blockEntityType :: !Int32
, blockEntityData :: !NBT
}
  deriving stock (Eq, Show)

instance ToBinary BlockEntity where
  put BlockEntity{..} =
       put (blockEntityX `unsafeShiftL` 4 .|. blockEntityZ)
    <> put blockEntityY
    <> put blockEntityType
    <> put blockEntityData
  {-# INLINE put #-}

instance FromBinary BlockEntity where
  get = BlockEntity <$> get <*> get <*> get <*> get <*> get
  {-# INLINE get #-}

data TrustEdges = DistrustEdges | TrustEdges
  deriving stock (Eq, Show, Generic)

instance ToBinary TrustEdges where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary TrustEdges where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data LightData = LightData {
  skyLightMask        :: !BitSet
, blockLightMask      :: !BitSet
, emptySkyLightMask   :: !BitSet
, emptyBlockLightMask :: !BitSet
, skyLight            :: !(V.Vector SkyLight)
, blockLight          :: !(V.Vector BlockLight)
}
  deriving stock (Eq, Show)

instance ToBinary LightData where
  put LightData{..} =
       put skyLightMask
    <> put blockLightMask
    <> put emptySkyLightMask
    <> put emptyBlockLightMask
    <> put (SizePrefixed @VarInt skyLight)
    <> put (SizePrefixed @VarInt blockLight)
  {-# INLINE put #-}

instance FromBinary LightData where
  get = LightData <$> get <*> get <*> get <*> get
    <*> (coerce @(SizePrefixed VarInt (V.Vector SkyLight)) <$> get)
    <*> (coerce @(SizePrefixed VarInt (V.Vector BlockLight)) <$> get)

newtype SkyLight = SkyLight (PackedVector ('Static 4096) ('Static 4) Int)
  deriving stock Show
  deriving newtype Eq

instance ToBinary SkyLight where
  put (SkyLight pvec) = put (VarInt 2048) <> put pvec
  {-# INLINE put #-}

instance FromBinary SkyLight where
  with f = with @VarInt $ \_ -> with (f . SkyLight )
  {-# INLINE with #-}

newtype BlockLight = BlockLight (PackedVector ('Static 4096) ('Static 4) Int)
  deriving stock Show
  deriving newtype Eq

instance ToBinary BlockLight where
  put (BlockLight pvec) = put (VarInt 2048) <> put pvec
  {-# INLINE put #-}

instance FromBinary BlockLight where
  with f = with @VarInt $ \_ -> with (f . BlockLight )
  {-# INLINE with #-}

instance ToBinary RegistryCodec where
  put = $(genBuilder $ codec @RegistryCodec)

instance FromBinary RegistryCodec where
  get = Flatparse.ParserT $ \fp eob s st ->
    case $$(genParser $ codec @RegistryCodec) of
      Flatparse.ParserT g -> case g fp eob s st of
        Flatparse.OK# st' res s' -> Flatparse.OK# st' res s'
        Flatparse.Fail# st' -> Flatparse.Fail# st'
        Flatparse.Err# st' e -> Flatparse.Err# st' $ InvalidNBT e

newtype PosBitField = UnsafePosBitField Int8
  deriving newtype (Eq, FromBinary, ToBinary)

instance Semigroup PosBitField where
  UnsafePosBitField l <> UnsafePosBitField r = UnsafePosBitField $ l .|. r

instance Monoid PosBitField where
  mempty = UnsafePosBitField 0

pattern PosBitField :: Bool -> Bool -> Bool -> Bool -> Bool -> PosBitField
pattern PosBitField xRel yRel zRel yawRel pitchRel <- ((\(UnsafePosBitField b) -> (
    (testBit b 1, testBit b 2, testBit b 4, testBit b 8, testBit b 16)
  )) -> (xRel, yRel, zRel, yawRel, pitchRel))
  where
    PosBitField xRel yRel zRel yawRel pitchRel = UnsafePosBitField $
          boolBit xRel 1
      .|. boolBit yRel 2
      .|. boolBit zRel 4
      .|. boolBit yawRel 8
      .|. boolBit pitchRel 16
{-# COMPLETE PosBitField #-} -- This is technically not legal. TODO Check on decoding!

boolBit :: Bits a => Bool -> Int -> a
boolBit True  = bit
boolBit False = const zeroBits

instance Show PosBitField where
  showsPrec prec (PosBitField xRel yRel zRel yawRel pitchRel) = showParen (prec > 10) $
      showString "PosBitField { "
    . showString "xRel = " . showsPrec 11 xRel
    . showString ", yRel = " . showsPrec 11 yRel
    . showString ", zRel = " . showsPrec 11 zRel
    . showString ", yawRel = " . showsPrec 11 yawRel
    . showString ", pitchRel = " . showsPrec 11 pitchRel
    . showString " }"

data PlayerInfoUpdates =
    forall hasAdd hasInitChat hasGameMode hasListed hasLatency hasDisplayName
  . (KnownPlayerInfoMask hasAdd hasInitChat hasGameMode hasListed hasLatency hasDisplayName)
  => PlayerInfoUpdates (V.Vector (UUID, PlayerInfo hasAdd hasInitChat hasGameMode hasListed hasLatency hasDisplayName))

class KnownPlayerInfoMask (hasAdd :: Bool) (hasInitChat :: Bool) (hasGameMode :: Bool) (hasListed :: Bool) (hasLatency :: Bool) (hasDisplayName :: Bool) where
  playerInfoMask :: Word8

class KnownBool (b :: Bool) where bool' :: a -> a -> a
instance KnownBool True where bool' l _ = l
instance KnownBool False where bool' _ r = r

instance
  ( KnownBool hasAdd
  , KnownBool hasInitChat
  , KnownBool hasGameMode
  , KnownBool hasListed
  , KnownBool hasLatency
  , KnownBool hasDisplayName
  ) => KnownPlayerInfoMask hasAdd hasInitChat hasGameMode hasListed hasLatency hasDisplayName where
    playerInfoMask =
          bool' @hasAdd         (bit 0) 0
      .|. bool' @hasInitChat    (bit 1) 0
      .|. bool' @hasGameMode    (bit 2) 0
      .|. bool' @hasListed      (bit 3) 0
      .|. bool' @hasLatency     (bit 4) 0
      .|. bool' @hasDisplayName (bit 5) 0

instance Eq PlayerInfoUpdates where
  PlayerInfoUpdates @l1 @l2 @l3 @l4 @l5 @l6 lv == PlayerInfoUpdates @r1 @r2 @r3 @r4 @r5 @r6 rv
    =    playerInfoMask @l1 @l2 @l3 @l4 @l5 @l6 == playerInfoMask @r1 @r2 @r3 @r4 @r5 @r6
      && lv == Unsafe.unsafeCoerce rv

instance Show PlayerInfoUpdates where
  show (PlayerInfoUpdates v) = show v

instance ToBinary PlayerInfoUpdates where
  put (PlayerInfoUpdates @l1 @l2 @l3 @l4 @l5 @l6 vec) =
       put (playerInfoMask @l1 @l2 @l3 @l4 @l5 @l6)
    <> put @VarInt (fromIntegral $ V.length vec)
    <> V.foldMap (\(uid, info) -> put uid <> put info) vec

instance FromBinary PlayerInfoUpdates where
  get = do
    mask <- get @Word8
    VarInt sz <- get
    let getOne :: forall l1 l2 l3 l4 l5 l6 st . FP.ParserT st PacketParseError (PlayerInfo l1 l2 l3 l4 l5 l6)
        getOne = UnsafePlayerInfo <$> getMasked 0 <*> getMasked 1 <*> getMasked 2 <*> getMasked 3 <*> getMasked 4 <*> getMasked 5
        getMasked :: FromBinary a => Int -> FP.ParserT st PacketParseError (Maybe a)
        getMasked n
          | testBit mask n = Just <$> get
          | otherwise = pure Nothing
        -- TODO Why does ghc think these are unused? This errors without them because withDict needs them
        mkPlayerInfoUpdates
          :: forall (l1 :: Bool) (l2 :: Bool) (l3 :: Bool) (l4 :: Bool) (l5 :: Bool) (l6 :: Bool) st . FP.ParserT st PacketParseError PlayerInfoUpdates
        mkPlayerInfoUpdates =
          withDict @(KnownPlayerInfoMask l1 l2 l3 l4 l5 l6) mask $
            PlayerInfoUpdates <$> V.replicateM (fromIntegral sz) ((,) <$> get <*> getOne @l1 @l2 @l3 @l4 @l5 @l6)
    mkPlayerInfoUpdates

data PlayerInfo (hasAdd :: Bool) (hasInitChat :: Bool) (hasGameMode :: Bool) (hasListed :: Bool) (hasLatency :: Bool) (hasDisplayName :: Bool)
  = UnsafePlayerInfo
    (Maybe Username)
    (Maybe Void)
    (Maybe GameMode)
    (Maybe Bool)
    (Maybe Void)
    (Maybe Void)
  deriving stock (Eq, Show)

pattern InfoAddPlayer :: Username -> PlayerInfo True False False False False False
pattern InfoAddPlayer x <- UnsafePlayerInfo (Just x) _ _ _ _ _
  where InfoAddPlayer x = UnsafePlayerInfo (Just x) Nothing Nothing Nothing Nothing Nothing

pattern InfoGameMode :: GameMode -> PlayerInfo False False True False False False
pattern InfoGameMode x <- UnsafePlayerInfo _ _ (Just x) _ _ _
  where InfoGameMode x = UnsafePlayerInfo Nothing Nothing (Just x) Nothing Nothing Nothing

pattern InfoListed :: Bool -> PlayerInfo False False False True False False
pattern InfoListed x <- UnsafePlayerInfo _ _ _ (Just x) _ _
  where InfoListed x = UnsafePlayerInfo Nothing Nothing Nothing (Just x) Nothing Nothing

combinePlayerInfo :: PlayerInfo l1 l2 l3 l4 l5 l6 -> PlayerInfo r1 r2 r3 r4 r5 r6 -> PlayerInfo (l1 || r1) (l2 || r2) (l3 || r3) (l4 || r4) (l5 || r5) (l6 || r6)
combinePlayerInfo (UnsafePlayerInfo l1 l2 l3 l4 l5 l6) (UnsafePlayerInfo r1 r2 r3 r4 r5 r6)
  = UnsafePlayerInfo
    (getLast $ Last l1 <> Last r1)
    (getLast $ Last l2 <> Last r2)
    (getLast $ Last l3 <> Last r3)
    (getLast $ Last l4 <> Last r4)
    (getLast $ Last l5 <> Last r5)
    (getLast $ Last l6 <> Last r6)

instance ToBinary (PlayerInfo hasAdd hasInitChat hasGameMode hasListed hasLatency hasDisplayName) where
  put (UnsafePlayerInfo a b c d e f) =
       maybe mempty (\x -> put x <> put (0 :: Word8)) a
    <> maybe mempty (\x -> put x) b
    <> maybe mempty (\x -> put x) c
    <> maybe mempty (\x -> put x) d
    <> maybe mempty (\x -> put x) e
    <> maybe mempty (\x -> put x) f

newtype PlayerInfoRemoves = PlayerInfoRemoves (V.Vector UUID)
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via SizePrefixed VarInt (V.Vector UUID)
