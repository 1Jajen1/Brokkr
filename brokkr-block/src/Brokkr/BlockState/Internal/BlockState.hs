{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns -Wno-missing-export-lists -Wno-unused-local-binds #-}
--{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Brokkr.BlockState.Internal.BlockState where

import Brokkr.BlockState.Internal.TH
import Data.Coerce
import GHC.TypeLits

newtype BlockState = BlockState Int
  deriving newtype (Eq, Show) -- TODO Generate better show instance

-- Aux types TODO Autogen some of these. Especially the boolean equivalents
-- Or at least autogent the instances, that should not be hard!

class ToId a where
  toId :: a -> Int

class FromId a where
  fromId :: Int -> a

class Cardinality a where
  cardinality :: Int

data Facing (up :: Bool) (down :: Bool) where
  North :: Facing up down
  East  :: Facing up down
  South :: Facing up down
  West  :: Facing up down
  Up    :: Facing 'True down
  Down  :: Facing up 'True

deriving stock instance Show (Facing 'True  'True )
deriving stock instance Show (Facing 'False 'False)
deriving stock instance Show (Facing 'False 'True )
deriving stock instance Show (Facing 'True  'False)
deriving stock instance Eq (Facing 'True  'True )
deriving stock instance Eq (Facing 'False 'False)
deriving stock instance Eq (Facing 'False 'True )
deriving stock instance Eq (Facing 'True  'False)

instance FromId (Facing 'False 'False) where
  fromId 0 = North
  fromId 1 = South
  fromId 2 = West
  fromId 3 = East

instance FromId (Facing 'True 'True) where
  fromId 0 = North
  fromId 1 = South
  fromId 2 = West
  fromId 3 = East
  fromId 4 = Up
  fromId 5 = Down

instance FromId (Facing 'False 'True) where
  fromId 0 = Down
  fromId 1 = North
  fromId 2 = South
  fromId 3 = West
  fromId 4 = East

instance ToId (Facing 'False 'False) where
  toId North = 0
  toId East  = 1
  toId South = 2
  toId West  = 3

instance ToId (Facing 'True 'True) where
  toId North = 0
  toId East  = 1
  toId South = 2
  toId West  = 3
  toId Up    = 4
  toId Down  = 5

instance ToId (Facing 'False 'True) where
  toId North = 1
  toId East  = 2
  toId South = 3
  toId West  = 4
  toId Down  = 0

instance Cardinality (Facing 'False 'False) where
  cardinality = 4

instance Cardinality (Facing 'True 'False) where
  cardinality = 5
instance Cardinality (Facing 'False 'True) where
  cardinality = 5
instance Cardinality (Facing 'True 'True) where
  cardinality = 6

data Attached (up :: Bool) (down :: Bool) (side :: Facing up down) = Attached | NotAttached

instance FromId (Attached up down side) where
  fromId 0 = Attached
  fromId 1 = NotAttached

instance ToId (Attached up down side) where
  toId Attached    = 0
  toId NotAttached = 1

instance Cardinality (Attached up down side) where
  cardinality = 2

data WallAttached (side :: Facing 'False 'False) = Low | Tall | None

instance FromId (WallAttached side) where
  fromId 0 = None
  fromId 1 = Low
  fromId 2 = Tall

instance ToId (WallAttached side) where
  toId None = 0
  toId Low  = 1
  toId Tall = 2

instance Cardinality (WallAttached s) where
  cardinality = 3

data Waterlogged = Waterlogged | NotWaterlogged

instance FromId Waterlogged where
  fromId 0 = Waterlogged
  fromId 1 = NotWaterlogged

instance ToId Waterlogged where
  toId Waterlogged    = 0
  toId NotWaterlogged = 1

instance Cardinality Waterlogged where
  cardinality = 2

data Powered = Powered | NotPowered

instance FromId Powered where
  fromId 0 = Powered
  fromId 1 = NotPowered

instance ToId Powered where
  toId Powered    = 0
  toId NotPowered = 1

instance Cardinality Powered where
  cardinality = 2

data Face (bell :: Bool) where
  OnFloor      :: Face bell
  OnWall       :: Face 'False
  OnCeiling    :: Face bell
  OnSingleWall :: Face 'True
  OnDoubleWall :: Face 'True 

instance FromId (Face 'False) where
  fromId 0 = OnFloor
  fromId 1 = OnWall
  fromId 2 = OnCeiling

instance ToId (Face 'False) where
  toId OnFloor   = 0
  toId OnWall    = 1
  toId OnCeiling = 2

instance Cardinality (Face 'False) where
  cardinality = 3

instance FromId (Face 'True) where
  fromId 0 = OnFloor
  fromId 1 = OnCeiling
  fromId 2 = OnSingleWall
  fromId 3 = OnDoubleWall

instance ToId (Face 'True) where
  toId OnFloor      = 0
  toId OnCeiling    = 1
  toId OnSingleWall = 2
  toId OnDoubleWall = 3

instance Cardinality (Face 'True) where
  cardinality = 4

data SlabType = TopSlab | BottomSlab | DoubleSlab

instance FromId SlabType where
  fromId 0 = TopSlab
  fromId 1 = BottomSlab
  fromId 2 = DoubleSlab

instance ToId SlabType where
  toId TopSlab    = 0
  toId BottomSlab = 1
  toId DoubleSlab = 2

instance Cardinality SlabType where
  cardinality = 3

data Axis = XAxis | YAxis | ZAxis

instance FromId Axis where
  fromId 0 = XAxis
  fromId 1 = YAxis
  fromId 2 = ZAxis

instance ToId Axis where
  toId XAxis = 0
  toId YAxis = 1
  toId ZAxis = 2

instance Cardinality Axis where
  cardinality = 3

newtype GrowthStage (max :: Nat) = GrowthStage Int

instance FromId (GrowthStage max) where
  fromId = coerce -- We do no bounds checking here as these values always come from the pattern synonyms, we later only export smart constructors -- TODO Actually do that and add a better note somewhere

instance ToId (GrowthStage max) where
  toId = coerce

instance KnownNat max => Cardinality (GrowthStage max) where
  cardinality = fromIntegral $ natVal @max undefined + 1

data StairShape = Straight | InnerLeft | InnerRight | OuterLeft | OuterRight

instance FromId StairShape where
  fromId 0 = Straight
  fromId 1 = InnerLeft
  fromId 2 = InnerRight
  fromId 3 = OuterLeft
  fromId 4 = OuterRight

instance ToId StairShape where
  toId Straight   = 0
  toId InnerLeft  = 1
  toId InnerRight = 2
  toId OuterLeft  = 3
  toId OuterRight = 4

instance Cardinality StairShape where
  cardinality = 5

data Half = TopHalf | BottomHalf

instance FromId Half where
  fromId 0 = TopHalf
  fromId 1 = BottomHalf

instance ToId Half where
  toId TopHalf    = 0
  toId BottomHalf = 1

instance Cardinality Half where
  cardinality = 2

data BedPart = HeadPart | FootPart

instance FromId BedPart where
  fromId 0 = HeadPart
  fromId 1 = FootPart

instance ToId BedPart where
  toId HeadPart = 0
  toId FootPart = 1

instance Cardinality BedPart where
  cardinality = 2

data Occupied = Occupied | NotOccupied

instance FromId Occupied where
  fromId 0 = Occupied
  fromId 1 = NotOccupied

instance ToId Occupied where
  toId Occupied = 0
  toId NotOccupied = 1

instance Cardinality Occupied where
  cardinality = 2

data Persistent = Persistent | NotPersistent

instance FromId Persistent where
  fromId 0 = Persistent
  fromId 1 = NotPersistent

instance ToId Persistent where
  toId Persistent = 0
  toId NotPersistent = 1

instance Cardinality Persistent where
  cardinality = 2

newtype LeafDistance = LeafDistance Int

instance FromId LeafDistance where
  fromId n = LeafDistance $ n + 1

instance ToId LeafDistance where
  toId (LeafDistance n) = n - 1

instance Cardinality LeafDistance where
  cardinality = 7

newtype Age (max :: Nat) = Age Int

instance FromId (Age max) where
  fromId = coerce

instance ToId (Age max) where
  toId = coerce

instance KnownNat max => Cardinality (Age max) where
  cardinality = (fromIntegral $ natVal @max undefined) + 1

newtype HoneyLevel = HoneyLevel Int

instance FromId HoneyLevel where
  fromId = coerce

instance ToId HoneyLevel where
  toId = coerce

instance Cardinality HoneyLevel where
  cardinality = 6

data Lit = Lit | NotLit

instance FromId Lit where
  fromId 0 = Lit
  fromId 1 = NotLit

instance ToId Lit where
  toId Lit    = 0
  toId NotLit = 1

instance Cardinality Lit where
  cardinality = 2

data ChestType = SingleChest | LeftChest | RightChest

instance FromId ChestType where
  fromId 0 = SingleChest
  fromId 1 = LeftChest
  fromId 2 = RightChest

instance ToId ChestType where
  toId SingleChest = 0
  toId LeftChest   = 1
  toId RightChest  = 2

instance Cardinality ChestType where
  cardinality = 3

data Open = Open | Closed

instance FromId Open where
  fromId 0 = Open
  fromId 1 = Closed

instance ToId Open where
  toId Open   = 0
  toId Closed = 1

instance Cardinality Open where
  cardinality = 2

newtype FluidLevel (max :: Nat) = FluidLevel Int

instance FromId (FluidLevel max) where
  fromId = coerce

instance ToId (FluidLevel max) where
  toId = coerce

instance KnownNat max => Cardinality (FluidLevel max) where
  cardinality = (fromIntegral $ natVal @max undefined) + 1

data Conditional = Conditional | NotConditional

instance FromId Conditional where
  fromId 0 = Conditional
  fromId 1 = NotConditional

instance ToId Conditional where
  toId Conditional    = 0
  toId NotConditional = 1

instance Cardinality Conditional where
  cardinality = 2

newtype Candles = Candles Int

instance FromId Candles where
  fromId n = Candles $ n + 1

instance ToId Candles where
  toId (Candles n) = n - 1

instance Cardinality Candles where
  cardinality = 4

data MushroomExposed (side :: Facing 'True 'True) = Exposed | NotExposed

instance FromId (MushroomExposed side) where
  fromId 0 = Exposed
  fromId 1 = NotExposed

instance ToId (MushroomExposed side) where
  toId Exposed    = 0
  toId NotExposed = 1

instance Cardinality (MushroomExposed s) where
  cardinality = 2

data Berries = Berries | NoBerries

instance FromId Berries where
  fromId 0 = Berries
  fromId 1 = NoBerries

instance ToId Berries where
  toId Berries   = 0
  toId NoBerries = 1

instance Cardinality Berries where
  cardinality = 2

data RedstonePlacement (side :: Facing 'False 'False) = RedstoneUp | RedstoneSide | RedstoneNone

instance FromId (RedstonePlacement side) where
  fromId 0 = RedstoneUp
  fromId 1 = RedstoneSide
  fromId 2 = RedstoneNone

instance ToId (RedstonePlacement side) where
  toId RedstoneUp = 0
  toId RedstoneSide = 1
  toId RedstoneNone = 2

instance Cardinality (RedstonePlacement side) where
  cardinality = 3

newtype Power = Power Int

instance FromId Power where
  fromId = coerce

instance ToId Power where
  toId = coerce

instance Cardinality Power where
  cardinality = 16

newtype LightLevel = LightLevel Int

instance FromId LightLevel where
  fromId = coerce

instance ToId LightLevel where
  toId = coerce

instance Cardinality LightLevel where
  cardinality = 16

data HingeSide = HingeLeft | HingeRight

instance FromId HingeSide where
  fromId 0 = HingeLeft
  fromId 1 = HingeRight

instance ToId HingeSide where
  toId HingeLeft  = 0
  toId HingeRight = 1

instance Cardinality HingeSide where
  cardinality = 2

newtype CauldronFill = CauldronFill Int

instance FromId CauldronFill where
  fromId n = CauldronFill $ n + 1

instance ToId CauldronFill where
  toId (CauldronFill fill) = fill -1

instance Cardinality CauldronFill where
  cardinality = 3

data Snowy = Snowy | NotSnowy

instance FromId Snowy where
  fromId 0 = Snowy
  fromId 1 = NotSnowy

instance ToId Snowy where
  toId Snowy    = 0
  toId NotSnowy = 1

instance Cardinality Snowy where
  cardinality = 2

data SculkSensorPhase = SensorInactive | SensorActive | SensorOnCooldown

instance FromId SculkSensorPhase where
  fromId 0 = SensorInactive
  fromId 1 = SensorActive
  fromId 2 = SensorOnCooldown

instance ToId SculkSensorPhase where
  toId SensorInactive   = 0
  toId SensorActive     = 1
  toId SensorOnCooldown = 2

instance Cardinality SculkSensorPhase where
  cardinality = 3

newtype Rotation = Rotation Int

instance FromId Rotation where
  fromId = coerce

instance ToId Rotation where
  toId = coerce

instance Cardinality Rotation where
  cardinality = 16

data FenceGateInWall = InWall | NotInWall

instance FromId FenceGateInWall where
  fromId 0 = InWall
  fromId 1 = NotInWall

instance ToId FenceGateInWall where
  toId InWall    = 0
  toId NotInWall = 1

instance Cardinality FenceGateInWall where
  cardinality = 2

data HasBottle (nr :: Nat) = HasBottle | HasNoBottle

instance FromId (HasBottle nr) where
  fromId 0 = HasBottle
  fromId 1 = HasNoBottle

instance ToId (HasBottle nr) where
  toId HasBottle   = 0
  toId HasNoBottle = 1

instance Cardinality (HasBottle nr) where
  cardinality = 2

data TnTStable = TnTStable | TnTUnstable

instance FromId TnTStable where
  fromId 0 = TnTUnstable
  fromId 1 = TnTStable

instance ToId TnTStable where
  toId TnTStable = 1
  toId TnTUnstable = 0

instance Cardinality TnTStable where
  cardinality = 2

data VerticalDirection = DirUp | DirDown

instance FromId VerticalDirection where
  fromId 0 = DirUp
  fromId 1 = DirDown

instance ToId VerticalDirection where
  toId DirUp   = 0
  toId DirDown = 1

instance Cardinality VerticalDirection where
  cardinality = 2

data DripstoneThickness = TipMerge | Tip | Frustum | Middle | Base

instance FromId DripstoneThickness where
  fromId 0 = TipMerge
  fromId 1 = Tip
  fromId 2 = Frustum
  fromId 3 = Middle
  fromId 4 = Base

instance ToId DripstoneThickness where
  toId TipMerge = 0
  toId Tip      = 1
  toId Frustum  = 2
  toId Middle   = 3
  toId Base     = 4

instance Cardinality DripstoneThickness where
  cardinality = 5

data HasRecord = HasRecord | HasNoRecord

instance FromId HasRecord where
  fromId 0 = HasRecord
  fromId 1 = HasNoRecord

instance ToId HasRecord where
  toId HasRecord   = 0
  toId HasNoRecord = 1

instance Cardinality HasRecord where
  cardinality = 2

data Orientation where
  DownEast  :: Orientation
  DownNorth :: Orientation
  DownSouth :: Orientation
  DownWest  :: Orientation
  UpEast    :: Orientation
  UpNorth   :: Orientation
  UpSouth   :: Orientation
  UpWest    :: Orientation
  WestUp    :: Orientation
  EastUp    :: Orientation
  NorthUp   :: Orientation
  SouthUp   :: Orientation

instance FromId Orientation where
  fromId 0  = DownEast
  fromId 1  = DownNorth
  fromId 2  = DownSouth
  fromId 3  = DownWest
  fromId 4  = UpEast
  fromId 5  = UpNorth
  fromId 6  = UpSouth
  fromId 7  = UpWest
  fromId 8  = WestUp
  fromId 9  = EastUp
  fromId 10 = NorthUp
  fromId 11 = SouthUp

instance ToId Orientation where
  toId DownEast  = 0
  toId DownNorth = 1
  toId DownSouth = 2
  toId DownWest  = 3
  toId UpEast    = 4
  toId UpNorth   = 5
  toId UpSouth   = 6
  toId UpWest    = 7
  toId WestUp    = 8
  toId EastUp    = 9
  toId NorthUp   = 10
  toId SouthUp   = 11

instance Cardinality Orientation where
  cardinality = 12

newtype ComposterFill = ComposterFill Int

instance FromId ComposterFill where
  fromId = coerce

instance ToId ComposterFill where
  toId = coerce

instance Cardinality ComposterFill where
  cardinality = 9

data Locked = Locked | Unlocked

instance FromId Locked where
  fromId 0 = Locked
  fromId 1 = Unlocked

instance ToId Locked where
  toId Locked   = 0
  toId Unlocked = 1

instance Cardinality Locked where
  cardinality = 2

newtype Delay = Delay Int

instance FromId Delay where
  fromId n = Delay $ n + 1 

instance ToId Delay where
  toId (Delay n) = n - 1

instance Cardinality Delay where
  cardinality = 4

data Triggered = Triggered | NotTriggered

instance FromId Triggered where
  fromId 0 = Triggered
  fromId 1 = NotTriggered

instance ToId Triggered where
  toId Triggered    = 0
  toId NotTriggered = 1

instance Cardinality Triggered where
  cardinality = 2

data HasEye = HasEye | HasNoEye

instance FromId HasEye where
  fromId 0 = HasEye
  fromId 1 = HasNoEye

instance ToId HasEye where
  toId HasEye   = 0
  toId HasNoEye = 1

instance Cardinality HasEye where
  cardinality = 2

data Inverted = Inverted | NotInverted

instance FromId Inverted where
  fromId 0 = Inverted
  fromId 1 = NotInverted

instance ToId Inverted where
  toId Inverted    = 0
  toId NotInverted = 1

instance Cardinality Inverted where
  cardinality = 2

data StructureBlockMode = SaveStructure | LoadStructure | CornerStructure | DataStructure

instance FromId StructureBlockMode where
  fromId 0 = SaveStructure
  fromId 1 = LoadStructure
  fromId 2 = CornerStructure
  fromId 3 = DataStructure

instance ToId StructureBlockMode where
  toId SaveStructure   = 0
  toId LoadStructure   = 1
  toId CornerStructure = 2
  toId DataStructure   = 3

instance Cardinality StructureBlockMode where
  cardinality = 4

data Enabled = Enabled | Disabled

instance FromId Enabled where
  fromId 0 = Enabled
  fromId 1 = Disabled

instance ToId Enabled where
  toId Enabled  = 0
  toId Disabled = 1

instance Cardinality Enabled where
  cardinality = 2

data PistonType = Normal | Sticky

instance FromId PistonType where
  fromId 0 = Normal
  fromId 1 = Sticky

instance ToId PistonType where
  toId Normal = 0
  toId Sticky = 1

instance Cardinality PistonType where
  cardinality = 2

data Short = Short | Long

instance FromId Short where
  fromId 0 = Short
  fromId 1 = Long 

instance ToId Short where
  toId Short = 0
  toId Long  = 1

instance Cardinality Short where
  cardinality = 2

data Extended = Extended | Retracted

instance FromId Extended where
  fromId 0 = Extended
  fromId 1 = Retracted 

instance ToId Extended where
  toId Extended  = 0
  toId Retracted = 1

instance Cardinality Extended where
  cardinality = 2

data Hanging = Hanging | NotHanging

instance FromId Hanging where
  fromId 0 = Hanging
  fromId 1 = NotHanging 

instance ToId Hanging where
  toId Hanging    = 0
  toId NotHanging = 1

instance Cardinality Hanging where
  cardinality = 2

newtype Charges = Charges Int

instance FromId Charges where
  fromId = coerce

instance ToId Charges where
  toId = coerce

instance Cardinality Charges where
  cardinality = 5

data Bottom = Bottom | NoBottom

instance FromId Bottom where
  fromId 0 = Bottom
  fromId 1 = NoBottom 

instance ToId Bottom where
  toId Bottom   = 0
  toId NoBottom = 1

instance Cardinality Bottom where
  cardinality = 2

newtype Pickles = Pickles Int

instance FromId Pickles where
  fromId n = Pickles $ n + 1 

instance ToId Pickles where
  toId (Pickles n) = n - 1

instance Cardinality Pickles where
  cardinality = 4

newtype SnowLayers = SnowLayers Int

instance FromId SnowLayers where
  fromId n = SnowLayers $ n + 1 

instance ToId SnowLayers where
  toId (SnowLayers n) = n - 1

instance Cardinality SnowLayers where
  cardinality = 8

newtype Bites = Bites Int

instance FromId Bites where
  fromId = coerce 

instance ToId Bites where
  toId = coerce

instance Cardinality Bites where
  cardinality = 7

data Drag = Drag | NoDrag

instance FromId Drag where
  fromId 0 = Drag
  fromId 1 = NoDrag

instance ToId Drag where
  toId Drag   = 0
  toId NoDrag = 1

instance Cardinality Drag where
  cardinality = 2

data BambooLeaves = NoLeaves | SmallLeaves | LargeLeaves

instance FromId BambooLeaves where
  fromId 0 = NoLeaves
  fromId 1 = SmallLeaves
  fromId 2 = LargeLeaves

instance ToId BambooLeaves where
  toId NoLeaves    = 0
  toId SmallLeaves = 1
  toId LargeLeaves = 2

instance Cardinality BambooLeaves where
  cardinality = 3

data SignalFire = SignalFire | NoSignalFire

instance FromId SignalFire where
  fromId 0 = SignalFire
  fromId 1 = NoSignalFire

instance ToId SignalFire where
  toId SignalFire   = 0
  toId NoSignalFire = 1

instance Cardinality SignalFire where
  cardinality = 2

data HasBook = HasBook | HasNoBook

instance FromId HasBook where
  fromId 0 = HasBook
  fromId 1 = HasNoBook

instance ToId HasBook where
  toId HasBook   = 0
  toId HasNoBook = 1

instance Cardinality HasBook where
  cardinality = 2

newtype Eggs = Eggs Int

instance FromId Eggs where
  fromId n = Eggs $ n + 1 

instance ToId Eggs where
  toId (Eggs n) = n - 1

instance Cardinality Eggs where
  cardinality = 4

newtype Hatch = Hatch Int

instance FromId Hatch where
  fromId = coerce 

instance ToId Hatch where
  toId = coerce

instance Cardinality Hatch where
  cardinality = 3

data WireAttached = WireAttached | WireDetached

instance FromId WireAttached where
  fromId 0 = WireAttached
  fromId 1 = WireDetached

instance ToId WireAttached where
  toId WireAttached = 0
  toId WireDetached = 1

instance Cardinality WireAttached where
  cardinality = 2

data Tilt = NoTilt | UnstableTilt | PartialTilt | FullTilt

instance FromId Tilt where
  fromId 0 = NoTilt
  fromId 1 = UnstableTilt
  fromId 2 = PartialTilt
  fromId 3 = FullTilt

instance ToId Tilt where
  toId NoTilt       = 0
  toId UnstableTilt = 1
  toId PartialTilt  = 2
  toId FullTilt     = 3

instance Cardinality Tilt where
  cardinality = 4

newtype Note = Note Int

instance FromId Note where
  fromId = coerce

instance ToId Note where
  toId = coerce

instance Cardinality Note where
  cardinality = 25

data Instrument =
    Harp | Basedrum | Snare | Hat | Bass | Flute | InstrumentBell | Guitar | Chime
  | Xylophone | IronXylophone | Cowbell | Didgeridoo | Bit | Banjo | Pling

instance FromId Instrument where
  fromId 0  = Harp
  fromId 1  = Basedrum
  fromId 2  = Snare
  fromId 3  = Hat
  fromId 4  = Bass
  fromId 5  = Flute
  fromId 6  = InstrumentBell
  fromId 7  = Guitar
  fromId 8  = Chime
  fromId 9  = Xylophone
  fromId 10 = IronXylophone
  fromId 11 = Cowbell
  fromId 12 = Didgeridoo
  fromId 13 = Bit
  fromId 14 = Banjo
  fromId 15 = Pling 

instance ToId Instrument where
  toId Harp           = 0 
  toId Basedrum       = 1
  toId Snare          = 2
  toId Hat            = 3
  toId Bass           = 4
  toId Flute          = 5
  toId InstrumentBell = 6
  toId Guitar         = 7
  toId Chime          = 8
  toId Xylophone      = 9
  toId IronXylophone  = 10
  toId Cowbell        = 11
  toId Didgeridoo     = 12
  toId Bit            = 13
  toId Banjo          = 14
  toId Pling          = 15

instance Cardinality Instrument where
  cardinality = 16

data Disarmed = Disarmed | Armed

instance FromId Disarmed where
  fromId 0 = Disarmed
  fromId 1 = Armed

instance ToId Disarmed where
  toId Disarmed = 0
  toId Armed    = 1

instance Cardinality Disarmed where
  cardinality = 2

newtype Moisture = Moisture Int

instance FromId Moisture where
  fromId = coerce

instance ToId Moisture where
  toId = coerce

instance Cardinality Moisture where
  cardinality = 7

data SlotOccupied (n :: Nat) = SlotOccupied | SlotEmpty

instance FromId (SlotOccupied n) where
  fromId 0 = SlotEmpty
  fromId 1 = SlotOccupied

instance ToId (SlotOccupied n) where
  toId SlotEmpty    = 0
  toId SlotOccupied = 1

instance Cardinality (SlotOccupied n) where
  cardinality = 2

data Bloom = Bloom | NoBloom

instance FromId Bloom where
  fromId 0 = NoBloom
  fromId 1 = Bloom

instance ToId Bloom where
  toId NoBloom = 0
  toId Bloom   = 1

instance Cardinality Bloom where
  cardinality = 2

data CanSummon = CanSummon | CannotSummon

instance FromId CanSummon where
  fromId 0 = CannotSummon
  fromId 1 = CanSummon

instance ToId CanSummon where
  toId CannotSummon = 0
  toId CanSummon    = 1

instance Cardinality CanSummon where
  cardinality = 2

data Shrieking = Shrieking | NotShrieking

instance FromId Shrieking where
  fromId 0 = NotShrieking
  fromId 1 = Shrieking

instance ToId Shrieking where
  toId NotShrieking = 0
  toId Shrieking    = 1

instance Cardinality Shrieking where
  cardinality = 2

inRange :: Int -> Int -> Int -> Bool
inRange x y z | y == z - 1 = x == y
              | otherwise  = y <= x && x < z
{-# INLINE inRange #-}

generateBlockStatePatterns
