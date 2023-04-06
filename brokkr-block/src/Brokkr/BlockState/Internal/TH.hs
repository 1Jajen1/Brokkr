{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MagicHash #-}
module Brokkr.BlockState.Internal.TH (
  generateBlockStatePatterns
, genPaletteMapping
) where

import Data.Text ( Text )
import Data.ByteString ( ByteString )
import Data.Aeson
import Data.Maybe
import qualified Brokkr.BlockState.Internal.BlockEntry as BE
import Language.Haskell.TH
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Foldable (foldl')
import Data.Char (toUpper)
import qualified Language.Haskell.TH as TH
import Data.List (sortOn)
import Data.Semigroup
import Data.Coerce
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import Data.Hashable
import Data.Word
import GHC.Exts
import Foreign.ForeignPtr
import qualified Data.Vector.Storable as S
import Control.Monad

{-

Generates two datatypes:

Id to nbt
Vector (ByteString, [(ByteString, ByteString)])

nbt to id
(ByteString, [(ByteString, ByteString)]) -> Int

TODO: Strip minecraft: prefix?

-}
genPaletteMapping :: Q [Dec]
genPaletteMapping = do
  entries <- runIO readBlocks
  
  let namesAndPropsToId = sortOn fst $ do
        (nameSpacedName, BE.BlockEntry{..}) <- sortOn (\(_, BE.BlockEntry{blockStates}) -> BE.stateId $ head blockStates) $ M.toList entries
        BE.BlockState{..} <- blockStates

        pure (fromIntegral $ hash (T.encodeUtf8 nameSpacedName, maybe [] (sortOn fst . fmap (\(k,v) -> (T.encodeUtf8 k, T.encodeUtf8 v)) . M.toList) stateProperties), fromIntegral stateId) 

      highestId = maximum $ do
        (_, BE.BlockEntry{blockStates}) <- M.toList entries
        BE.BlockState{stateId} <- blockStates
        pure stateId
      
      !setOfHashes@(I# setOfHashes#) = IS.size . IS.fromList $ fmap (fromIntegral . fst) namesAndPropsToId

      hLiFptrSz = setOfHashes * 8
      vLiFptrSz = setOfHashes * 4

      hLitArr = S.fromListN setOfHashes $ fmap fst namesAndPropsToId
      valLitArr = S.fromListN setOfHashes $ fmap snd namesAndPropsToId

      (hLitFptr, _, _) = S.unsafeToForeignPtr hLitArr
      (vLitFptr, _, _) = S.unsafeToForeignPtr valLitArr

  let hashLit = litE . BytesPrimL . mkBytes (coerce @(ForeignPtr Word64) hLitFptr) 0 $ fromIntegral hLiFptrSz
      valsLit = litE . BytesPrimL . mkBytes (coerce @(ForeignPtr Word32) vLitFptr) 0 $ fromIntegral vLiFptrSz

  -- error $ show $ S.elemIndex 13909007002878426453 hLitArr
  -- error $ show setOfHashes

  -- Quick check if we have no hash collisions
  -- We will probably run into one at some point, but that's for future me to worry about
  when (setOfHashes /= length namesAndPropsToId) . error $ "Expected " <> show (length namesAndPropsToId) <> " but actual is " <> show setOfHashes

  [d|
    hashProps :: ByteString -> [(ByteString, ByteString)] -> Int
    hashProps !ns !arr = hash (ns, arr)
    {-# INLINE hashProps #-}

    propsToId :: ByteString -> [(ByteString, ByteString)] -> Int
    propsToId !n !props = I# (int32ToInt# (loop 0# setOfHashes#))
      where
        !hs = fromIntegral $ hashProps n props
        hsLit  = $(hashLit)
        valLit = $(valsLit)
        loop !l !u
          | isTrue# (l >=# u) = error $ "Unknown hash " <> show hs <> " for name " <> show n <> " and props " <> show props 
          | otherwise =
            let a = W# (word64ToWord# (indexWord64OffAddr# hsLit mid#))
            in case compare a hs of
              LT -> loop (mid# +# 1#) u
              EQ -> indexInt32OffAddr# valLit mid#
              GT -> loop l mid#
          where
            mid# = (l +# u) `quotInt#` 2#

    type HighestBlockStateId = $(pure . LitT . NumTyLit $ fromIntegral highestId)

    |]

    

{-

Generates bidirectional patterns of form:

pattern XXXX :: Arg1 -> ... ArgN -> BlockState
pattern XXXX x1 ... xN <- isStateXXXX -> Just (x1, ..., xN) where
  XXXX x1 ... xN = BlockState toBlockStateXXXX

isStateXXXX (BlockState i) = if inRange i lowestId highestId then Just (getArg i 1, ..., getArg i n) else Nothing
getArg i nr = (i `mod` cardinalities) `div` prevCardinalities
toBlockState x1 ... xN = foldl' (\acc x -> acc + toId x * cardinalities) lowestId properties

-- cardinalities and prevCardinalities is precomputed at compile time and is the product of the cardinalities of each property that follows this one

TODO Extract getArg and toArg and make a rule for ghc to fuse. Such that

case x of RedstoneWire east north _ south west -> RedstoneWire east north (Power 1) south west

doesn't actually calcuate anything and only replaces the power value

How would this actually work?

\n -> let n' = n - lowestId in (toArg1 n', toArg2 n', toArg3 n')

Say I want to replace arg 2 I do

\(a1, _, a3) -> (lowestId + fromArg1 a1 + fromArg2 new + fromArg3 a3)

So in combination we have:
\n -> let n' = n - lowestId in (\(a1, _, a3) -> (lowestId + fromArg1 a1 + fromArg2 new + fromArg3 a3)) (toArg1 n, toArg2 n, toArg3 n)
=> Case of known constr
\n -> let n' = n - lowestId in (lowestId + fromArg1 (toArg1 n') + fromArg2 new + fromArg3 (toArg3 n'))
=> inline the let
\n -> (lowestId + fromArg1 (toArg1 $ n - lowestId) + fromArg2 new + fromArg3 (toArg3 $ n - lowestId))
=>


toArgX n = (n mod cX) div pcX
fromArgX n = n * cX

fromArgX (toArgX n)
=>
fromArgX . floor ((n mod cX) div pcX)
=>
floor((n mod cX) div pcX) * cX
=>
floor((n - xX * floor(n div cX)) div pcX) * cX
=>


-}
generateBlockStatePatterns :: Q [Dec]
generateBlockStatePatterns = do
  entries <- runIO $ readBlocks
  sequence $ do
    (namespacedName, BE.BlockEntry{..}) <- M.toList entries

    let name = fromSnakeCase $ T.drop 10 namespacedName
        pName = mkName $ T.unpack name

    let props = sortOn fst $ maybe [] M.toList blockProperties
        propsDown = reverse props
        lowId :: Int = coerce $ foldMap (\BE.BlockState{..} -> Min stateId) blockStates
        highId :: Int = 1 + coerce (foldMap (\BE.BlockState{..} -> Max stateId) blockStates)
        patType = foldl' (\ty x -> AppT (AppT ArrowT $ conFromProps name x) ty) (ConT $ mkName "BlockState") props
        args = zipWith (\(x :: Int) _ -> mkName $ "x" <> show x) [1..] props
        lN = mkName "l"
        toArgs = [| \n -> if inRange (coerce n) lowId highId then let ($(varP lN) :: Int) = coerce n - lowId in Just $(mkTup $ varE lN) else Nothing |]
        fromArgs = foldl' (\x (nr, arg) -> [| $(x) + toId $(varE arg) * $(getCard $ drop nr propsDown) |]) [| lowId |] $ zip [1..] args
        viewPat = if length props == 1
          then varP $ mkName "x1"
          else tupP $ fmap varP args
        constrPat = varP <$> args
        getCard = foldl' (\x y -> [| $(x) * $(getCard1 y) |]) [| 1 |]
        getCard1 prop = appTypeE [| cardinality |] . pure $ conFromProps name prop
        mkTup scrut = if | length props == 1 -> [| fromId $(scrut) |]
                         | null props        -> [| () |]
                         | otherwise         -> tupE $ do     
                            (nr, _) <- zip [1..] args
                            pure [| fromId $ ($(scrut) `mod` $(getCard $ drop (nr - 1) propsDown)) `div` $(getCard $ drop nr propsDown) |]  

    [   patSynSigD pName (pure patType)
      , patSynD pName
          (prefixPatSyn args)
          (explBidir [ clause constrPat (normalB [| BlockState $ $(fromArgs) |]) [] ])
          (parensP $ viewP toArgs $ conP (mkName "Just") [viewPat])
      -- https://gitlab.haskell.org/ghc/ghc/-/issues/23203
      -- , pure $ PragmaD $ InlineP pName Inline FunLike AllPhases
      ]

conFromProps :: Text -> (Text, [Text]) -> TH.Type
conFromProps ty ("north", _)
  | attachable ty = AppT (AppT (AppT (ConT . mkName $ T.unpack "Attached") (PromotedT $ mkName "False")) (PromotedT $ mkName "False")) (PromotedT $ mkName "North")
  | isWall ty = AppT (ConT . mkName $ T.unpack "WallAttached") (PromotedT $ mkName "North")
  | isMushroomBlock ty = AppT (ConT . mkName $ T.unpack "MushroomExposed") (PromotedT $ mkName "North")
  | ty == "RedstoneWire" = AppT (ConT . mkName $ T.unpack "RedstonePlacement") (PromotedT $ mkName "North")
  | otherwise = error $ "Unknown north: " <> T.unpack ty
conFromProps ty ("south", _)
  | attachable ty = AppT (AppT (AppT (ConT . mkName $ T.unpack "Attached") (PromotedT $ mkName "False")) (PromotedT $ mkName "False")) (PromotedT $ mkName "South")
  | isWall ty = AppT (ConT . mkName $ T.unpack "WallAttached") (PromotedT $ mkName "South")
  | isMushroomBlock ty = AppT (ConT . mkName $ T.unpack "MushroomExposed") (PromotedT $ mkName "South")
  | ty == "RedstoneWire" = AppT (ConT . mkName $ T.unpack "RedstonePlacement") (PromotedT $ mkName "South")
  | otherwise = error $ "Unknown south: " <> T.unpack ty
conFromProps ty ("east", _)
  | attachable ty = AppT (AppT (AppT (ConT . mkName $ T.unpack "Attached") (PromotedT $ mkName "False")) (PromotedT $ mkName "False")) (PromotedT $ mkName "East")
  | isWall ty = AppT (ConT . mkName $ T.unpack "WallAttached") (PromotedT $ mkName "East")
  | isMushroomBlock ty = AppT (ConT . mkName $ T.unpack "MushroomExposed") (PromotedT $ mkName "East")
  | ty == "RedstoneWire" = AppT (ConT . mkName $ T.unpack "RedstonePlacement") (PromotedT $ mkName "East")
  | otherwise = error $ "Unknown east: " <> T.unpack ty
conFromProps ty ("west", _)
  | attachable ty = AppT (AppT (AppT (ConT . mkName $ T.unpack "Attached") (PromotedT $ mkName "False")) (PromotedT $ mkName "False")) (PromotedT $ mkName "West")
  | isWall ty = AppT (ConT . mkName $ T.unpack "WallAttached") (PromotedT $ mkName "West")
  | isMushroomBlock ty = AppT (ConT . mkName $ T.unpack "MushroomExposed") (PromotedT $ mkName "West")
  | ty == "RedstoneWire" = AppT (ConT . mkName $ T.unpack "RedstonePlacement") (PromotedT $ mkName "West")
  | otherwise = error $ "Unknown west: " <> T.unpack ty
conFromProps ty ("up", _)
  | isWall ty || ty == "Fire" || ty == "ChorusPlant" || ty == "GlowLichen" || ty == "Vine" || ty == "SculkVein"
    = AppT (AppT (AppT (ConT . mkName $ T.unpack "Attached") (PromotedT $ mkName "True")) (PromotedT $ mkName "False")) (PromotedT $ mkName "Up")
  | isMushroomBlock ty = AppT (ConT . mkName $ T.unpack "MushroomExposed") (PromotedT $ mkName "Up")
  | otherwise = error $ "Unknown up: " <> T.unpack ty
conFromProps ty ("down", _)
  | isMushroomBlock ty = AppT (ConT . mkName $ T.unpack "MushroomExposed") (PromotedT $ mkName "Down")
  | ty == "ChorusPlant" || ty == "GlowLichen" || ty == "SculkVein" = AppT (AppT (AppT (ConT . mkName $ T.unpack "Attached") (PromotedT $ mkName "False")) (PromotedT $ mkName "True")) (PromotedT $ mkName "Down")
  | otherwise = error $ "Unknown down: " <> T.unpack ty
conFromProps _ ("waterlogged", _) = ConT $ mkName "Waterlogged"
conFromProps _ ("powered", _) = ConT $ mkName "Powered"
conFromProps ty ("facing", _)
  | isBanner ty || isButton ty || isWallSkull ty || isStair ty || isBed ty || isWallTorch ty || ty == "BeeNest" || isChest ty ||
    isTrapdoor ty || isGlazedTerracotta ty || isDoor ty || isWallSign ty || isWallFan ty || ty == "Smoker" || isFenceGate ty ||
    ty == "BigDripleafStem" || ty == "Repeater" || ty == "Furnace" || ty == "EndPortalFrame" || ty == "Cocoa" || ty == "Bell" ||
    ty == "Beehive" || ty == "JackOLantern" || isAnvil ty || ty == "Comparator" || ty == "AttachedMelonStem" || ty == "AttachedPumpkinStem" ||
    ty == "Ladder" || ty == "Stonecutter" || ty == "Grindstone" || isDripleaf ty || isCampfire ty || ty == "Lectern" || ty == "Lever" ||
    ty == "TripwireHook" || ty == "CarvedPumpkin" || ty == "BlastFurnace" || ty == "Loom" || isHangingSign ty || ty == "ChiseledBookshelf"
    = facingNoUpNoDown
  | isShulkerBox ty || isCommandBlock ty || ty == "LightningRod" || ty == "Dropper" || ty == "AmethystCluster" || isAmethystBud ty ||
    isPiston ty || ty == "Dispenser" || ty == "Barrel" || ty == "EndRod" || ty == "Observer"
    = facingUpAndDown
  | ty == "Hopper" = facingNoUpDown
  | otherwise = error $ "Unknown facing: " <> T.unpack ty
  where
    facingUpAndDown = AppT (AppT (ConT $ mkName "Facing") (PromotedT $ mkName "True")) (PromotedT $ mkName "True")
    facingNoUpNoDown = AppT (AppT (ConT $ mkName "Facing") (PromotedT $ mkName "False")) (PromotedT $ mkName "False")
    facingNoUpDown = AppT (AppT (ConT $ mkName "Facing") (PromotedT $ mkName "False")) (PromotedT $ mkName "True")
conFromProps _ ("face", _) = AppT (ConT $ mkName "Face") (PromotedT $ mkName "False")
conFromProps _ ("attachment", _) = AppT (ConT $ mkName "Face") (PromotedT $ mkName "True")
conFromProps ty ("type", _)
  | isSlab ty = ConT $ mkName "SlabType"
  | isChest ty = ConT $ mkName "ChestType"
  | isPiston ty = ConT $ mkName "PistonType"
  | otherwise = error $ "Unknown type: " <> T.unpack ty
conFromProps _ ("axis", _) = ConT $ mkName "Axis"
conFromProps _ ("stage", _) = AppT (ConT $ mkName "GrowthStage") (LitT $ NumTyLit 1)
conFromProps _ ("shape", _) = ConT $ mkName "StairShape"
conFromProps _ ("half", _) = ConT $ mkName "Half"
conFromProps _ ("part", _) = ConT $ mkName "BedPart"
conFromProps _ ("occupied", _) = ConT $ mkName "Occupied"
conFromProps _ ("persistent", _) = ConT $ mkName "Persistent"
conFromProps _ ("distance", _) = ConT $ mkName "LeafDistance"
conFromProps _ ("honey_level", _) = ConT $ mkName "HoneyLevel"
conFromProps _ ("lit", _) = ConT $ mkName "Lit"
conFromProps _ ("open", _) = ConT $ mkName "Open"
conFromProps _ ("conditional", _) = ConT $ mkName "Conditional"
conFromProps _ ("candles", _) = ConT $ mkName "Candles"
conFromProps _ ("berries", _) = ConT $ mkName "Berries"
conFromProps _ ("power", _) = ConT $ mkName "Power"
conFromProps _ ("hinge", _) = ConT $ mkName "HingeSide"
conFromProps _ ("snowy", _) = ConT $ mkName "Snowy"
conFromProps _ ("sculk_sensor_phase", _) = ConT $ mkName "SculkSensorPhase"
conFromProps _ ("in_wall", _) = ConT $ mkName "FenceGateInWall"
conFromProps _ ("rotation", _) = ConT $ mkName "Rotation"
conFromProps _ ("unstable", _) = ConT $ mkName "TnTStable"
conFromProps _ ("thickness", _) = ConT $ mkName "DripstoneThickness"
conFromProps _ ("has_record", _) = ConT $ mkName "HasRecord"
conFromProps _ ("orientation", _) = ConT $ mkName "Orientation"
conFromProps _ ("locked", _) = ConT $ mkName "Locked"
conFromProps _ ("delay", _) = ConT $ mkName "Delay"
conFromProps _ ("triggered", _) = ConT $ mkName "Triggered"
conFromProps _ ("eye", _) = ConT $ mkName "HasEye"
conFromProps _ ("inverted", _) = ConT $ mkName "Inverted"
conFromProps _ ("mode", _) = ConT $ mkName "StructureBlockMode"
conFromProps _ ("enabled", _) = ConT $ mkName "Enabled"
conFromProps _ ("short", _) = ConT $ mkName "Short"
conFromProps _ ("extended", _) = ConT $ mkName "Extended"
conFromProps _ ("hanging", _) = ConT $ mkName "Hanging"
conFromProps _ ("charges", _) = ConT $ mkName "Charges"
conFromProps _ ("bottom", _) = ConT $ mkName "Bottom"
conFromProps _ ("pickles", _) = ConT $ mkName "Pickles"
conFromProps _ ("layers", _) = ConT $ mkName "SnowLayers"
conFromProps _ ("bites", _) = ConT $ mkName "Bites"
conFromProps _ ("drag", _) = ConT $ mkName "Drag"
conFromProps _ ("leaves", _) = ConT $ mkName "BambooLeaves"
conFromProps _ ("signal_fire", _) = ConT $ mkName "SignalFire"
conFromProps _ ("has_book", _) = ConT $ mkName "HasBook"
conFromProps _ ("hatch", _) = ConT $ mkName "Hatch"
conFromProps _ ("eggs", _) = ConT $ mkName "Eggs"
conFromProps _ ("attached", _) = ConT $ mkName "WireAttached"
conFromProps _ ("tilt", _) = ConT $ mkName "Tilt"
conFromProps _ ("instrument", _) = ConT $ mkName "Instrument"
conFromProps _ ("note", _) = ConT $ mkName "Note"
conFromProps _ ("disarmed", _) = ConT $ mkName "Disarmed"
conFromProps _ ("moisture", _) = ConT $ mkName "Moisture"
conFromProps _ ("vertical_direction", _) = ConT $ mkName "VerticalDirection"
conFromProps _ ("has_bottle_0", _) = AppT (ConT $ mkName "HasBottle") (LitT $ NumTyLit 0)
conFromProps _ ("has_bottle_1", _) = AppT (ConT $ mkName "HasBottle") (LitT $ NumTyLit 1)
conFromProps _ ("has_bottle_2", _) = AppT (ConT $ mkName "HasBottle") (LitT $ NumTyLit 2)
conFromProps "Composter" ("level", _) = ConT $ mkName "ComposterFill"
conFromProps "Light" ("level", _) = ConT $ mkName "LightLevel"
conFromProps "WaterCauldron" ("level", _) = ConT $ mkName "CauldronFill"
conFromProps "PowderSnowCauldron" ("level", _) = ConT $ mkName "CauldronFill"
conFromProps "Water" ("level", _) = AppT (ConT $ mkName "FluidLevel") (LitT $ NumTyLit 15)
conFromProps "Lava" ("level", _) = AppT (ConT $ mkName "FluidLevel") (LitT $ NumTyLit 15)
conFromProps "MelonStem" ("age", _) = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 7)
conFromProps ty ("age", _)
  | isVines ty || ty == "Kelp" = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 25)
  | ty == "Fire" || ty == "SugarCane" || ty == "Cactus"
    = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 15)
  | ty == "Beetroots" || ty == "FrostedIce" || ty == "NetherWart" || ty == "SweetBerryBush" || ty == "MangrovePropagule"
    = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 4)
  | ty == "Cocoa" = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 2)
  | ty == "Bamboo" = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 1)
  | ty == "Potatoes" || ty == "PumpkinStem" || ty == "MelonStem" || ty == "Carrots" ||
    ty == "Wheat"
    = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 7)
  | ty == "ChorusFlower" = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 5)
conFromProps "ChiseledBookshelf" (pTy, _)
  | pTy == "slot_0_occupied" = AppT (ConT $ mkName "SlotOccupied") (LitT $ NumTyLit 0)
  | pTy == "slot_1_occupied" = AppT (ConT $ mkName "SlotOccupied") (LitT $ NumTyLit 1)
  | pTy == "slot_2_occupied" = AppT (ConT $ mkName "SlotOccupied") (LitT $ NumTyLit 2)
  | pTy == "slot_3_occupied" = AppT (ConT $ mkName "SlotOccupied") (LitT $ NumTyLit 3)
  | pTy == "slot_4_occupied" = AppT (ConT $ mkName "SlotOccupied") (LitT $ NumTyLit 4)
  | pTy == "slot_5_occupied" = AppT (ConT $ mkName "SlotOccupied") (LitT $ NumTyLit 5)
conFromProps "SculkCatalyst" ("bloom", _) = ConT $ mkName "Bloom"
conFromProps "SculkShrieker" ("can_summon", _) = ConT $ mkName "CanSummon"
conFromProps "SculkShrieker" ("shrieking", _) = ConT $ mkName "Shrieking"
conFromProps ty (pTy, _) = error $ "Unknown datatype: " <> T.unpack ty <> " : " <> T.unpack pTy 

attachable :: Text -> Bool
attachable ty =
  T.isSuffixOf "GlassPane" ty || ty == "Fire" || T.isSuffixOf "Fence" ty || ty == "ChorusPlant" || ty == "GlowLichen" ||
  ty == "Vine" || ty == "IronBars" || ty == "Tripwire" || ty == "SculkVein"

isSlab :: Text -> Bool
isSlab = T.isSuffixOf "Slab"

isBanner :: Text -> Bool
isBanner = T.isSuffixOf "Banner"

isButton :: Text -> Bool
isButton = T.isSuffixOf "Button"

isWall :: Text -> Bool
isWall = T.isSuffixOf "Wall"

isWallSkull :: Text -> Bool
isWallSkull ty = T.isSuffixOf "WallSkull" ty || T.isSuffixOf "WallHead" ty

isStair :: Text -> Bool
isStair = T.isSuffixOf "Stairs"

isBed :: Text -> Bool
isBed = T.isSuffixOf "Bed"

isShulkerBox :: Text -> Bool
isShulkerBox = T.isSuffixOf "ShulkerBox"

isChest :: Text -> Bool
isChest = T.isSuffixOf "Chest"

isTrapdoor :: Text -> Bool
isTrapdoor = T.isSuffixOf "Trapdoor"

isCommandBlock :: Text -> Bool
isCommandBlock = T.isSuffixOf "CommandBlock"

isMushroomBlock :: Text -> Bool
isMushroomBlock ty = T.isSuffixOf "MushroomBlock" ty || T.isSuffixOf "MushroomStem" ty

isGlazedTerracotta :: Text -> Bool
isGlazedTerracotta = T.isSuffixOf "GlazedTerracotta"

isVines :: Text -> Bool
isVines = T.isSuffixOf "Vines"

isDoor :: Text -> Bool
isDoor = T.isSuffixOf "Door"

isWallSign :: Text -> Bool
isWallSign = T.isSuffixOf "WallSign"

isWallFan :: Text -> Bool
isWallFan = T.isSuffixOf "WallFan"

isFenceGate :: Text -> Bool
isFenceGate = T.isSuffixOf "FenceGate"

isAmethystBud :: Text -> Bool
isAmethystBud = T.isSuffixOf "AmethystBud"

isPiston :: Text -> Bool
isPiston ty = T.isSuffixOf "Piston" ty || ty == "PistonHead"

isAnvil :: Text -> Bool
isAnvil = T.isSuffixOf "Anvil"

isWallTorch :: Text -> Bool
isWallTorch = T.isSuffixOf "WallTorch"

isDripleaf :: Text -> Bool
isDripleaf = T.isSuffixOf "Dripleaf"

isCampfire :: Text -> Bool
isCampfire = T.isSuffixOf "Campfire"

isHangingSign :: Text -> Bool
isHangingSign = T.isSuffixOf "HangingSign"

-- Reading blocks
type BlockEntries = M.Map Text BE.BlockEntry

readBlocks :: IO BlockEntries
readBlocks = fromJust <$> decodeFileStrict' @BlockEntries "./blocks.json"

fromSnakeCase :: Text -> Text
fromSnakeCase t = firstUpperCase $ replaceSnake t
  where
    replaceSnake t1 = T.concat $ firstUpperCase <$> T.splitOn "_" t1

firstUpperCase :: Text -> Text
firstUpperCase t = case T.uncons t of
  Just (c, xs) -> T.cons (toUpper c) xs
  Nothing -> t
