{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiWayIf #-}
module Block.Internal.TH (
  generateBlockStatePatterns
, genPaletteMapping
) where

import Data.Text ( Text )
import Data.Aeson
import Data.Maybe
import qualified Block.Internal.BlockEntry as BE
import Language.Haskell.TH
import qualified Data.Text as T
import Data.Foldable (foldl')
import Data.Char (toUpper)
import qualified Language.Haskell.TH as TH
import Data.List (sortOn)
import Data.Ord
import Data.Semigroup
import Data.Coerce
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector as V

{-

Generates two datatypes:

Vector (Text, [(Text, Text)])
HashMap (Text, [(Text, Text)]) Int

-}
genPaletteMapping :: Q [Dec]
genPaletteMapping = do
  entries <- runIO $ readBlocks
  
  let entriesById = listE $ do
        (nameSpacedName, BE.BlockEntry{..}) <- sortOn (\(_, BE.BlockEntry{blockStates}) -> BE.stateId $ head blockStates) $ M.toList entries
        let props = sortOn (Down . fst) $ maybe [] M.toList blockProperties
            nrProps = length props
            lowId = BE.stateId $ head blockStates
            name = fromSnakeCase $ T.drop 10 nameSpacedName
            getCard ps = foldl' (\x y -> [| $(x) * $(getCard1 y) |]) [| 1 |] ps
            getCard1 prop = appTypeE [| cardinality |] . pure $ conFromProps name prop
            getInd nr = [| \i -> (i `mod` $(getCard $ drop (nr - 1) props)) `div` $(getCard $ drop nr props) |]
        pure $ [| (lowId, (nameSpacedName, \n ->
            let i = n - lowId
            in $(
              foldl' (\acc nr -> [| ($([| fst $ props !! nr|]), ($([| V.fromListN (length props) . snd $ props !! nr |])) V.! ($(getInd nr) i)) : $(acc) |]) [| [] |] [1..nrProps]
              )
          ))
          |]
      entriesByProps = listE $ do
        (nameSpacedName, BE.BlockEntry{..}) <- sortOn (\(_, BE.BlockEntry{blockStates}) -> BE.stateId $ head blockStates) $ M.toList entries
        let lowId = BE.stateId $ head blockStates
            name = fromSnakeCase $ T.drop 10 nameSpacedName
            props = sortOn (Down . fst) $ maybe [] M.toList blockProperties
            nrProps = length props
            getIdFor nr = [| \m ->
              $(getCard $ drop nr props) *
                (fromJust $ elemIndex (fromJust $ M.lookup ($([| fst $ props !! (nr - 1) |])) m) $([| snd $ props !! (nr - 1) |]))
              |]
            getCard ps = foldl' (\x y -> [| $(x) * $(getCard1 y) |]) [| 1 |] ps
            getCard1 prop = appTypeE [| cardinality |] . pure $ conFromProps name prop
        pure [| (nameSpacedName, \m -> $(foldl' (\acc nr -> [| $(acc) + ($(getIdFor nr) m) |]) [| lowId |] [1..nrProps])) |]
      highestId = maximum $ do
        (_, BE.BlockEntry{blockStates}) <- M.toList entries
        BE.BlockState{stateId} <- blockStates
        pure stateId 

  [d|
    idToProps :: IM.IntMap (Text, (Int -> [(Text, Text)]))
    idToProps = IM.fromList $(entriesById)

    propsToId :: HM.HashMap Text (M.Map Text Text -> Int)
    propsToId = HM.fromList $(entriesByProps)

    type HighestBlockStateId = $(pure . LitT . NumTyLit $ fromIntegral highestId)

    |]

    

{-

Generates bidirectional patterns of form:

pattern XXXX :: Arg1 -> ... ArgN -> BlockState
pattern XXXX x1 ... xN <- isStateXXXX -> Just (x1, ..., xN) where
  XXXX x1 ... xN = BlockState toBlockStateXXXX

isStateXXXX (BlockState i) = if inRange i lowestId highestId then Just (getArg i 1, ..., getArg i n) else Nothing
getArg i nr = (i `mod` cardinalities) `div` prevCardinalities
toBlockState x1 ... xN = foldl' (\acc x -> acc + toId x * cardinalities) 0 properties

-- cardinalities and prevCardinalities is precomputed at compile time and is the product of the cardinalities of each property that follows this one

-}
generateBlockStatePatterns :: Q [Dec]
generateBlockStatePatterns = do
  entries <- runIO $ readBlocks
  sequence $ do
    (namespacedName, BE.BlockEntry{..}) <- M.toList entries

    let name = fromSnakeCase $ T.drop 10 namespacedName
        pName = mkName $ T.unpack name

    let props = sortOn (Down . fst) $ maybe [] M.toList blockProperties
        propsDown = reverse props
        lowId :: Int = coerce $ foldMap (\BE.BlockState{..} -> Min stateId) blockStates
        highId :: Int = 1 + (coerce $ foldMap (\BE.BlockState{..} -> Max stateId) blockStates)
        patType = foldl' (\ty x -> AppT (AppT ArrowT $ conFromProps name x) ty) (ConT $ mkName "BlockState") props
        args = zipWith (\x _ -> mkName $ "x" <> show x) [1..] props
        lN = mkName "l"
        toArgs = [| \n -> if inRange (coerce n) lowId highId then let ($(varP lN) :: Int) = coerce n - lowId in Just $(mkTup) else Nothing |]
        fromArgs = foldl' (\x (nr, arg) -> [| $(x) + (toId $(varE arg)) * $(getCard $ drop nr propsDown) |]) [| 0 |] $ zip [1..] args
        viewPat = if | length props == 1 -> varP $ mkName "x1"
                     | otherwise -> tupP $ fmap varP args
        constrPat = varP <$> args
        getCard ps = foldl' (\x y -> [| $(x) * $(getCard1 y) |]) [| 1 |] ps
        getCard1 prop = appTypeE [| cardinality |] . pure $ conFromProps name prop
        mkTup = if | length props == 1 -> [| fromId $(varE lN) |]
                   | null props        -> [| () |]
                   | otherwise         -> tupE $ do
                      (nr, _) <- zip [1..] args
                      pure [| fromId $ ($(varE lN) `mod` $(getCard $ drop (nr - 1) propsDown)) `div` $(getCard $ drop nr propsDown) |]  

    [   patSynSigD pName (pure patType)
      , patSynD pName
          (prefixPatSyn args)
          (explBidir [ clause constrPat (normalB [| BlockState $ $(fromArgs) |]) [] ])
          (parensP $ viewP toArgs $ conP (mkName "Just") [viewPat])
      -- TODO Add pragmas to inline these patterns?
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
  | isWall ty || ty == "Fire" || ty == "ChorusPlant" || ty == "GlowLichen" || ty == "Vine" 
    = AppT (AppT (AppT (ConT . mkName $ T.unpack "Attached") (PromotedT $ mkName "True")) (PromotedT $ mkName "False")) (PromotedT $ mkName "Up")
  | isMushroomBlock ty = AppT (ConT . mkName $ T.unpack "MushroomExposed") (PromotedT $ mkName "Up")
  | otherwise = error $ "Unknown up: " <> T.unpack ty
conFromProps ty ("down", _)
  | isMushroomBlock ty = AppT (ConT . mkName $ T.unpack "MushroomExposed") (PromotedT $ mkName "Down")
  | ty == "ChorusPlant" || ty == "GlowLichen" = AppT (AppT (AppT (ConT . mkName $ T.unpack "Attached") (PromotedT $ mkName "False")) (PromotedT $ mkName "True")) (PromotedT $ mkName "Down")
  | otherwise = error $ "Unknown up: " <> T.unpack ty
conFromProps _ ("waterlogged", _) = ConT $ mkName "Waterlogged"
conFromProps _ ("powered", _) = ConT $ mkName "Powered"
conFromProps ty ("facing", _)
  | isBanner ty || isButton ty || isWallSkull ty || isStair ty || isBed ty || isWallTorch ty || ty == "BeeNest" || isChest ty ||
    isTrapdoor ty || isGlazedTerracotta ty || isDoor ty || isWallSign ty || isWallFan ty || ty == "Smoker" || isFenceGate ty ||
    ty == "BigDripleafStem" || ty == "Repeater" || ty == "Furnace" || ty == "EndPortalFrame" || ty == "Cocoa" || ty == "Bell" ||
    ty == "Beehive" || ty == "JackOLantern" || isAnvil ty || ty == "Comparator" || ty == "AttachedMelonStem" || ty == "AttachedPumpkinStem" ||
    ty == "Ladder" || ty == "Stonecutter" || ty == "Grindstone" || isDripleaf ty || isCampfire ty || ty == "Lectern" || ty == "Lever" ||
    ty == "TripwireHook" || ty == "CarvedPumpkin" || ty == "BlastFurnace" || ty == "Loom"
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
conFromProps _ ("stage", _) = AppT (ConT $ mkName "GrowthStage") (LitT $ NumTyLit 2)
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
  | ty == "Beetroots" || ty == "FrostedIce" || ty == "NetherWart" || ty == "SweetBerryBush"
    = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 4)
  | ty == "Cocoa" || ty == "Bamboo" = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 2)
  | ty == "Potatoes" || ty == "PumpkinStem" || ty == "MelonStem" || ty == "Carrots" ||
    ty == "Wheat"
    = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 7)
  | ty == "ChorusFlower" = AppT (ConT $ mkName "Age") (LitT $ NumTyLit 5)
conFromProps ty (pTy, _) = error $ "Unknown datatype: " <> T.unpack ty <> " : " <> T.unpack pTy 

attachable :: Text -> Bool
attachable ty =
  T.isSuffixOf "GlassPane" ty || ty == "Fire" || T.isSuffixOf "Fence" ty || ty == "ChorusPlant" || ty == "GlowLichen" ||
  ty == "Vine" || ty == "IronBars" || ty == "Tripwire"

isSlab :: Text -> Bool
isSlab ty = T.isSuffixOf "Slab" ty

isBanner :: Text -> Bool
isBanner ty = T.isSuffixOf "Banner" ty

isButton :: Text -> Bool
isButton ty = T.isSuffixOf "Button" ty

isWall :: Text -> Bool
isWall ty = T.isSuffixOf "Wall" ty

isWallSkull :: Text -> Bool
isWallSkull ty = T.isSuffixOf "WallSkull" ty || T.isSuffixOf "WallHead" ty

isStair :: Text -> Bool
isStair ty = T.isSuffixOf "Stairs" ty

isBed :: Text -> Bool
isBed ty = T.isSuffixOf "Bed" ty

isShulkerBox :: Text -> Bool
isShulkerBox ty = T.isSuffixOf "ShulkerBox" ty

isChest :: Text -> Bool
isChest ty = T.isSuffixOf "Chest" ty

isTrapdoor :: Text -> Bool
isTrapdoor ty = T.isSuffixOf "Trapdoor" ty

isCommandBlock :: Text -> Bool
isCommandBlock ty = T.isSuffixOf "CommandBlock" ty

isMushroomBlock :: Text -> Bool
isMushroomBlock ty = T.isSuffixOf "MushroomBlock" ty || T.isSuffixOf "MushroomStem" ty

isGlazedTerracotta :: Text -> Bool
isGlazedTerracotta ty = T.isSuffixOf "GlazedTerracotta" ty

isVines :: Text -> Bool
isVines ty = T.isSuffixOf "Vines" ty

isDoor :: Text -> Bool
isDoor ty = T.isSuffixOf "Door" ty

isWallSign :: Text -> Bool
isWallSign ty = T.isSuffixOf "WallSign" ty

isWallFan :: Text -> Bool
isWallFan ty = T.isSuffixOf "WallFan" ty

isFenceGate :: Text -> Bool
isFenceGate ty = T.isSuffixOf "FenceGate" ty

isAmethystBud :: Text -> Bool
isAmethystBud ty = T.isSuffixOf "AmethystBud" ty

isPiston :: Text -> Bool
isPiston ty = T.isSuffixOf "Piston" ty || ty == "PistonHead"

isAnvil :: Text -> Bool
isAnvil ty = T.isSuffixOf "Anvil" ty

isWallTorch :: Text -> Bool
isWallTorch ty = T.isSuffixOf "WallTorch" ty

isDripleaf :: Text -> Bool
isDripleaf ty = T.isSuffixOf "Dripleaf" ty

isCampfire :: Text -> Bool
isCampfire ty = T.isSuffixOf "Campfire" ty


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
