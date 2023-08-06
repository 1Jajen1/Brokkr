{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
module Brokkr.Packet.ClientToServer.Play.Types.Internal (
  TransactionId(..)
, MessageCount(..)
, Command(..), mkCommand
, Timestamp(..)
, Salt(..)
, ArgumentSignatures(..)
, ArgumentSignature(..), mkArgumentSignature
, Acknowledged(..)
, Message(..), mkMessage
, Signature(..), mkSignature
, ActionId(..)
, Locale(..), mkLocale
, ViewDistance(..)
, ChatMode(..)
, UseChatColors(..)
, DisplayedSkinParts(..)
, pattern DisplayedSkinParts, capeEnabled, jacketEnabled, leftSleeveEnabled, rightSleeveEnabled, leftPantsLegEnabled, rightPantsLegEnabled, hatEnabled
, MainHand(..)
, UseTextFiltering(..)
, AllowServerListings(..)
, CommandSuggestionsReqText(..), mkCommandSuggestionsReqText
, ContainerClickAction(..)
, PluginMessageData(..)
, InteractType(..)
, JigsawLevels(..)
, KeepJigsaws(..)
, Lock(..)
, PaddleTurning(..)
, MakeAll(..)
, PlayerAbilities(..)
, pattern Abilities, isFlying
, ActionStatus(..)
, BlockFace(..)
, PlayerCommandAction(..)
, HorseJumpBoost(..)
, InputSideways(..)
, InputForward(..)
, InputFlags(..)
, pattern InputFlags, inputJump, inputUnmount
, RecipeBookId(..)
, BookOpen(..)
, FilterActive(..)
, ResourcePackStatus(..)
, AdvancementTabAction(..)
, AdvancementTab(..), mkAdvancementTab
, SignLine(..), mkSignLine
, TargetPlayer(..)
, Cursor(..)
, InsideBlock(..)
) where

import Brokkr.Packet.Binary
import Brokkr.Packet.Common.Internal
import Brokkr.Packet.MCString
import Brokkr.Packet.SizePrefixed

import Data.Bits
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Coerce
import Data.Int
import Data.Text.Internal (Text(..))
import qualified Data.Vector as V
import Data.Word

import FlatParse.Basic qualified as Flatparse

import GHC.Exts
import GHC.Generics (Generic)

newtype TransactionId = TransactionId Int32
  deriving stock Show
  deriving (ToBinary, FromBinary) via VarInt

newtype MessageCount = MessageCount Int32
  deriving stock Show
  deriving (ToBinary, FromBinary) via VarInt

newtype Command = UnsafeCommand { unCommand :: Text }
  deriving stock Show
  deriving (ToBinary, FromBinary) via MCString 256

mkCommand :: Text -> Maybe Command
mkCommand = coerce . mkMCString @256

newtype Timestamp = Timestamp Int64
  deriving stock Show
  deriving newtype (ToBinary, FromBinary)

newtype Salt = Salt Int64
  deriving stock Show
  deriving newtype (ToBinary, FromBinary)

newtype ArgumentSignatures = ArgumentSignatures (V.Vector ArgumentSignature)
  deriving newtype Show
  deriving (ToBinary, FromBinary) via SizePrefixed VarInt (V.Vector ArgumentSignature)

data ArgumentSignature = UnsafeArgumentSignature {
  argumentName      :: !Text
, argumentSignature :: !ByteString
}

instance ToBinary ArgumentSignature where
  put (UnsafeArgumentSignature name sig) = put (UnsafeMCString name) <> put sig
  {-# INLINE put #-}

instance FromBinary ArgumentSignature where
  with f = with @(MCString 256) $ \(UnsafeMCString name) ->
    with @(FixedSizeByteString 256) $ \(UnsafeFixedSizeByteString sig) ->
      f (UnsafeArgumentSignature name sig)
  {-# INLINE with #-}

mkArgumentSignature :: Text -> ByteString -> Maybe ArgumentSignature
mkArgumentSignature t@(Text _ _ tLen) bs
  | tLen <= 256 && BS.length bs == 256 = Just $ UnsafeArgumentSignature t bs
  | otherwise = Nothing

instance Show ArgumentSignature where
  showsPrec prec UnsafeArgumentSignature{..} = showParen (prec > 10) $
      showString "ArgumentSignature { "
    . showString "argumentName = "
    . showsPrec 11 argumentName
    . showString ", argumentSignature = "
    . showsPrec 11 (HexBS argumentSignature)
    . showString " }"

newtype Acknowledged = Acknowledged BitSet
  deriving stock Show
  deriving newtype (ToBinary, FromBinary)

newtype Message = UnsafeMessage { unMessage :: Text }
  deriving stock Show
  deriving (ToBinary, FromBinary) via MCString 256

mkMessage :: Text -> Maybe Message
mkMessage = coerce . mkMCString @256

newtype Signature = UnsafeSignature { unSignature :: ByteString }
  deriving (ToBinary, FromBinary) via FixedSizeByteString 256

mkSignature :: ByteString -> Maybe Signature
mkSignature bs
  | BS.length bs == 256 = Just $ UnsafeSignature bs
  | otherwise = Nothing

instance Show Signature where
  showsPrec prec (UnsafeSignature bs) = showParen (prec > 10) $
    showString "Signature " . showsPrec 11 (HexBS bs)

data ActionId = PerformRespawn | RequestStats
  deriving stock (Show, Generic)

instance ToBinary ActionId where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary ActionId where
  with = withEnum @VarInt (\w -> tagToEnum# w)
  {-# INLINE with #-}

newtype Locale = UnsafeLocale { unLocale :: Text }
  deriving stock Show
  deriving (ToBinary, FromBinary) via MCString 16

mkLocale :: Text -> Maybe Locale
mkLocale = coerce . mkMCString @16

newtype ViewDistance = ViewDistance Int8
  deriving stock Show
  deriving newtype (ToBinary, FromBinary)

data ChatMode = ChatEnabled | ChatCommandsOnly | ChatHidden
  deriving stock (Show, Generic)

instance ToBinary ChatMode where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary ChatMode where
  with = withEnum @VarInt (\w -> tagToEnum# w)
  {-# INLINE with #-}

data UseChatColors = NoChatColors | UseChatColors
  deriving stock (Show, Generic)

instance ToBinary UseChatColors where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary UseChatColors where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

newtype DisplayedSkinParts = UnsafeDisplayedSkinParts Word8
  deriving newtype (ToBinary, FromBinary)

pattern DisplayedSkinParts :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> DisplayedSkinParts
pattern DisplayedSkinParts {
  capeEnabled
, jacketEnabled
, leftSleeveEnabled
, rightSleeveEnabled
, leftPantsLegEnabled
, rightPantsLegEnabled
, hatEnabled
} <- (\(UnsafeDisplayedSkinParts x) -> (testBit x 0, testBit x 1, testBit x 2, testBit x 3, testBit x 4, testBit x 5, testBit x 6)
  -> (capeEnabled, jacketEnabled, leftSleeveEnabled, rightSleeveEnabled, leftPantsLegEnabled, rightPantsLegEnabled, hatEnabled))
  where
    DisplayedSkinParts capeEnabled jacketEnabled leftSleeveEnabled rightSleeveEnabled leftPantsLegEnabled rightPantsLegEnabled hatEnabled
      = UnsafeDisplayedSkinParts $
            boolBit capeEnabled          0
        .|. boolBit jacketEnabled        1
        .|. boolBit leftSleeveEnabled    2
        .|. boolBit rightSleeveEnabled   3
        .|. boolBit leftPantsLegEnabled  4
        .|. boolBit rightPantsLegEnabled 5
        .|. boolBit hatEnabled           6
{-# INLINE DisplayedSkinParts #-}
{-# COMPLETE DisplayedSkinParts #-}

boolBit :: Bool -> Int -> Word8
boolBit False = const 0
boolBit True  = bit

instance Show DisplayedSkinParts where
  showsPrec prec skp = showParen (prec > 10) $
      showString "DisplayedSkinParts { "
    . showString "capeEnabled = "
    . showsPrec 11 (capeEnabled skp)
    . showString ", jacketEnabled = "
    . showsPrec 11 (jacketEnabled skp)
    . showString ", leftSleeveEnabled = "
    . showsPrec 11 (leftSleeveEnabled skp)
    . showString ", rightSleeveEnabled = "
    . showsPrec 11 (rightSleeveEnabled skp)
    . showString ", leftPantsLegEnabled = "
    . showsPrec 11 (leftPantsLegEnabled skp)
    . showString ", rightPantsLegEnabled = "
    . showsPrec 11 (rightPantsLegEnabled skp)
    . showString ", hatEnabled = "
    . showsPrec 11 (hatEnabled skp)
    . showString " }"

data MainHand = HandLeft | HandRight
  deriving stock (Show, Generic)

instance ToBinary MainHand where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary MainHand where
  with = withEnum @VarInt (\w -> tagToEnum# w)
  {-# INLINE with #-}

data UseTextFiltering = NoTextFiltering | UseTextFiltering
  deriving stock (Show, Generic)

instance ToBinary UseTextFiltering where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary UseTextFiltering where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data AllowServerListings = DisallowServerListings | AllowServerListings
  deriving stock (Show, Generic)

instance ToBinary AllowServerListings where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary AllowServerListings where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

newtype CommandSuggestionsReqText = UnsafeCommandSuggestionsReqText { unCommandSuggestionsReqText :: Text }
  deriving newtype Show
  deriving (ToBinary, FromBinary) via MCString 32500

mkCommandSuggestionsReqText :: Text -> Maybe CommandSuggestionsReqText
mkCommandSuggestionsReqText = coerce . mkMCString @32500

-- TODO Hide constructor and make this safe with pattern synonyms
data ContainerClickAction = UnsafeContainerClickAction !(SlotIndex Int16) !ButtonId !Int32
  deriving stock Show

instance ToBinary ContainerClickAction where
  put (UnsafeContainerClickAction si bi m) = put si <> put bi <> put (VarInt m)
  {-# INLINE put #-}

instance FromBinary ContainerClickAction where
  with f = with $ \si -> with $ \bi -> with @VarInt $ \case
    0 -> case bi of
      UnsafeButtonId 0 -> f (UnsafeContainerClickAction si bi 0)
      UnsafeButtonId 1 -> f (UnsafeContainerClickAction si bi 0)
      UnsafeButtonId n -> Flatparse.err $ InvalidEnumValue "Container click action with mode 0" (fromIntegral n)
    1 -> case bi of
      UnsafeButtonId 0 -> f (UnsafeContainerClickAction si bi 1)
      UnsafeButtonId 1 -> f (UnsafeContainerClickAction si bi 1)
      UnsafeButtonId n -> Flatparse.err $ InvalidEnumValue "Container click action with mode 1" (fromIntegral n)
    2 -> let UnsafeButtonId n = bi
         in if (n < 0 || n > 8) && (n /= 40)
          then Flatparse.err $ InvalidEnumValue "Container click action with mode 2" (fromIntegral n)
          else f (UnsafeContainerClickAction si bi 2)
    3 -> case bi of
      UnsafeButtonId 2 -> f (UnsafeContainerClickAction si bi 3)
      UnsafeButtonId n -> Flatparse.err $ InvalidEnumValue "Container click action with mode 3" (fromIntegral n)
    4 -> case bi of
      UnsafeButtonId 0 -> f (UnsafeContainerClickAction si bi 4)
      UnsafeButtonId 1 -> f (UnsafeContainerClickAction si bi 4)
      UnsafeButtonId n -> Flatparse.err $ InvalidEnumValue "Container click action with mode 4" (fromIntegral n)
    5 -> case (bi, si) of
      (UnsafeButtonId 0, UnsafeSlotIndex (-999)) -> f (UnsafeContainerClickAction si bi 5)
      (UnsafeButtonId 4, UnsafeSlotIndex (-999)) -> f (UnsafeContainerClickAction si bi 5)
      (UnsafeButtonId 8, UnsafeSlotIndex (-999)) -> f (UnsafeContainerClickAction si bi 5)
      (UnsafeButtonId 1, _                     ) -> f (UnsafeContainerClickAction si bi 5)
      (UnsafeButtonId 5, _                     ) -> f (UnsafeContainerClickAction si bi 5)
      (UnsafeButtonId 9, _                     ) -> f (UnsafeContainerClickAction si bi 5)
      (UnsafeButtonId 2, UnsafeSlotIndex (-999)) -> f (UnsafeContainerClickAction si bi 5)
      (UnsafeButtonId 6, UnsafeSlotIndex (-999)) -> f (UnsafeContainerClickAction si bi 5)
      (UnsafeButtonId 10,UnsafeSlotIndex (-999)) -> f (UnsafeContainerClickAction si bi 5)
      (UnsafeButtonId n, _)  -> Flatparse.err $ InvalidEnumValue "Container click action with mode 5 (May also be a wrong slot index!)" (fromIntegral n) -- TODO Err
    6 -> case bi of
      UnsafeButtonId 0 -> f (UnsafeContainerClickAction si bi 6)
      UnsafeButtonId n -> Flatparse.err $ InvalidEnumValue "Container click action with mode 6" (fromIntegral n)
    m -> Flatparse.err $ InvalidEnumValue "Container click action unknown mode" (fromIntegral m)
  {-# INLINE with #-}

newtype PluginMessageData = PluginMessageData ByteString
  deriving newtype ToBinary

instance Show PluginMessageData where
  showsPrec prec (PluginMessageData bs) = showParen (prec > 10) $
    showString "PluginMessageData " . showsPrec 11 (HexBS bs)

instance FromBinary PluginMessageData where
  get = PluginMessageData <$> Flatparse.takeRest
  {-# INLINE get #-}

data InteractType = InteractWith | Attack | InteractAt !Float !Float !Float !Hand
  deriving stock Show

instance ToBinary InteractType where
  put InteractWith = put @VarInt 0
  put Attack = put @VarInt 1
  put (InteractAt x y z hand) = put @VarInt 2 <> put x <> put y <> put z <> put hand
  {-# INLINE put #-}

instance FromBinary InteractType where
  with f = with @VarInt $ \case
    0 -> f InteractWith
    1 -> f Attack
    2 -> (InteractAt <$> get <*> get <*> get <*> get) >>= f
    n -> Flatparse.err $ InvalidEnumValue "InteractType" (fromIntegral n)
  {-# INLINE with #-}

newtype JigsawLevels = JigsawLevels Int32
  deriving stock Show
  deriving (ToBinary, FromBinary) via VarInt

data KeepJigsaws = DiscardJigsaws | KeepJigsaws
  deriving stock (Show, Generic)

instance ToBinary KeepJigsaws where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary KeepJigsaws where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data Lock = Unlocked | Lock
  deriving stock (Show, Generic)

instance ToBinary Lock where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary Lock where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data PaddleTurning = PaddleStill | PaddleTurning
  deriving stock (Show, Generic)

instance ToBinary PaddleTurning where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary PaddleTurning where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data MakeAll = MakeOne | MakeAll
  deriving stock (Show, Generic)

instance ToBinary MakeAll where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary MakeAll where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

newtype PlayerAbilities = UnsafePlayerAbilities Int8
  deriving newtype (ToBinary, FromBinary)

pattern Abilities :: Bool -> PlayerAbilities
pattern Abilities { isFlying } <-
  ((\(UnsafePlayerAbilities i) -> testBit i 1) -> isFlying)
  where Abilities isFlying = UnsafePlayerAbilities . fromIntegral $ boolBit isFlying 1

instance Show PlayerAbilities where
  showsPrec prec pa = showParen (prec > 10) $
      showString "Abilities { "
    . showString "isFlying = "
    . showsPrec 11 (isFlying pa)
    . showString " }"

data ActionStatus =
    StartDigging
  | CancelledDigging
  | FinishedDigging
  | DropItemStack
  | DropItem
  | ShootArrowOrFinishShooting
  | SwapItemInHand
  deriving stock (Show, Generic)

instance ToBinary ActionStatus where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary ActionStatus where
  with = withEnum @VarInt (\w -> tagToEnum# w)
  {-# INLINE with #-}

data BlockFace = FaceBottom | FaceTop | FaceNorth | FaceSouth | FaceWest | FaceEast
  deriving stock (Show, Generic)

instance ToBinary BlockFace where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary BlockFace where
  with = withEnum @VarInt (\w -> tagToEnum# w)
  {-# INLINE with #-}

data PlayerCommandAction =
    StartSneaking
  | StopSneaking
  | LeaveBed
  | StartSprinting
  | StopSprinting
  | StartJumpWithHorse
  | StopJumpWithHorse
  | OpenHorseInventory
  | StartFlyingWithElytra
  deriving stock (Show, Generic)

instance ToBinary PlayerCommandAction where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary PlayerCommandAction where
  with = withEnum @VarInt (\w -> tagToEnum# w)
  {-# INLINE with #-}

newtype HorseJumpBoost = HorseJumpBoost Int32
  deriving stock Show
  deriving (ToBinary, FromBinary) via VarInt

newtype InputSideways = InputSideways Float
  deriving stock Show
  deriving newtype (ToBinary, FromBinary)

newtype InputForward = InputForward Float
  deriving stock Show
  deriving newtype (ToBinary, FromBinary)

newtype InputFlags = UnsafeInputFlags Word8
  deriving newtype (ToBinary, FromBinary)

pattern InputFlags :: Bool -> Bool -> InputFlags
pattern InputFlags { inputJump, inputUnmount } <-
  ((\(UnsafeInputFlags w) -> (testBit w 1, testBit w 2)) -> (inputJump, inputUnmount))
  where InputFlags inputJump inputUnmount = UnsafeInputFlags $ boolBit inputJump 1 .|. boolBit inputUnmount 2
{-# COMPLETE InputFlags #-} 

instance Show InputFlags where
  showsPrec prec p = showParen (prec > 10) $
      showString "InputFlags { "
    . showString "inputJump = "
    . showsPrec 11 (inputJump p)
    . showString ", inputUnmount = "
    . showsPrec 11 (inputUnmount p)
    . showString " }"

data RecipeBookId = BookCrafting | BookFurnace | BookBlastFurnace | BookSmoker
  deriving stock (Show, Generic)

instance ToBinary RecipeBookId where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary RecipeBookId where
  with = withEnum @VarInt (\w -> tagToEnum# w)
  {-# INLINE with #-}

data BookOpen = BookClosed | BookOpen
  deriving stock (Show, Generic)

instance ToBinary BookOpen where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary BookOpen where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data FilterActive = FilterActive | NoFilterActive
  deriving stock (Show, Generic)

instance ToBinary FilterActive where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary FilterActive where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data ResourcePackStatus = 
    ResourcePackLoaded
  | ResourcePackDeclined
  | ResourcePackFailed
  | ResourcePackAccepted
  deriving stock (Show, Generic)

instance ToBinary ResourcePackStatus where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary ResourcePackStatus where
  with = withEnum @VarInt (\w -> tagToEnum# w)
  {-# INLINE with #-}

data AdvancementTabAction = OpenAdvancementTab !AdvancementTab | CloseAdvancementTab
  deriving stock Show

instance ToBinary AdvancementTabAction where
  put (OpenAdvancementTab tab) = put @VarInt 0 <> put tab
  put CloseAdvancementTab = put @VarInt 1
  {-# INLINE put #-}

instance FromBinary AdvancementTabAction where
  with f = with @VarInt $ \case
    0 -> with $ f . OpenAdvancementTab
    1 -> f CloseAdvancementTab
    n -> Flatparse.err $ InvalidEnumValue "AdvancementTabAction" (fromIntegral n) 
  {-# INLINE with #-}

newtype AdvancementTab = UnsafeAdvancementTab Text
  deriving stock Show
  deriving (ToBinary, FromBinary) via Identifier

mkAdvancementTab :: Text -> Maybe AdvancementTab
mkAdvancementTab = coerce . mkIdentifier

newtype SignLine = UnsafeSignLine Text
  deriving stock Show
  deriving (ToBinary, FromBinary) via MCString 384

mkSignLine :: Text -> Maybe SignLine
mkSignLine = coerce . mkMCString @384

newtype TargetPlayer = TargetPlayer UUID
  deriving stock Show
  deriving newtype (ToBinary, FromBinary)

data Cursor = Cursor !Float !Float !Float
  deriving stock Show

instance ToBinary Cursor where
  put (Cursor x y z) = put x <> put y <> put z
  {-# INLINE put #-}

instance FromBinary Cursor where
  with f = with $ \x -> with $ \y -> with $ \z ->
    f (Cursor x y z)
  {-# INLINE with #-}

data InsideBlock = NotInBlock | InsideBlock
  deriving stock (Show, Generic)

instance ToBinary InsideBlock where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary InsideBlock where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}
