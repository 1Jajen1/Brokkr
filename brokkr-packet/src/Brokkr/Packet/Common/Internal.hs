{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
module Brokkr.Packet.Common.Internal (
  Location(..)
, pattern Location
, Difficulty(..)
, BitSet(..)
, WindowId(..)
, ButtonId(..)
, pattern EnchantmentTableButton
, pattern LecternButton
, StateId(..)
, SlotIndex(..)
, IndexedSlot(..)
, Slot(..)
, ItemId(..)
, ItemCount(..)
, ItemMetadata(..)
, Identifier(..), mkIdentifier
, PageText(..), mkPageText
, BookTitle(..), mkBookTitle
, EntityId(..)
, IsSneaking(..)
, Hand(..)
, Position(..)
, Rotation(..)
, OnGround(..)
, SequenceId(..)
, SessionId(..)
, ExpiresAt(..)
, PublicKey(..)
, KeySignature(..)
, RecipeId(..), mkRecipeId
, ItemName(..), mkItemName
, HasEffect(..)
, PotionEffect(..)
, Chat(..), mkChat
, VerifyToken(..)
, Username(..), mkUsername
, UserUUID(..)
, MessageId(..)
, TeleportId(..)
) where

import Brokkr.NBT (NBT)
import Brokkr.NBT qualified as NBT
import Brokkr.NBT.Codec

import Brokkr.Packet.Binary
import Brokkr.Packet.SizePrefixed
import Brokkr.Packet.MCString

import Data.Bits
import Data.ByteString (ByteString)
import Data.Coerce
import Data.Int
import Data.Text (Text)

import Data.Vector.Storable qualified as S

import Data.Word

import FlatParse.Basic qualified as Flatparse

import GHC.Exts
import GHC.Generics (Generic)

newtype Location = UnsafeLocation Int64
  deriving newtype (Eq, ToBinary, FromBinary)

-- | Slightly unsafe, because it is assumed that the position is valid:
-- This means x,z fit into 26 bits and y fits into 12 bits
pattern Location :: Int -> Int -> Int -> Location
pattern Location x y z <- (decodeLocation -> (x,y,z))
  where Location x y z = UnsafeLocation $ (fromIntegral x `unsafeShiftL` 38) .|. fromIntegral y .|. ((fromIntegral z .&. 0x3FFFFFF) `unsafeShiftL` 12)
{-# INLINE Location #-}
{-# COMPLETE Location #-}

decodeLocation :: Location -> (Int, Int, Int)
{-# INLINE decodeLocation #-}
decodeLocation (UnsafeLocation i) =
  ( fromIntegral $ i `unsafeShiftR` 38
  , fromIntegral $ i .&. 0xFFF
  , fromIntegral $ (i `unsafeShiftR` 12) .&. 0x3FFFFFF
  )

instance Show Location where
  showsPrec prec (Location x y z) = showParen (prec > 10) $
      showString "Location "
    . showsPrec prec (x,y,z)

data Difficulty = Peaceful | Easy | Normal | Hard
  deriving stock (Show, Generic)

instance ToBinary Difficulty where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary Difficulty where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

newtype BitSet = BitSet (S.Vector Word64)
  deriving newtype Eq
  deriving FromBinary via SizePrefixed VarInt (S.Vector Word64)

instance ToBinary BitSet where
  put (BitSet v)
    | S.all (== 0) v = put (VarInt 0)
    | otherwise      = put (SizePrefixed @VarInt v)

instance Show BitSet where
  show (BitSet b) = show b
  -- show (BitSet vec) = S.foldMap (\el ->
  --   reverse $ goOne 0 (fromIntegral el) []
  --   ) vec
  --   where
  --     goOne :: Int -> Word -> String -> String
  --     goOne !n !i acc
  --       | n >= 64 = acc
  --       | otherwise = case i .&. 1 of
  --         0 -> '0' : acc'
  --         1 -> '1' : acc'
  --         _ -> error "BitSet.Show.goOne: Unreachable"
  --       where acc' = goOne (n + 1) (i `unsafeShiftR` 1) acc

newtype WindowId = WindowId Int8
  deriving stock Show
  deriving newtype (ToBinary, FromBinary)

newtype ButtonId = UnsafeButtonId Int8
  deriving stock Show
  deriving newtype (ToBinary, FromBinary)

data EnchantmentTableButton = EnchantingTop | EnchantingMiddle | EnchantingBottom

pattern EnchantmentTableButton :: EnchantmentTableButton -> ButtonId
pattern EnchantmentTableButton x
  <- ((\(UnsafeButtonId n) -> case n of
      0 -> EnchantingTop
      1 -> EnchantingMiddle
      2 -> EnchantingBottom
      _ -> error $ "Invalid button id for enchantment tables: " <> show n
     ) -> x)
  where
    EnchantmentTableButton EnchantingTop    = UnsafeButtonId 0
    EnchantmentTableButton EnchantingMiddle = UnsafeButtonId 1
    EnchantmentTableButton EnchantingBottom = UnsafeButtonId 2
{-# INLINE EnchantmentTableButton #-}
{-# COMPLETE EnchantmentTableButton #-}

data LecternButton = LecternPreviousPage | LecternNextPage | LecternTakeBook | LecternPage !Int8

pattern LecternButton :: LecternButton -> ButtonId
pattern LecternButton x
  <- ((\(UnsafeButtonId n) -> case n of
      0 -> LecternPreviousPage
      1 -> LecternNextPage
      2 -> LecternTakeBook
      _ -> if n > 100
        then LecternPage (100 - n)
        else error $ "Invalid button id for lecterns: " <> show n
     ) -> x)
  where
    LecternButton LecternPreviousPage = UnsafeButtonId 0
    LecternButton LecternNextPage     = UnsafeButtonId 1
    LecternButton LecternTakeBook     = UnsafeButtonId 2
    LecternButton (LecternPage n)     = UnsafeButtonId $ 100 + n
{-# INLINE LecternButton #-}
{-# COMPLETE LecternButton #-}

-- TODO Stonecutter and loom

newtype StateId = StateId Int32
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via VarInt

-- | 'enc' exists because of inconsistencies
newtype SlotIndex enc = UnsafeSlotIndex Int16
  deriving stock Show
  deriving newtype Eq

instance ToBinary (SlotIndex VarInt) where
  put (UnsafeSlotIndex i) = put (VarInt $ fromIntegral i)
  {-# INLINE put #-}
instance FromBinary (SlotIndex VarInt) where
  with f = with $ \(VarInt n) -> f (UnsafeSlotIndex $ fromIntegral n)
  {-# INLINE with #-}

deriving via Int16 instance (ToBinary (SlotIndex Int16))
deriving via Int16 instance (FromBinary (SlotIndex Int16))

data IndexedSlot = IndexedSlot !(SlotIndex Int16) !Slot
  deriving stock (Eq, Show)

instance ToBinary IndexedSlot where
  put (IndexedSlot si s) = put si <> put s
  {-# INLINE put #-}

instance FromBinary IndexedSlot where
  with f = with $ \si -> with $ \s -> f (IndexedSlot si s)
  {-# INLINE with #-}

data Slot = EmptySlot | Slot !ItemId !ItemCount !ItemMetadata
  deriving stock (Eq, Show)

instance ToBinary Slot where
  put EmptySlot = put False
  put (Slot itemId itemCount meta) = put itemId <> put itemCount <> put meta
  {-# INLINE put #-}

instance FromBinary Slot where
  get = with $ \case
    False -> pure EmptySlot
    True -> Slot <$> get <*> get <*> get 
  {-# INLINE get #-}

newtype ItemId = ItemId Int32
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via VarInt

newtype ItemCount = ItemCount Int8
  deriving stock Show
  deriving newtype Eq
  deriving newtype (ToBinary, FromBinary)

-- TODO Better type here?
newtype ItemMetadata = ItemMetadata NBT
  deriving stock Show
  deriving newtype Eq
  deriving newtype (ToBinary, FromBinary)

newtype Identifier = UnsafeIdentifier Text
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via MCString 32767

instance HasCodec Identifier where
  -- TODO Validate
  codec = dimapCodec
    [|| UnsafeIdentifier . NBT.toText ||]
    [|| \(UnsafeIdentifier t) -> NBT.fromText t ||]
    $ codec @NBT.NBTString

-- TODO Validate NameSpace [a-z0-9.-_] : Value [a-z0-9.-_/]
-- Don't forget the FromBinary instance

mkIdentifier :: Text -> Maybe Identifier
mkIdentifier = coerce . mkMCString @32767

newtype PageText = UnsafePageText Text
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via MCString 8192

mkPageText :: Text -> Maybe PageText
mkPageText = coerce . mkMCString @8192

newtype BookTitle = UnsafeBookTitle Text
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via MCString 128

mkBookTitle :: Text -> Maybe BookTitle
mkBookTitle = coerce . mkMCString @8192

-- | 'enc' exists because minecraft is *very* inconsistent with how
-- entity id's are encoded. Either VarInt or Int32
newtype EntityId enc = EntityId Int32
  deriving stock Show
  deriving newtype Eq

deriving via VarInt instance (ToBinary (EntityId VarInt))
deriving via VarInt instance (FromBinary (EntityId VarInt))

deriving via Int32 instance (ToBinary (EntityId Int32))
deriving via Int32 instance (FromBinary (EntityId Int32))

data IsSneaking = NotSneaking | Sneaking
  deriving stock (Eq, Show, Generic)

instance ToBinary IsSneaking where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary IsSneaking where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

data Hand = MainHand | OffHand
  deriving stock (Eq, Show, Generic)

instance ToBinary Hand where
  put = putEnum @VarInt
  {-# INLINE put #-}

instance FromBinary Hand where
  with = withEnum @VarInt (\w -> tagToEnum# w)
  {-# INLINE with #-}

data Position = Position !Double !Double !Double
  deriving stock (Eq, Show)

instance ToBinary Position where
  put (Position x y z) = put x <> put y <> put z
  {-# INLINE put #-}

instance FromBinary Position where
  with f = with $ \x -> with $ \y -> with $ \z ->
    f (Position x y z)
  {-# INLINE with #-}

data Rotation = Rotation { yaw :: !Float, pitch :: !Float }
  deriving stock (Eq, Show)

instance ToBinary Rotation where
  put (Rotation yaw pitch) = put yaw <> put pitch
  {-# INLINE put #-}

instance FromBinary Rotation where
  with f = with $ \yaw -> with $ \pitch ->
    f (Rotation yaw pitch)
  {-# INLINE with #-}

data OnGround = OffGround | OnGround
  deriving stock (Eq, Show, Generic)

instance ToBinary OnGround where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary OnGround where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

newtype SequenceId = SequenceId Int32
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via VarInt

newtype SessionId = SessionId UUID
  deriving stock Show
  deriving newtype Eq
  deriving newtype (ToBinary, FromBinary)

newtype ExpiresAt = ExpiresAt Int64
  deriving stock Show
  deriving newtype Eq
  deriving newtype (ToBinary, FromBinary)

newtype PublicKey = PublicKey ByteString
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via SizePrefixed VarInt ByteString

instance Show PublicKey where
  showsPrec prec (PublicKey bs) = showParen (prec > 10) $
    showString "PublicKey " . showsPrec 11 (HexBS bs)

newtype KeySignature = KeySignature ByteString 
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via SizePrefixed VarInt ByteString

instance Show KeySignature where
  showsPrec prec (KeySignature bs) = showParen (prec > 10) $
    showString "KeySignature " . showsPrec 11 (HexBS bs)

newtype RecipeId = UnsafeRecipeId { unRecipeId :: Text }
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via Identifier

mkRecipeId :: Text -> Maybe RecipeId
mkRecipeId = coerce . mkIdentifier

newtype ItemName = UnsafeItemName Text
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via MCString 32767

mkItemName :: Text -> Maybe ItemName
mkItemName = coerce . mkMCString @32767

data HasEffect = NoEffect | HasEffect
  deriving stock (Eq, Show, Generic)

instance ToBinary HasEffect where
  put = putEnum @Int8
  {-# INLINE put #-}

instance FromBinary HasEffect where
  with = withEnum @Int8 (\w -> tagToEnum# w)
  {-# INLINE with #-}

-- TODO Generate pattern synonyms for the effects in the registry package
-- TODO Do the same for the other registries
newtype PotionEffect = PotionEffect Int32
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via VarInt

-- TODO More extensive interface here
newtype Chat = UnsafeChat { unChat :: Text }
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via MCString 262144

mkChat :: Text -> Maybe Chat
mkChat = coerce . mkMCString @262144

newtype VerifyToken = VerifyToken ByteString
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via SizePrefixed VarInt ByteString

instance Show VerifyToken where
  showsPrec prec (VerifyToken bs) = showParen (prec > 10) $
    showString "VerifyToken " . showsPrec 11 (HexBS bs)

newtype Username = UnsafeUsername { unUsername :: Text }
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via MCString 16

mkUsername :: Text -> Maybe Username
mkUsername = coerce . mkMCString @16

newtype UserUUID = UserUUID UUID
  deriving stock Show
  deriving newtype Eq
  deriving newtype (ToBinary, FromBinary)

newtype MessageId = MessageId Int32
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via VarInt

newtype TeleportId = TeleportId Int32
  deriving stock Show
  deriving newtype Eq
  deriving (ToBinary, FromBinary) via VarInt
