{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MagicHash #-}
module Brokkr.NBT.Class (
  FromNBT(..)
, ToNBT(..)
, NBTParser(runNBTParser#), runParser
, withCompound
, (.:), (.:?)
, compound, (.=)
) where

import Brokkr.NBT.NBTError
import Brokkr.NBT.NBTString
import Brokkr.NBT.Internal
import Brokkr.NBT.ByteOrder

import Data.Foldable
import Data.Functor
import Data.Int
import Data.Text (Text)

import Data.Vector qualified as V
import Data.Vector.Storable qualified as S

import Data.Primitive.SmallArray

import Brokkr.NBT.Slice

-- | Class based 'NBT' encoding
--
-- Encode any type to the intermediate 'NBT' structure
--
-- Encodes into 'Tag' instead of 'NBT' as not all applications
-- need or want an empty top level 'NBT'.
class ToNBT a where
  toNBT :: a -> Tag

instance ToNBT Int8 where
  toNBT = TagByte

instance ToNBT Int16 where
  toNBT = TagShort

instance ToNBT Int32 where
  toNBT = TagInt

instance ToNBT Int64 where
  toNBT = TagLong

instance ToNBT Float where
  toNBT = TagFloat

instance ToNBT Double where
  toNBT = TagDouble

instance ToNBT NBTString where
  toNBT = TagString

instance ToNBT (S.Vector Int8) where
  toNBT = TagByteArray

instance ToNBT (S.Vector (BigEndian Int32)) where
  toNBT = TagIntArray

instance ToNBT (S.Vector (BigEndian Int64)) where
  toNBT = TagLongArray

-- | Copies and byteswaps the vector
instance ToNBT (S.Vector Int32) where
  toNBT = toNBT . arrSwapBE

-- | Copies and byteswaps the vector
instance ToNBT (S.Vector Int64) where
  toNBT = toNBT . arrSwapBE

instance ToNBT (SmallArray Tag) where
  toNBT = TagList

instance ToNBT a => ToNBT (SmallArray a) where
  toNBT = TagList . fmap toNBT

instance ToNBT (Slice NBT) where
  toNBT = TagCompound

instance ToNBT a => ToNBT (V.Vector a) where
  toNBT v = TagList . smallArrayFromListN (V.length v) . fmap toNBT $ V.toList v

instance ToNBT Tag where
  toNBT = id

-- | Run an 'NBTParser' over a 'Tag'
--
-- Usually the parser is obtained from 'FromNBT'
runParser :: NBTParser a -> Tag -> Either NBTError a
runParser (NBTParser f) t = case f t of
  (# res | #) -> Right res
  (# | e #) -> Left e 

-- | Parser over 'Tag'
--
-- Based on 'Aeson'.
newtype NBTParser a = NBTParser { runNBTParser# :: Tag -> (# a | NBTError #) }

err :: NBTError -> NBTParser a
err e = NBTParser $ \_ -> (# | e #)

instance Functor NBTParser where
  fmap f (NBTParser g) = NBTParser $ \t -> case g t of
    (# a | #) -> (# f a | #)
    (# | e #) -> (# | e #)

instance Applicative NBTParser where
  pure a = NBTParser $ \_ -> (# a | #)
  NBTParser ff <*> NBTParser fa = NBTParser $ \t -> case ff t of
    (# | e #) -> (# | e #)
    (# f | #) -> case fa t of
      (# | e #) -> (# | e #)
      (# a | #) -> (# f a | #) 

instance Monad NBTParser where
  NBTParser g >>= f = NBTParser $ \t -> case g t of
    (# | e #) -> (# | e #)
    (# a | #) -> case f a of
      NBTParser h -> h t

-- | Class based 'NBT' decoding
class FromNBT a where
  -- | Given a name and a 'Tag', decode into a domain object
  fromNBT :: Text -> Tag -> NBTParser a

instance FromNBT Int8 where
  fromNBT name = \case
    TagByte b -> pure b
    _ -> err $ InvalidType name

instance FromNBT Int16 where
  fromNBT name = \case
    TagShort s -> pure s
    _ -> err $ InvalidType name

instance FromNBT Int32 where
  fromNBT name = \case
    TagInt i -> pure i
    _ -> err $ InvalidType name

instance FromNBT Int64 where
  fromNBT name = \case
    TagLong l -> pure l
    _ -> err $ InvalidType name

instance FromNBT Float where
  fromNBT name = \case
    TagFloat f -> pure f
    _ -> err $ InvalidType name

instance FromNBT Double where
  fromNBT name = \case
    TagDouble d -> pure d
    _ -> err $ InvalidType name

instance FromNBT NBTString where
  fromNBT name = \case
    TagString str -> pure str
    _ -> err $ InvalidType name

instance FromNBT (S.Vector Int8) where
  fromNBT name = \case
    TagByteArray v -> pure v
    _ -> err $ InvalidType name

instance FromNBT (S.Vector (BigEndian Int32)) where
  fromNBT name = \case
    TagIntArray v -> pure v
    _ -> err $ InvalidType name

instance FromNBT (S.Vector (BigEndian Int64)) where
  fromNBT name = \case
    TagLongArray v -> pure v
    _ -> err $ InvalidType name


-- | Copies and byteswaps the vector
instance FromNBT (S.Vector Int32) where
  fromNBT name t = arrSwapBE <$> fromNBT @(S.Vector (BigEndian Int32)) name t


-- | Copies and byteswaps the vector
instance FromNBT (S.Vector Int64) where
  fromNBT name t = arrSwapBE <$> fromNBT @(S.Vector (BigEndian Int64)) name t

instance FromNBT (SmallArray Tag) where
  fromNBT name = \case
    TagList xs -> pure xs
    _ -> err $ InvalidType name

instance FromNBT a => FromNBT (SmallArray a) where
  fromNBT name = \case
    TagList xs -> traverse (fromNBT name) xs
    _ -> err $ InvalidType name

instance FromNBT a => FromNBT (V.Vector a) where
  fromNBT name t = fromNBT name t <&> \xs -> V.fromListN (sizeofSmallArray xs) $ toList xs

instance FromNBT (Slice NBT) where
  fromNBT name = \case
    TagCompound c -> pure c
    _ -> err $ InvalidType name

instance FromNBT Tag where
  fromNBT _ = pure

-- | Create a domain object from a compound tag
--
-- Fails if not given a compound
--
-- @
--    data MyObj = MyObj Int (Maybe Byte)
--
--    withCompound "name" $ \c -> do
--      arg1 <- c .:  "arg1"
--      arg2 <- c .:? "arg2"
--      pure $ MyObj arg1 arg2
-- @
withCompound :: Text -> (Slice NBT -> NBTParser a) -> Tag -> NBTParser a
withCompound name f = \case
  TagCompound slice -> f slice
  _ -> err $ InvalidType name

-- | Given a compound slice, a key and a type, parse an object from the 'Tag' with the given required key
(.:) :: FromNBT a => Slice NBT -> NBTString -> NBTParser a
!m .: !k = case findWithIndex (\(NBT key _) -> key) k m of
  (# _, (# NBT _ t | #) #) -> fromNBT (toText k) t
  (# _, (# | (#  #) #) #) -> err $ MissingKey $ toText k

-- | Given a compound slice, a key and a type, parse an object from the 'Tag' with the given optional key
(.:?) :: FromNBT a => Slice NBT -> NBTString -> NBTParser (Maybe a)
m .:? k = case findWithIndex (\(NBT key _) -> key) k m of
  (# _, (# NBT _ t | #) #) -> Just <$> fromNBT (toText k) t
  (# _, (# | _ #) #) -> pure Nothing

-- | Create a compound tag from a key value list
compound :: [(NBTString, Tag)] -> Tag
compound = TagCompound . fromList . fmap (uncurry NBT)

-- | Create a key value pair with any value that can be converted to 'NBT'
(.=) :: ToNBT a => NBTString -> a -> (NBTString, Tag)
!k .= !v = (k, toNBT v)

infixr 8 .=
