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

instance ToNBT (S.Vector Int32BE) where
  toNBT = TagIntArray

instance ToNBT (S.Vector Int64BE) where
  toNBT = TagLongArray

instance ToNBT (S.Vector Int32) where
  toNBT = toNBT . arrSwapBE32

instance ToNBT (S.Vector Int64) where
  toNBT = toNBT . arrSwapBE64

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

runParser :: NBTParser a -> Tag -> Either NBTError a
runParser (NBTParser f) t = case f t of
  (# res | #) -> Right res
  (# | e #) -> Left e 

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

class FromNBT a where
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

instance FromNBT (S.Vector Int32BE) where
  fromNBT name = \case
    TagIntArray v -> pure v
    _ -> err $ InvalidType name

instance FromNBT (S.Vector Int64BE) where
  fromNBT name = \case
    TagLongArray v -> pure v
    _ -> err $ InvalidType name

instance FromNBT (S.Vector Int32) where
  fromNBT name t = arrSwapBE32 <$> fromNBT @(S.Vector Int32BE) name t

instance FromNBT (S.Vector Int64) where
  fromNBT name t = arrSwapBE64 <$> fromNBT @(S.Vector Int64BE) name t

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

withCompound :: Text -> (Slice NBT -> NBTParser a) -> Tag -> NBTParser a
withCompound name f = \case
  TagCompound slice -> f slice
  _ -> err $ InvalidType name

(.:) :: FromNBT a => Slice NBT -> NBTString -> NBTParser a
!m .: !k = case findWithIndex (\(NBT key _) -> key) k m of
  (# _, (# NBT _ t | #) #) -> fromNBT (toText k) t
  (# _, (# | (#  #) #) #) -> err $ MissingKey $ toText k

(.:?) :: FromNBT a => Slice NBT -> NBTString -> NBTParser (Maybe a)
m .:? k = case findWithIndex (\(NBT key _) -> key) k m of
  (# _, (# NBT _ t | #) #) -> Just <$> fromNBT (toText k) t
  (# _, (# | _ #) #) -> pure Nothing

compound :: [(NBTString, Tag)] -> Tag
compound = TagCompound . fromList . fmap (uncurry NBT)

-- | Create a key value pair with any value that can be converted to 'NBT'
(.=) :: ToNBT a => NBTString -> a -> (NBTString, Tag)
!k .= !v = (k, toNBT v)

infixr 8 .=
