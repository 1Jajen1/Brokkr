{-# LANGUAGE MagicHash, UnboxedTuples #-}
module Util.NBT.Internal (
  NBT(..)
, Tag(..)
, NBTString(..)
, NBTParser(..)
, FromNBT(..)
, ToNBT(..)
, withCompound
, (.:), (.:?), (.!=)
, compound, (.=)
) where

import Brokkr.NBT hiding (parseNBT, compound, (.=))
import Brokkr.NBT.Internal hiding (parseNBT)
import Brokkr.NBT.NBTString.Internal
import Brokkr.NBT.Slice (Slice)
import qualified Brokkr.NBT.Slice as Slice

import Data.Int
import Data.Text hiding (empty)
import qualified Data.Vector.Storable as S
import qualified Data.Vector as V
import qualified Data.Text.Encoding as T
import Control.Monad
import Prelude hiding (succ)
import Control.Applicative
import Data.Primitive (sizeofSmallArray, smallArrayFromListN)
import Data.Foldable
import Data.Maybe
import GHC.Exts hiding (toList)


-- | Custom Parser for NBT values
--
-- CPS'd parser, the first argument is the success continuation, the second the abort continuation
-- Example: runParser p Just Nothing
-- TODO: Test this against runParser :: forall {r} :: RuntimeRep, a :: TYPE' r . Tag -> (# a | #)
newtype NBTParser a = NBTParser { runParser :: forall r . (a -> r) -> r -> Tag -> r }

-- TODO Test laws.
instance Functor NBTParser where
  fmap f p = NBTParser $ \succ absent tag -> runParser p (succ . f) absent tag
  {-# INLINE fmap #-}

instance Applicative NBTParser where
  pf <*> pa = NBTParser $ \succ absent tag -> runParser pf (\f -> runParser pa (succ . f) absent tag) absent tag
  {-# INLINE (<*>) #-}
  pure a = NBTParser $ \succ _ _ -> succ a
  {-# INLINE pure #-}

instance Monad NBTParser where
  pa >>= f = NBTParser $ \succ absent tag -> runParser pa (\a -> runParser (f a) succ absent tag) absent tag
  {-# INLINE (>>=) #-}

instance Alternative NBTParser where
  empty = NBTParser $ \_ absent _ -> absent
  {-# INLINE empty #-}
  p1 <|> p2 = NBTParser $ \succ absent tag -> runParser p1 succ (runParser p2 succ absent tag) tag
  {-# INLINE (<|>) #-}

instance MonadPlus NBTParser where

{- Note: [Why no default implementations via GHC.Generics?]

Those are definitely possible however they slow down compile time quite a bit, are not as fast as handwritten instances and
couple data to NBT format a little too much for my liking.

If this ever gets its own library then I'll probably add it together with th based deriving and more, but for now this will suffice as
all instances will be handwritten!

-}

-- | A type which can be converted from NBT, but could also fail.
--
-- Use helpers such as 'withCompound' and '(.:)' to implement this class.
class FromNBT a where
  parseNBT :: Tag -> NBTParser a

instance FromNBT Tag where
  parseNBT = pure
  {-# INLINE parseNBT #-}

instance FromNBT Bool where
  parseNBT = \case
    TagByte 0 -> pure False
    TagByte 1 -> pure True
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT Int8 where
  parseNBT = \case
    TagByte n -> pure n
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT Int16 where
  parseNBT = \case
    TagShort s -> pure s
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT Int32 where
  parseNBT = \case
    TagInt i -> pure i
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT Int64 where
  parseNBT = \case
    TagLong i -> pure i
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT Float where
  parseNBT = \case
    TagFloat f -> pure f
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT Double where
  parseNBT = \case
    TagDouble d -> pure d
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT (S.Vector Int8) where
  parseNBT = \case
    TagByteArray arr -> pure arr
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT Text where
  parseNBT = \case
    TagString (NBTString str) -> pure $ T.decodeUtf8 str -- TODO Decode from java cesu8 instead
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT a => FromNBT (V.Vector a) where
  parseNBT = \case
    TagList xs -> traverse parseNBT $ V.fromListN (sizeofSmallArray xs) $ toList xs
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT (S.Vector Int32BE) where
  parseNBT = \case
    TagIntArray arr -> pure arr
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT (S.Vector Int64BE) where
  parseNBT = \case
    TagLongArray arr -> pure arr
    _ -> empty
  {-# INLINE parseNBT #-}

-- | A type which can be converted to NBT.
--
-- Use helpers such as 'compound' and '(.=)' to implement this class.
class ToNBT a where
  toNBT :: a -> Tag
  -- TODO Think about what it takes to implement toEncoding similar to Aeson, the problem is that NBT is TagId Name Tag and JSON is Name Value
  -- this means in NBT we have to know the name before we can write the tag, this is annoying ^-^ The main problem is getting the tagid without creating a Tag
  -- like I could have two methods a -> TagId and a -> Builder but that just gets annoying...
  -- TODO Think about this some more once I have benchmarks, not worth doing this without proof that it is faster (tho it likely is but meh)
  -- Also NBT encoding usually happens for writing to files not network, so we don't need max perf anyway, it just has to be fast enough

instance ToNBT Tag where
  toNBT = id
  {-# INLINE toNBT #-}

instance ToNBT Bool where
  toNBT False = TagByte 0
  toNBT True = TagByte 1
  {-# INLINE toNBT #-}

instance ToNBT Int8 where
  toNBT = TagByte
  {-# INLINE toNBT #-}

instance ToNBT Int16 where
  toNBT = TagShort
  {-# INLINE toNBT #-}

instance ToNBT Int32 where
  toNBT = TagInt
  {-# INLINE toNBT #-}

instance ToNBT Int64 where
  toNBT = TagLong
  {-# INLINE toNBT #-}

instance ToNBT Float where
  toNBT = TagFloat
  {-# INLINE toNBT #-}

instance ToNBT Double where
  toNBT = TagDouble
  {-# INLINE toNBT #-}

instance ToNBT (S.Vector Int8) where
  toNBT = TagByteArray
  {-# INLINE toNBT #-}

instance ToNBT Text where
  toNBT = TagString . NBTString . T.encodeUtf8 -- TODO encode cesu8 instead
  {-# INLINE toNBT #-}

instance ToNBT a => ToNBT (V.Vector a) where
  toNBT xs = TagList $ fmap toNBT $ smallArrayFromListN (V.length xs) $ toList xs
  {-# INLINE toNBT #-} 

instance ToNBT (S.Vector Int32BE) where
  toNBT = TagIntArray
  {-# INLINE toNBT #-}

instance ToNBT (S.Vector Int64BE) where
  toNBT = TagLongArray
  {-# INLINE toNBT #-}

-- Utilities for implementing FromNBT

-- | Create a 'NBTParser' which applies the inner function only if given a compound tag.
--
-- The resulting 'NBTParser' will only succeed if the given 'Tag' is a compound and the
-- 'NBTParser' returned by the function succeeds.
withCompound :: (Slice NBT -> NBTParser a) -> Tag -> NBTParser a
withCompound f = \case
  TagCompound m -> f m
  _ -> empty
{-# INLINE withCompound #-}

-- | Create a parser which tries to read a 'Tag' from a compound.
--
-- Succeeds only if the key is in the compound and the 'NBTParser' for 'a' given the value for
-- the key succeeds.
--
-- If you need the key to be optional use '(.:?)' instead.
(.:) :: FromNBT a => Slice NBT -> NBTString -> NBTParser a
!m .: !k = case findWithIndexNBT k m of
  (# _, (# NBT _ t | #) #) -> parseNBT t
  (# _, (# | _ #) #) -> empty
{-# INLINE (.:) #-}

findWithIndexNBT :: NBTString -> Slice NBT -> (# Int#, (# NBT | (# #) #) #)
findWithIndexNBT = Slice.findWithIndex(\(NBT k _) -> k) 

-- | Create a parser which tries to read a 'Tag' from a compound.
--
-- Succeeds with Nothing if the key is not present, otherwise behaves like '(.:)'
--
-- If you want to provide a default value use '(.!=)'
(.:?) :: FromNBT a => Slice NBT -> NBTString -> NBTParser (Maybe a)
m .:? k = case findWithIndexNBT k m of
  (# _, (# NBT _ t | #) #) -> Just <$> parseNBT t
  (# _, (# | _ #) #) -> pure Nothing
{-# INLINE (.:?) #-}

-- | Provide a default value to a 'NBTParser' returning a 'Maybe'.
--
-- Only provides the default if the initial parser succeeded with
-- Nothing as a result.
(.!=) :: NBTParser (Maybe a) -> a -> NBTParser a
!p .!= !def = fromMaybe def <$> p
{-# INLINE (.!=) #-}

-- | Create a compound tag from a key-value list.
--
-- Use '(.=)' to simplify building such a list.
compound :: [(NBTString, Tag)] -> Tag
compound = TagCompound . Slice.fromList . fmap (uncurry NBT)
{-# INLINE compound #-}

-- | Create a key value pair with any value that can be converted to 'NBT'
(.=) :: ToNBT a => NBTString -> a -> (NBTString, Tag)
!k .= !v = (k, toNBT v)
{-# INLINE (.=) #-}

infixr 8 .=
