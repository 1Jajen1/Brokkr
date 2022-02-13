{-# LANGUAGE MagicHash  #-}
module Util.NBT.Internal (
  NBT(..)
, Tag(..)
, tagId
, tagFromId
, NBTString(..)
, NBTParser(..)
, FromNBT(..)
, ToNBT(..)
, withCompound
, (.:), (.:?), (.!=)
, compound, (.=)
) where

import Data.Int
import Data.Text hiding (empty)
import qualified Data.Vector.Storable as S
import qualified Data.Vector as V
import Data.Map.Strict hiding (empty)
import GHC.Exts ( Int(I#), dataToTag#, coerce )
import Util.Binary
import FlatParse.Basic hiding ((<|>), empty, runParser)
import qualified FlatParse.Basic hiding ((<|>))
import Data.Void
import qualified Mason.Builder as B
import Data.Word
import qualified Data.Text.Encoding as T
import Util.Flatparse
import qualified Data.ByteString as BS
import Control.Monad
import Prelude hiding (succ)
import Control.Applicative
import qualified Data.Map.Strict as Map
import Data.Maybe

data Tag =
    TagEnd
  | TagByte !Int8
  | TagShort !Int16
  | TagInt !Int32
  | TagLong !Int64
  | TagFloat !Float
  | TagDouble !Double
  | TagByteArray !(S.Vector Int8) 
  | TagString !Text
  | TagList !(V.Vector Tag)
  | TagCompound !(Map Text Tag)
  | TagIntArray !(S.Vector Int32)
  | TagLongArray !(S.Vector Int64)
  deriving stock Show

tagId :: Tag -> Int
tagId !tag = (I# (dataToTag# tag))

tagFromId :: Int8 -> Parser Void Tag
tagFromId = \case
  0 -> pure TagEnd
  1 -> TagByte <$> get
  2 -> TagShort <$> get
  3 -> TagInt <$> get
  4 -> TagLong <$> get
  5 -> TagFloat <$> get
  6 -> TagDouble <$> get
  7 -> TagByteArray . coerce <$> get @(SizePrefixed Int32 (S.Vector Int8))
  8 -> TagString . coerce <$> get @NBTString
  9 -> TagList <$> do
    tid <- get @Int8
    len <- get @Int32
    V.replicateM (fromIntegral len) (tagFromId tid)
  10 -> TagCompound <$> do
    nbts <- FlatParse.Basic.many get
    tid <- get @Int8
    case tid of
      0 -> pure . fromList $ fmap (\(NBT n t) -> (n, t)) nbts
      _ -> FlatParse.Basic.empty
  11 -> TagIntArray . coerce <$> get @(SizePrefixed Int32 (S.Vector Int32))
  12 -> TagLongArray . coerce <$> get @(SizePrefixed Int32 (S.Vector Int64))
  _ -> FlatParse.Basic.empty

instance ToBinary Tag where
  put tag = case tag of
    TagEnd -> mempty
    TagByte b -> B.int8 b
    TagShort s -> B.int16BE s
    TagInt i -> B.int32BE i
    TagLong l -> B.int64BE l
    TagFloat f -> B.floatBE f
    TagDouble d -> B.doubleBE d
    TagByteArray arr -> put (SizePrefixed @Int32 arr)
    TagString str -> put (NBTString str)
    TagList xs ->
      let len = V.length xs
          tid = fromIntegral $ if len == 0
            then tagId TagEnd
            else tagId $ xs V.! 0
      in B.word8 tid <> put (fromIntegral @_ @Int32 len) <> V.foldMap (\x -> put x) xs
    TagCompound c -> foldMapWithKey (\n t -> put $ NBT n t) c <> B.int8 0
    TagIntArray arr -> put (SizePrefixed @Int32 arr)
    TagLongArray arr -> put (SizePrefixed @Int32 arr)

newtype NBTString = NBTString Text

instance FromBinary NBTString where
  get = do
    len <- get @Word16
    bs <- takeN $ fromIntegral len
    pure . NBTString $ T.decodeUtf8 bs 
  {-# INLINE get #-}

instance ToBinary NBTString where
  put (NBTString str) =
    let bs = T.encodeUtf8 str
        len = BS.length bs
    in put @Word16 (fromIntegral len) <> B.byteString bs

data NBT = NBT !Text !Tag
  deriving stock Show

instance FromBinary NBT where
  get = do
    -- Don't consume the tag id on failure so that the compound parser has an easier time determining if it finished correctly
    -- It uses many (get @NBT) thus when this fails we either have a faulty NBT (in that case the next thing is not TagEnd) or we are
    -- done (in which case we have TagEnd next). This may be problematic if the faulty nbt branch backtracks to right before a 0...
    -- TODO ^-^ 
    tid <- lookahead $ get @Int8
    when (tid == 0) FlatParse.Basic.empty
    _ <- anyWord8

    NBTString name <- get
    tag <- tagFromId tid
    pure $ NBT name tag
  {-# INLINE get #-}

instance ToBinary NBT where
  put (NBT n t) =
    let tid = tagId t
    in B.int8 (fromIntegral tid) <> put (NBTString n) <> put t
  {-# INLINE put #-}

-- Parsing into custom types
newtype NBTParser a = NBTParser { runParser :: forall r . (a -> r) -> r -> Tag -> r }

-- TODO Test laws. TODO Test this whole module...
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

{- Note: Why no default implementations via GHC.Generics?

Those are definitely possible however they slow down compile time quite a bit, are not as fast as handwritten instances and
couple data to NBT format a little too much for my liking.

If this ever gets its own library then I'll probably add it together with th based deriving and more, but for now this will suffice as
all instances will be handwritten!

-}
-- FromNBT
class FromNBT a where
  parseNBT :: Tag -> NBTParser a

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
    TagString str -> pure str
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT a => FromNBT (V.Vector a) where
  parseNBT = \case
    TagList xs -> traverse parseNBT xs
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT (S.Vector Int32) where
  parseNBT = \case
    TagIntArray arr -> pure arr
    _ -> empty
  {-# INLINE parseNBT #-}

instance FromNBT (S.Vector Int64) where
  parseNBT = \case
    TagLongArray arr -> pure arr
    _ -> empty
  {-# INLINE parseNBT #-}

-- To NBT
class ToNBT a where
  toNBT :: a -> Tag
  -- TODO Think about what it takes to implement toEncoding similar to Aeson, the problem is that NBT is TagId Name Tag and JSON is Name Value
  -- this means in NBT we have to know the name before we can write the tag, this is annoying ^-^ The main problem is getting the tagid without creating a Tag
  -- like I could have two methods a -> TagId and a -> Builder but that just gets annoying...
  -- TODO Think about this some more once I have benchmarks, not worth doing this without proof that it is faster (tho it likely is but meh)
  -- Also NBT encoding usually happens for writing to files not network, so we don't need max perf anyway, it just has to be fast enough

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
  toNBT = TagString
  {-# INLINE toNBT #-}

instance ToNBT a => ToNBT (V.Vector a) where
  toNBT xs = TagList $ fmap toNBT xs
  {-# INLINE toNBT #-} 

instance ToNBT (S.Vector Int32) where
  toNBT = TagIntArray
  {-# INLINE toNBT #-}

instance ToNBT (S.Vector Int64) where
  toNBT = TagLongArray
  {-# INLINE toNBT #-}

-- Utilities for implementing FromNBT
withCompound :: (Map Text Tag -> NBTParser a) -> Tag -> NBTParser a
withCompound f = \case
  TagCompound m -> f m
  _ -> empty
{-# INLINE withCompound #-}

(.:) :: FromNBT a => Map Text Tag -> Text -> NBTParser a
m .: k = case Map.lookup k m of
  Just tag -> parseNBT tag
  Nothing -> empty
{-# INLINE (.:) #-}

(.:?) :: FromNBT a => Map Text Tag -> Text -> NBTParser (Maybe a)
m .:? k = case Map.lookup k m of
  Just tag -> Just <$> parseNBT tag
  Nothing -> pure Nothing
{-# INLINE (.:?) #-}

(.!=) :: NBTParser (Maybe a) -> a -> NBTParser a
p .!= def = fromMaybe def <$> p
{-# INLINE (.!=) #-}

compound :: [(Text, Tag)] -> Tag
compound = TagCompound . fromList
{-# INLINE compound #-}

(.=) :: Text -> Tag -> (Text, Tag)
(.=) = (,)
{-# INLINE (.=) #-}
