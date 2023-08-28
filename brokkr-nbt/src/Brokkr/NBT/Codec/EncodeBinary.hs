{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module Brokkr.NBT.Codec.EncodeBinary (
  genBuilder
) where

import Brokkr.NBT.NBTString.Internal

import Brokkr.NBT.Internal
import Brokkr.NBT.Codec.Internal

import Data.ByteString.Internal qualified as BS

import Data.Coerce
import Data.Int
import Data.Primitive.SmallArray

import Data.Vector.Storable qualified as S
import Foreign.Storable qualified as S

import Language.Haskell.TH qualified as TH

import Mason.Builder qualified as Mason

-- | Generate a builder which encodes to binary 'NBT'
genBuilder :: NBTCodec Value i0 o0 -> TH.Q TH.Exp
genBuilder c = [| \a -> Mason.int8 ($(extractTag c) a) <> putNBTString (NBTString mempty) <> $(go c) a |]
  where
    go :: NBTCodec ctx i o -> TH.Q TH.Exp
    go TagCodec      = [| putTag |]
    go ByteCodec{}   = [| Mason.int8     |]
    go ShortCodec{}  = [| Mason.int16BE  |]
    go IntCodec{}    = [| Mason.int32BE  |]
    go LongCodec{}   = [| Mason.int64BE  |]
    go FloatCodec{}  = [| Mason.floatBE  |]
    go DoubleCodec{} = [| Mason.doubleBE |]
    go StringCodec{} = [| putNBTString   |]
    go ByteArrayCodec{} = [| \v -> putArray v |]
    go IntArrayCodec{}  = [| \v -> putArray v |]
    go LongArrayCodec{} = [| \v -> putArray v |]
    go (RmapCodec _ i)       = go i
    go (RmapEitherCodec _ i) = go i
    go (LmapCodec f i)       = [| \x -> $(go i) ($(TH.unTypeCode f) x) |]
    
    go (ListCodec _ inner)   = goInner inner
      where
        goInner :: NBTCodec Value i o -> TH.Q TH.Exp
        goInner i = case i of
          ByteCodec _   -> [| \v -> Mason.int8 1 <> putArray v |]
          ShortCodec _  -> [| \v -> Mason.int8 2 <> putArray v |]
          IntCodec _    -> [| \v -> Mason.int8 3 <> putArray v |]
          LongCodec _   -> [| \v -> Mason.int8 4 <> putArray v |]
          FloatCodec _  -> [| \v -> Mason.int8 5 <> putArray v |]
          DoubleCodec _ -> [| \v -> Mason.int8 6 <> putArray v |]
          StringCodec _ -> [| \v -> Mason.int8 8 <> Mason.int32BE (fromIntegral $ sizeofSmallArray v) <> foldMap (\x -> putNBTString x) v |]
          ByteArrayCodec _ -> [| \v -> Mason.int8 7  <> Mason.int32BE (fromIntegral $ sizeofSmallArray v) <> foldMap (\x -> putArray x) v |]
          IntArrayCodec _  -> [| \v -> Mason.int8 11 <> Mason.int32BE (fromIntegral $ sizeofSmallArray v) <> foldMap (\x -> putArray x) v |]
          LongArrayCodec _ -> [| \v -> Mason.int8 12 <> Mason.int32BE (fromIntegral $ sizeofSmallArray v) <> foldMap (\x -> putArray x) v |]
          ListCodec _ i' -> [| \v -> Mason.int8 9 <> Mason.int32BE (fromIntegral $ sizeofSmallArray v) <> foldMap (\x -> $(goInner i')) v |]
          CompoundCodec _ i' -> [| \v -> Mason.int8 10 <> Mason.int32BE (fromIntegral $ sizeofSmallArray v) <> foldMap (\a -> $(go i') a <> Mason.int8 0) v |]
          RmapCodec _ i' -> goInner i'
          RmapEitherCodec _ i' -> goInner i'
          LmapCodec f i' -> error "TODO Lmap list"

    go (CompoundCodec _ inner) = [| \a -> $(go inner) a <> Mason.int8 0 |]

    go (PureCodec _)   = [| const mempty |]
    go (ApCodec ff fa) = [| \x -> $(go ff) x <> $(go fa) x |]
    go (RequiredKeyCodec key i _) = [| \a -> Mason.int8 ($(extractTag i) a) <> putNBTString key <> $(go i) a |]
    go (OptionalKeyCodec key i _) = [| \case
      Nothing -> mempty
      Just x  -> Mason.int8 ($(extractTag i) x) <> putNBTString key <> $(go i) x
      |]

    extractTag :: NBTCodec Value i o -> TH.Q TH.Exp
    extractTag ByteCodec{}      = [| const 1  |]
    extractTag ShortCodec{}     = [| const 2  |]
    extractTag IntCodec{}       = [| const 3  |]
    extractTag LongCodec{}      = [| const 4  |]
    extractTag FloatCodec{}     = [| const 5  |]
    extractTag DoubleCodec{}    = [| const 6  |]
    extractTag ByteArrayCodec{} = [| const 7  |]
    extractTag StringCodec{}    = [| const 8  |]
    extractTag ListCodec{}      = [| const 9  |]
    extractTag CompoundCodec{}  = [| const 10 |]
    extractTag IntArrayCodec{}  = [| const 11 |]
    extractTag LongArrayCodec{} = [| const 12 |]
    extractTag TagCodec{}       = [| \t -> fromIntegral (getTagId t) |]
    extractTag (RmapCodec _ i)  = extractTag i
    extractTag (RmapEitherCodec _ i)
                                = extractTag i
    extractTag (LmapCodec _ i)  = extractTag i

putArray :: forall a . S.Storable a => S.Vector a -> Mason.Builder
{-# INLINE putArray #-}
putArray v = Mason.int32BE (fromIntegral sz) <> Mason.byteString (BS.BS (coerce fp) $ sz * S.sizeOf (undefined :: a))
  where (fp, sz) = S.unsafeToForeignPtr0 v

