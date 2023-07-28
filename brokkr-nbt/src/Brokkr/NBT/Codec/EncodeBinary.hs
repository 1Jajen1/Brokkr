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
import Data.Primitive.SmallArray

import Data.Vector.Storable qualified as S

import Language.Haskell.TH qualified as TH

import Mason.Builder qualified as Mason

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
    go ByteArrayCodec{} = [| \v -> case S.unsafeToForeignPtr0 v of (fp, sz) -> Mason.int32BE (fromIntegral sz) <> Mason.byteString (BS.BS (coerce fp) sz      ) |]
    go IntArrayCodec{}  = [| \v -> case S.unsafeToForeignPtr0 v of (fp, sz) -> Mason.int32BE (fromIntegral sz) <> Mason.byteString (BS.BS (coerce fp) $ sz * 4) |]
    go LongArrayCodec{} = [| \v -> case S.unsafeToForeignPtr0 v of (fp, sz) -> Mason.int32BE (fromIntegral sz) <> Mason.byteString (BS.BS (coerce fp) $ sz * 8) |]
    go (RmapCodec _ i)       = go i
    go (RmapEitherCodec _ i) = go i
    go (LmapCodec f i)       = [| \x -> $(go i) ($(TH.unTypeCode f) x) |]
    go (ListCodec _ inner)   = [| \arr ->
      let sz = sizeofSmallArray arr
      in if sz == 0
        then Mason.int8 0 <> Mason.int32BE 0
        else
          let fstTag = indexSmallArray arr 0
          in Mason.int8 ($(extractTag inner) fstTag) <> Mason.int32BE (fromIntegral sz) <> foldMap (\a -> $(go inner) a) arr
      |]
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
