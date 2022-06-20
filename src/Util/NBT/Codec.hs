{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-} -- These will go away in the future anyway
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash  #-}
module Util.NBT.Codec (
  NBTCodec
, pure, (<*>)
, (<$>)
, HasCodec(..)
, requiredField
, parseNBTDirect
) where

import qualified Prelude
import Prelude hiding ((<$>), pure, (<*>))

import Language.Haskell.TH hiding (Code)
import qualified Language.Haskell.TH as TH
import Data.Int
import Data.Vector (Vector)
import Data.Void (Void)
import Util.NBT.Internal
import qualified FlatParse.Basic as FP
import qualified Util.Binary as Binary
import qualified Data.Vector as V
import GHC.Exts (dataToTag#, Int (I#))
import Control.Monad (when)
import Data.Foldable (foldl')
import Data.Bits
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Mutable as MV
import qualified Data.List as List
import Data.Hashable (hash)
import qualified Data.IntSet as IS

-- TODO Complete. I am currently a bit stuck on how to handle compounds here...



data NBTContext = Value | Object

type Code a = TH.Code Q a

-- From autodocodec!
data NBTCodec (ctx :: NBTContext) inp out where
  ByteCodec        :: NBTCodec Value Int8  Int8
  ShortCodec       :: NBTCodec Value Int16 Int16
  IntCodec         :: NBTCodec Value Int32 Int32
  LongCodec        :: NBTCodec Value Int64 Int64
  FloatCodec       :: NBTCodec Value Float Float
  DoubleCodec      :: NBTCodec Value Double Double
  StringCodec      :: NBTCodec Value NBTString NBTString
  ListCodec        :: NBTCodec Value a b -> NBTCodec Value (Vector a) (Vector b)
  CompoundCodec    :: NBTCodec Object a b -> NBTCodec Value a b
  RequiredKeyCodec :: NBTString -> NBTCodec Value a b -> NBTCodec Object a b
  PureCodec        :: Code a -> NBTCodec Object Void a
  ApCodec          :: NBTCodec Object inp (a -> b) -> NBTCodec Object inp a -> NBTCodec Object inp b
  DiMapCodec       :: Code (a -> b) -> Code (c -> d) -> NBTCodec ctx b c -> NBTCodec ctx a d

pure :: Code a -> NBTCodec Object Void a
pure = PureCodec
{-# INLINE pure #-}

(<*>) :: NBTCodec Object inp (a -> b) -> NBTCodec Object inp a -> NBTCodec Object inp b
(<*>) = ApCodec
{-# INLINE (<*>) #-}
 
(<$>) :: Code (a -> b) -> NBTCodec ctx inp a -> NBTCodec ctx inp b
(<$>) f = DiMapCodec [|| id ||] f
{-# INLINE (<$>) #-}

class HasCodec a where
  codec :: NBTCodec Value a a

instance HasCodec Int8 where
  codec = ByteCodec
  {-# INLINE codec #-}
instance HasCodec Int16 where
  codec = ShortCodec
  {-# INLINE codec #-}
instance HasCodec Int32 where
  codec = IntCodec
  {-# INLINE codec #-}
instance HasCodec Int64 where
  codec = LongCodec
  {-# INLINE codec #-}
instance HasCodec Float where
  codec = FloatCodec
  {-# INLINE codec #-}
instance HasCodec Double where
  codec = DoubleCodec
  {-# INLINE codec #-}
instance HasCodec NBTString where
  codec = StringCodec
  {-# INLINE codec #-}

instance HasCodec a => HasCodec (Vector a) where
  codec = ListCodec codec
  {-# INLINE codec #-}

requiredField :: HasCodec a => NBTString -> NBTCodec Object a a
requiredField key = RequiredKeyCodec key codec
{-# INLINE requiredField #-}

-- Encoding/Decoding

codecId :: NBTCodec Value inp out -> Int
codecId !c = (I# (dataToTag# c))
{-# INLINE codecId #-}

-- Generate: Parser Void a
parseNBTDirect :: NBTCodec Object inp out -> Code (FP.Parser Void out)
parseNBTDirect c1 =
  [||
    do
      tid <- Binary.get @Int8
      when (tid /= 10) FP.empty -- TODO Avoid hardcoding?
      FP.anyWord16_ -- empty string
      $$(parseCompound c1)
    ||]
  where
    parseNBTValue :: NBTCodec Value inp out -> Code (FP.Parser Void out)
    -- Primitives
    parseNBTValue ByteCodec = [|| Binary.get ||]
    parseNBTValue ShortCodec = [|| Binary.get ||]
    parseNBTValue IntCodec = [|| Binary.get ||]
    parseNBTValue LongCodec = [|| Binary.get ||]
    parseNBTValue FloatCodec = [|| Binary.get ||]
    parseNBTValue DoubleCodec = [|| Binary.get ||]
    parseNBTValue StringCodec = [|| Binary.get ||]

    parseNBTValue (ListCodec c) =
      [||
        do
          tid <- Binary.get @Int8
          when (tid /= expectedTId) FP.empty

          len <- Binary.get @Int16

          V.replicateM (fromIntegral len) $$(parseNBTValue c) 
        ||]
      where expectedTId = fromIntegral $ codecId c

    parseNBTValue (CompoundCodec inner) = parseCompound inner

    parseNBTValue (DiMapCodec _ g inner) = [|| fmap $$(g) $$(parseNBTValue inner) ||]

    parseCompound :: NBTCodec Object inp out -> Code (FP.Parser Void out)
    parseCompound cod' =
        [||
          do
            tup <- $$(unsafeCodeCoerce $ tupExp innerCodecs)
            $$(unsafeCodeCoerce $ parseComp cod') tup
          ||]
      where
        innerCodecs = keysToCodecs cod'

        keysToCodecs :: NBTCodec Object a b -> [(NBTString, Int, SomeValueCodec)]
        keysToCodecs (DiMapCodec _ _ inner) = keysToCodecs inner
        keysToCodecs (ApCodec l r) = keysToCodecs l <> keysToCodecs r
        keysToCodecs (RequiredKeyCodec k cod) = [(k, codecId cod, SomeValueCodec cod)]
        keysToCodecs (PureCodec _) = []

        tupExp [] = tupE $ fmap (Prelude.pure [| error "not parsed yet" |]) innerCodecs
        tupExp ((k, expTid, c):xs) =
          [| do
              tid <- fmap fromIntegral $ Binary.get @Int8
              when (testBit tid validTypes) FP.empty

              key <- Binary.get @NBTString

              undefined
            |]

        validTypes = foldl' (\acc (_, i, _) -> setBit i acc) 0 innerCodecs

        hashes =
          let hsList = fmap (\(bs, _, _) -> hash bs) innerCodecs
          in if length hsList /= IS.size (IS.fromList hsList)
            then error "Hash collision" -- For future me to worry about
            else U.fromList hsList

        parseComp :: NBTCodec Object a b -> Q Exp
        parseComp (PureCodec a) = [| \_ -> Prelude.pure $(unTypeCode a) |]
        parseComp (ApCodec ff fa) =
          [| \tup -> do
              f <- $(parseComp ff) tup
              a <- $(parseComp fa) tup
              Prelude.pure (f a)
            |]
        parseComp (RequiredKeyCodec key _) = [| \tup -> Prelude.pure $ $(indexByKey key) tup |]
        parseComp (DiMapCodec _ g inner) = [| fmap $(unTypeCode g) ($(parseComp inner) tup) |]

        indexByKey key =
          let i = List.elemIndex key (fmap (\(n, _, _) -> n) innerCodecs)
          in undefined


data SomeValueCodec = forall a b . SomeValueCodec (NBTCodec Value a b)

-- Generate: NBT -> Maybe a
