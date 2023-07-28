{-# LANGUAGE DataKinds, TemplateHaskellQuotes, DerivingStrategies, DeriveAnyClass, OverloadedStrings #-}
module BigTest where

import Brokkr.NBT.Codec
import Brokkr.NBT.NBTString.Internal

import Control.DeepSeq

import Data.Int

import Data.Primitive

import Data.Vector.Storable qualified as S

import GHC.Generics (Generic)

data BigTest = BigTest {
  nested     :: {-# UNPACK #-} !Nested1
, byteTest   :: {-# UNPACK #-} !Int8
, shortTest  :: {-# UNPACK #-} !Int16
, intTest    :: {-# UNPACK #-} !Int32
, longTest   :: {-# UNPACK #-} !Int64
, floatTest  :: {-# UNPACK #-} !Float
, doubleTest :: {-# UNPACK #-} !Double
, byteArrayTest :: {-# UNPACK #-} !(S.Vector Int8)
, listTestLong     :: {-# UNPACK #-} !(SmallArray Int64)
, listTestCompound :: {-# UNPACK #-} !(SmallArray ListComp)
, stringTest :: {-# UNPACK #-} !NBTString
}
  deriving stock (Show, Generic)
  deriving anyclass NFData

data Nested1 = Nested1 {
  egg :: {-# UNPACK #-} !Nested2
, ham :: {-# UNPACK #-} !Nested2
}
  deriving stock (Show, Generic)
  deriving anyclass NFData

instance HasCodec Nested1 where
  codec = compound "nested compound" $ [|| Nested1 ||]
    <$#> requiredField "egg" .= [|| egg ||]
    <*#> requiredField "ham" .= [|| ham ||]

data Nested2 = Nested2 {
  name  :: {-# UNPACK #-} !NBTString
, value :: {-# UNPACK #-} !Float
}
  deriving stock (Show, Generic)
  deriving anyclass NFData

instance HasCodec Nested2 where
  codec = compound "nested 2" $ [|| Nested2 ||]
    <$#> requiredField "name"  .= [|| name  ||]
    <*#> requiredField "value" .= [|| value ||]

data ListComp = ListComp {
  createdOn :: {-# UNPACK #-} !Int64
, nameL     :: {-# UNPACK #-} !NBTString
}
  deriving stock (Show, Generic)
  deriving anyclass NFData

instance HasCodec ListComp where
  codec = compound "created-on" $ [|| ListComp ||]
    <$#> requiredField "created-on" .= [|| createdOn ||]
    <*#> requiredField "name" .= [|| nameL ||]

instance NFData NBTString where
  rnf (NBTString bs) = rnf bs

bigTestCodec :: NBTCodec Value BigTest BigTest
bigTestCodec = compound "level" $ [|| BigTest ||]
  <$#> requiredField "nested compound test" .= [|| nested ||]
  <*#> requiredField "byteTest"   .= [|| byteTest ||]
  <*#> requiredField "shortTest"  .= [|| shortTest ||]
  <*#> requiredField "intTest"    .= [|| intTest ||]
  <*#> requiredField "longTest"   .= [|| longTest ||]
  <*#> requiredField "floatTest"  .= [|| floatTest ||]
  <*#> requiredField "doubleTest" .= [|| doubleTest ||]
  <*#> requiredField "byteArrayTest (the first 1000 values of (n*n*255+n*7)%100, starting with n=0 (0, 62, 34, 16, 8, ...))" .= [|| byteArrayTest ||]
  <*#> requiredField "listTest (long)" .= [|| listTestLong ||]
  <*#> requiredField "listTest (compound)" .= [|| listTestCompound ||]
  <*#> requiredField "stringTest" .= [|| stringTest ||]
