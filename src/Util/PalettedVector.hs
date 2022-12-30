module Util.PalettedVector (
  PalettedVector(..)
) where

import Control.DeepSeq
import Control.Monad.ST.Strict

import Data.Int

import Data.Proxy

import qualified Data.Vector.Unboxed as U

import GHC.TypeLits

import Network.Util.VarNum

import Util.Binary

import Util.Vector.Packed (PackedVector, DynamicNat(..))
import qualified Util.Vector.Packed as P

data PalettedVector (sz :: Nat) (globalBitSz :: Nat) =
    Global      {-# UNPACK #-} !(PackedVector ('Static sz) ('Static globalBitSz))
  | SingleValue {-# UNPACK #-} !Int
  -- TODO Use Unboxed vectors instead. They should be better for such small vectors
  | Indirect    {-# UNPACK #-} !(U.Vector Int) {-# UNPACK #-} !(PackedVector ('Static sz) 'Dynamic)

instance (KnownNat sz, KnownNat mBSz) => Show (PalettedVector sz mBSz) where
  show (Global v)      = "Global " <> show v
  show (SingleValue v) = "Single " <> show v
  show (Indirect p v)  = "Indirect " <> show p <> show v 

instance NFData (PalettedVector sz maxBitSz) where
  rnf (Global vec) = rnf vec
  rnf (SingleValue w) = rnf w
  rnf (Indirect p vec) = rnf p `seq` rnf vec

-- Network serialization
instance (KnownNat sz, KnownNat mBSz) => ToBinary (PalettedVector sz mBSz) where
  put (Global v) =
       put (fromIntegral @_ @Int8 bitSz)
    <> put (VarInt $ fromIntegral numInt64)
    <> put v
    where
      bitSz = fromIntegral $ natVal (Proxy @mBSz)
      vLen = runST $ P.length <$> P.unsafeThaw v
      numInt64 = P.nrWords bitSz vLen
  put (SingleValue v) = put (0 :: Int8) <> put (VarInt $ fromIntegral v) <> put (VarInt 0) -- No data
  put (Indirect p v) =
       put (fromIntegral @_ @Int8 bitSz)
    <> put (VarInt . fromIntegral $ U.length p)
    <> U.foldMap (\x -> put $ VarInt $ fromIntegral x) p
    <> put (VarInt $ fromIntegral numInt64)
    <> put v
    where
      bitSz = runST $ P.bitSz <$> P.unsafeThaw v
      vLen = runST $ P.length <$> P.unsafeThaw v
      numInt64 = P.nrWords bitSz vLen
  {-# INLINE put #-}
