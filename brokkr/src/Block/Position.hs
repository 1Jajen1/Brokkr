{-# LANGUAGE PatternSynonyms #-}
module Block.Position (
  BlockPosition(..)
, pattern BlockPos
) where

import Util.Linear.V3
import Util.Binary
import Data.Word
import Data.Bits

newtype BlockPosition = BlockPosition (V3 Int)
  deriving stock Show
  deriving newtype Eq

pattern BlockPos :: Int -> Int -> Int -> BlockPosition
pattern BlockPos x y z = BlockPosition (V3_Int x y z)
{-# COMPLETE BlockPos #-}

instance FromBinary BlockPosition where
  get = do
    w <- get @Word64
    let x' = unsafeShiftR w 38
        y' = w .&. 0xFFF
        z' = (unsafeShiftR w 12) .&. 0x3FFFFFF
        x = if x' >= 2 ^ (25 :: Int)
          then x' - 2 ^ (26 :: Int)
          else x'
        z = if z' >= 2 ^ (25 :: Int)
          then z' - 2 ^ (26 :: Int)
          else z'
        y = if y' >= 2 ^ (12 :: Int)
          then y' - 2 ^ (13 :: Int)
          else y'
    return $ BlockPos (fromIntegral x) (fromIntegral y) (fromIntegral z)

instance ToBinary BlockPosition where
  put (BlockPos x y z) =
    let y' :: Word64 = fromIntegral y .&. 0xFFF
        x' :: Word64 = unsafeShiftL (fromIntegral x .&. 0x3FFFFFF) 38
        z' :: Word64 = unsafeShiftL (fromIntegral z .&. 0x3FFFFFF) 12
    in put $ x' .|. z' .|. y'
