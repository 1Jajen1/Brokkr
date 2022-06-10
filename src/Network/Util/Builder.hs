module Network.Util.Builder (
  toStrictSizePrefixedByteString
) where

import Network.Util.VarNum

import Data.IORef
import Data.ByteString
import qualified Data.ByteString.Internal as BS
import Foreign
import Foreign.ForeignPtr.Unsafe
import GHC.IO (unsafePerformIO)
import qualified Mason.Builder.Internal as B

toStrictSizePrefixedByteString :: Int -> B.Builder -> ByteString
toStrictSizePrefixedByteString szEstimate bb = unsafePerformIO $ do
  -- This is almost an exact copy of toStrictByteString from mason, but handles prefixing the length, later on will deal with compression as well
  let prefixSz = 5 -- TODO Adjust when I implement compression
      finalSzEst = szEstimate + prefixSz
      initSz = (finalSzEst + 64 - 1) .&. (complement $ 64 - 1) -- get some multiple of 64 larger than finalSzEst
  fptr0 <- mallocForeignPtrBytes szEstimate
  bufRef <- newIORef fptr0
  let ptr0 = unsafeForeignPtrToPtr fptr0
  B.Buffer _ pos <- B.unBuilder bb (B.GrowingBuffer bufRef) (B.Buffer (plusPtr ptr0 initSz) (plusPtr ptr0 prefixSz))

  fptr <- readIORef bufRef
  let ptr = unsafeForeignPtrToPtr fptr
  let sz = (minusPtr pos ptr) - prefixSz

  let varSz = varIntSize sz 
      finalLen = sz + varSz
      pSize = prefixSz - varSz
  _ <- writeVarNumInternal (fromIntegral sz) (plusPtr ptr pSize)

  pure $ case pSize of
    0 -> BS.BS fptr finalLen
    _ -> BS.BS (plusForeignPtr fptr pSize) finalLen
{-# INLINE toStrictSizePrefixedByteString #-}
