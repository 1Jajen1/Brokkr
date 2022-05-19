{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module Network.Effect.Packet (
  readPacket
, sendPacket
, sendPackets
-- internal
, toStrictSizePrefixedByteString -- TODO Move to internal module
) where
  
import qualified Effectful.State.Static.Local as S
import Data.ByteString
import qualified Data.ByteString.Internal as BS
import Network.Effect.Network
import Effectful
import Data.Void
import FlatParse.Basic
import qualified Util.Binary as Binary
import Network.Util.VarNum
import Util.Flatparse
import Util.Binary
import qualified Mason.Builder.Internal as B
import GHC.IO.Unsafe
import Foreign.ForeignPtr
import Data.IORef
import Foreign.ForeignPtr.Unsafe
import Foreign.Ptr
import Data.Bits
import Foreign.Marshal
import GHC.Exts (IsList(fromList))

readPacket ::
  ( S.State ByteString :> es
  , Network :> es)
  => Parser Void a
  -> Eff es a
readPacket parser = do
  leftover <- S.get
  (packetBs, leftoverBs) <- go leftover
  S.put leftoverBs
  case runParser parser packetBs of
    OK p _ -> pure p -- TODO Decide wether or not the remaining bytes should be empty...
    _ -> error "Failed to parse packet" -- TODO
  where
    lenParser = do
      VarInt len <- Binary.get @VarInt
      takeN $ fromIntegral len
    go bs = do
      case runParser lenParser bs of
        OK resBs remBs -> pure (resBs, remBs)
        Fail -> do
          moreBs <- receiveBytes
          go (bs <> moreBs)

{- Future work with compression: Can compression and later encryption be added without too much of a problem?

Adding a parameter for compression and encryption settings seems trivial. Callers would then need to somehow store
or access such parameters, but that does not seem problematic at all.

However toStrictSizePrefixedByteString might need to be reworked a little...

Haskell zlib's api works over lazy bytestrings and thus needs the finished product from toStrictSizePrefixedByteString, I kind of wish I could
compress in place with just passing a ptr and a size and than prefix the compressed size in place, that'd avoid another copy
Or at least compress into a pre-allocated buffer with some offset from the start so I can fill in the size...

But that is independent of public api so I should be fine just keeping the current api and reworking the internals as necessary.

Also look into using zlib-ng or other libs.

-}

{- Future work with custom allocators and io_uring:

Since I want to experience with custom allocators and io_uring I may need to rewrite how bytes are allocated and what exactly is passed to sendBytes.

For custom allocators I can write a custom backend for the Builder interface from mason since that is very flexible.
For io_uring I probably won't need to change too many things, just pass the buffers to a writev sqe.
At some point I'd like to investigate how I can avoid needless copies etc since the buffers will only ever be used for writing data out I may as well
pass them to the kernel with zerocopy and with a custom allocator I also control when I need to free it

Again this is internals only if I only ever use sendPacket, so should be fine.

-}

sendPacket :: (ToBinary a, Network :> es) => Int -> a -> Eff es ()
sendPacket szEstimate a = sendBytes [toStrictSizePrefixedByteString szEstimate (put a)]
{-# INLINE sendPacket #-}

sendPackets :: (ToBinary a, Network :> es) => Int -> [a] -> Eff es ()
sendPackets szEstimate as = sendBytes . fromList $ fmap (\a -> toStrictSizePrefixedByteString szEstimate $ put a) as
{-# INLINE sendPackets #-}

{- Future work for writing out large packets

For very large large Packets that repeatedly allocate/insert large chunks such as chunk data or chunk light data which has large
 arrays we need a different approach since a single growing buffer would incur quite a few large copies, collecting a set of buffers seems like a better strategy.
 A hybrid approach similar to toLazyBytestring seems good, BUT the packets only become large because of single large bytearray inserts, nothing else is
 that large, so maybe just have a normal growing buffer and when we encounter a large bytestring insert we write out both the current buffer and the large
 bytestring at once and continue with a fresh buffer. This is different from toLazyBytestring and is only better because the only reason we cross
 the size threshold is single large inserts.
-}

{- | Write a builder into a strict bytestring

Can be passed a size estimate which adjusts the initial buffer size to avoid needlessly having to grow and reallocate memory.
-}
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

  pPtr <- writeVarNumInternal (fromIntegral sz) ptr
  let pSz = minusPtr pPtr ptr
      off = prefixSz - pSz
      finalLen = sz + pSz

  case off of
    0 -> pure $ BS.BS fptr finalLen
    _ -> do
      copyBytes (plusPtr ptr off) ptr pSz
      pure $ BS.BS (plusForeignPtr fptr off) finalLen
{-# INLINE toStrictSizePrefixedByteString #-}
