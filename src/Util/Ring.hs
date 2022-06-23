module Util.Ring (
  Ring
, newRingBuffer
, push
, pushN
, take
, peek
, peekN
, advanceN
) where
import Data.Primitive
import Control.Monad.Primitive
import qualified Data.Vector as MV
import qualified Data.Vector.Generic.Mutable as VG
import Data.Bits
import Control.Concurrent.MVar
import Prelude hiding (take)
import qualified Data.Vector as V
import Control.Monad (void)

newtype Mut a = Mut (MutableByteArray RealWorld)

newMut :: forall a . Prim a => a -> IO (Mut a)
newMut el = Mut <$> do
  mar <- newByteArray (sizeOf el)
  writeByteArray mar 0 el
  pure mar

readMut :: Prim a => Mut a -> IO a
readMut (Mut arr) = readByteArray arr 0

writeMut :: Prim a => Mut a -> a -> IO ()
writeMut (Mut arr) = writeByteArray arr 0 

-- TODO Make a unlifted, stuff in these rings should usually not be lazy since we never drop any
-- TODO MVar vs TVar MVar is great if only a single thread is blocked or writes/reads always write/read one element at a time
--  TVar is much better if when n threads are blocked >= n writes/reads come in
data Ring a = Ring !(Mut Word) !(Mut Word) !(MVar ()) !(MV.MVector RealWorld a)

newRingBuffer :: Int -> IO (Ring a)
newRingBuffer sz
  | sz < 1 || popCount sz /= 1 = error "Ring.newRingBuffer invalid sz"
  | otherwise = do
    (r,w) <- (,) <$> newMut 0 <*> newMut 0
    l <- newMVar ()
    arr <- VG.new sz
    pure $ Ring r w l arr

mask :: Ring a -> Word -> Word
mask (Ring _ _ _ arr) i = i .&. (fromIntegral (VG.length arr) - 1)

-- Blocks if the ring is full
push :: Ring a -> a -> IO ()
push ring@(Ring _ w l arr) el = full ring >>= \case
  False -> do
    w' <- readMut w
    VG.unsafeWrite arr (fromIntegral $ mask ring w') el
    writeMut w $ w' + 1
    _ <- tryPutMVar l ()
    pure ()
  True -> takeMVar l >> push ring el

pushN :: Ring a -> V.Vector a -> IO ()
pushN ring@(Ring r w l arr) els = do
  r' <- readMut r
  w' <- readMut w
  let n = V.length els
      len = VG.length arr
  if n >= len -  (fromIntegral $ w' - r')
    then takeMVar l >> pushN ring els
    else do
      let writeI = fromIntegral $ mask ring w'
      -- check if the write fits without wrapping around
      if writeI + n >= len
        then do
          -- we need two copies since our write wraps around
          let firstPart = len - writeI
              remPart = n - firstPart
              mSlice1 = VG.unsafeSlice writeI firstPart arr
              mSlice2 = VG.unsafeSlice 0 remPart arr
              eSlice1 = V.unsafeSlice 0 firstPart els
              eSlice2 = V.unsafeSlice firstPart remPart els
          V.unsafeCopy mSlice1 eSlice1
          V.unsafeCopy mSlice2 eSlice2
        -- Just one copy needed as elements fit into one slice
        else do
          V.unsafeCopy (VG.unsafeSlice writeI n arr) els
      writeMut w $ w' + fromIntegral n
      tryPutMVar l () >> pure ()

-- Blocks if the ring is empty
peek :: Ring a -> IO a
peek ring@(Ring r _ l arr) = empty ring >>= \case
  False -> do
    r' <- readMut r
    VG.unsafeRead arr . fromIntegral $ mask ring r'
  True -> takeMVar l >> peek ring

take :: Ring a -> IO a
take ring = do
  a <- peek ring
  advanceN ring 1
  pure a

-- Blocks if the ring is empty
-- TODO Can we avoid freeze or use unsafeFreeze? It segfaults, but maybe we can prevent that in a cheap way
peekN :: Ring a -> IO (V.Vector a)
peekN ring@(Ring r w lock arr) = empty ring >>= \case
  True -> takeMVar lock >> peekN ring
  False -> do
    !r' <- readMut r
    !w' <- readMut w
    let !readI = fromIntegral $ mask ring r'
        !sz = min (VG.length arr - readI) . fromIntegral $ w' - r'
        !slice = VG.unsafeSlice readI sz arr
    !res <- V.freeze slice
    pure res

advanceN :: Ring a -> Word -> IO ()
advanceN (Ring r _ l arr) n = do
  r' <- readMut r
  -- TODO Check for overlap, the way I use it right now this can't happen but it might in the future
  -- Weak references have a perf impact right?
  VG.clear (VG.unsafeSlice (fromIntegral r') (fromIntegral n) arr)
  writeMut r (r' + n)
  void $ tryPutMVar l ()

empty :: Ring a -> IO Bool
empty (Ring r w _ _) = do
  r' <- readMut r
  w' <- readMut w
  pure $ r' == w'

full :: Ring a -> IO Bool
full ring@(Ring _ _ _ arr) = do
  sz <- size ring
  pure $ sz == (fromIntegral $ VG.length arr)

size :: Ring a -> IO Word
size (Ring r w _ _) = do
  r' <- readMut r
  w' <- readMut w
  pure $ w' - r'
