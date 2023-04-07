module Util.Queue (
  Queue
, new
, push, pushN
, flush
, isEmpty
) where
import Control.Concurrent
import Data.Vector.Mutable (MVector)
import qualified Data.Vector.Mutable as MV
import Control.Monad.Primitive
import Data.IORef
import Util.PrimVar
import qualified Data.Vector as V

data Queue a = Queue !(MVar ()) !(PrimVar RealWorld Int) !(IORef (MVector RealWorld a))

instance Show (Queue a) where
  show _ = "Queue"

new :: Int -> IO (Queue a)
new i
  | i > 0 = Queue <$> newMVar () <*> newPrimVar 0 <*> (MV.new i >>= newIORef)
  | otherwise = error $ "Util.Queue:new invalid initial size: " <> show i
{-# INLINE new #-}

push :: Queue a -> a -> IO ()
push (Queue lock indexRef arrRef) el = withMVar lock $ \_ -> do
  i <- readPrimVar indexRef
  arr <- growIfNeeded i arrRef
  MV.unsafeWrite arr i el
  writePrimVar indexRef $ i + 1

pushN :: Queue a -> V.Vector a -> IO ()
pushN (Queue lock indexRef arrRef) els = withMVar lock $ \_ -> do
  mEls <- V.unsafeThaw els
  i <- readPrimVar indexRef
  arr <- growIfNeeded (i + V.length els) arrRef
  MV.unsafeCopy (MV.unsafeSlice i (V.length els) arr) mEls
  writePrimVar indexRef $ i + V.length els

flush :: Queue a -> IO (V.Vector a)
flush (Queue lock indexRef arrRef) = withMVar lock $ \_ -> do
  i <- readPrimVar indexRef
  arr <- readIORef arrRef
  let slice = MV.unsafeSlice 0 i arr
  ret <- V.freeze slice
  MV.clear arr
  writePrimVar indexRef 0
  pure ret

isEmpty :: Queue a -> IO Bool
isEmpty (Queue lock indexRef _) = withMVar lock $ \_ -> do
  i <- readPrimVar indexRef
  pure $ i == 0

growIfNeeded :: Int -> IORef (MVector RealWorld a) -> IO (MVector RealWorld a)
growIfNeeded i ref = do
  arr <- readIORef ref
  let sz = MV.length arr
  if i < sz
    then pure arr
    else do
      -- TODO Better growth strategy, also think about shrinking again?
      arr' <- MV.new $ max (i + 1) (sz * 2)
      MV.unsafeCopy (MV.unsafeSlice 0 sz arr') arr
      writeIORef ref arr'
      pure arr'
