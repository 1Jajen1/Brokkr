{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Brokkr.STM.RWLock (
  TRWLock
, RwSTM
, newTRWLockIO
, newTRWLock
, withRead
, withWrite
, atomicallyRwSTM
) where

import Brokkr.STM.Class

import Control.Concurrent.STM

import GHC.Conc
import GHC.Exts
import GHC.IO

type Version = Word

data LockState = Writing | Reading !Version

-- Read/Write lock. Allows concurrent reads and sequential writes.
-- Built on top of STM it provides no fairness guarantee's at all
-- Writes will not be starved because they don't need to wait for
--  all readers to finish
-- Instead a write simply marks the lock as taken and the actual update
--  is deferred until after the transaction completes and the lock is
--  reset. Concurrent reads would then be retried.
-- Requires writes to be performed in 'RwSTM', which is effectively
-- WriterT [(u, TRWLock)] STM a but more efficient
-- Has more overhead to allocate the queue of update ops
newtype TRWLock = TRWLock (TVar LockState)

newTRWLockIO :: IO TRWLock
newTRWLockIO = TRWLock <$> newTVarIO (Reading 0)

newTRWLock :: MonadSTM m => m TRWLock
newTRWLock = liftSTM $ TRWLock <$> newTVar (Reading 0)

-- Defined over MonadSTM to allow the read side to be used in normal transactions too
withRead :: MonadSTM m => TRWLock -> m a -> m a
withRead (TRWLock tv) act = liftSTM (readTVar tv) >>= \case
  Reading _ -> act
  Writing -> liftSTM retry

withWrite :: TRWLock -> RwSTM u u -> RwSTM u ()
withWrite (TRWLock tv) act = liftSTM (readTVar tv) >>= \case
  Writing -> liftSTM retry
  Reading (W# w) -> do
    liftSTM $ writeTVar tv Writing
    u <- act
    mkRwSTM $ \uArr0 tvArr0 vArr0 len s0 ->
      case grow uArr0 tvArr0 vArr0 len s0 of
        (# uArr, tvArr, vArr, s #) -> case writeSmallArray# uArr len u s of
          s1 -> case writeSmallArray# tvArr len (coerce tv) s1 of
            s2 -> case writeWordArray# vArr len w s2 of
              s3 -> (# s3, len +# 1#, uArr, tvArr, vArr, () #)
  where
    grow uArr tvArr vArr len s
      | isTrue# (len <# cap) = (# uArr, tvArr, vArr, s #)
      | otherwise =
        case newSmallArray# (cap *# 2#) (error "RwSTM:grow:u:empty") s of
          (# s1, uArr' #) -> case copySmallMutableArray# uArr 0# uArr' 0# len s1 of
            s2 -> case newSmallArray# (cap *# 2#) (error "RwSTM:grow:tv:empty") s2 of
              (# s3, tvArr' #) -> case copySmallMutableArray# tvArr 0# tvArr' 0# len s3 of
                s4 -> case newByteArray# (cap *# 16#) s4 of
                  (# s5, vArr' #) -> case copyMutableByteArray# vArr 0# vArr' 0# (len *# 8#) s5 of
                    s6 -> (# uArr', tvArr', vArr', s6 #)
      where cap = sizeofSmallMutableArray# uArr

newtype RwSTM u a = RwSTMI (
     SmallMutableArray# RealWorld u       -- Update types
  -> SmallMutableArray# RealWorld TRWLock -- tvars to update
  -> MutableByteArray# RealWorld          -- version numbers
  -> Int#                                 -- nr of elements in the previous 3 arrays
  -> State# RealWorld
  -> (# State# RealWorld, Int#, SmallMutableArray# RealWorld u, SmallMutableArray# RealWorld TRWLock, MutableByteArray# RealWorld, a #)
  )

mkRwSTM ::
  (  SmallMutableArray# RealWorld u
  -> SmallMutableArray# RealWorld TRWLock
  -> MutableByteArray# RealWorld
  -> Int#
  -> State# RealWorld
  -> (# State# RealWorld, Int#, SmallMutableArray# RealWorld u, SmallMutableArray# RealWorld TRWLock, MutableByteArray# RealWorld, a #)
  ) -> RwSTM u a
mkRwSTM f = RwSTMI $ oneShot $ \uArr -> oneShot $ \tvArr -> oneShot $ \vArr -> oneShot $ \len -> oneShot $ \s -> f uArr tvArr vArr len s

instance Functor (RwSTM u) where
  fmap f (RwSTMI g) = mkRwSTM $ \uArr tvArr vArr len s -> case g uArr tvArr vArr len s of
    (# s', len', uArr', tvArr', vArr', res #) -> (# s', len', uArr', tvArr', vArr', f res #)

instance Applicative (RwSTM u) where
  pure a = mkRwSTM $ \uArr tvArr vArr len s -> (# s, len, uArr, tvArr, vArr, a #)
  ff <*> fa = do f <- ff; fmap f fa

instance Monad (RwSTM u) where
  (RwSTMI g) >>= f = mkRwSTM $ \uArr tvArr vArr len s -> case g uArr tvArr vArr len s of
    (# s', len', uArr', tvArr', vArr', res #) -> case f res of
      RwSTMI h -> h uArr' tvArr' vArr' len' s'

instance MonadSTM (RwSTM u) where
  liftSTM (STM f) = mkRwSTM $ \uArr tvArr vArr len s -> case f s of
    (# s', res #) -> (# s', len, uArr, tvArr, vArr, res #)

-- Run a transaction that may contain TRWLock's. Performs the writes using the callback and resets the written variables
atomicallyRwSTM :: (u -> IO ()) -> RwSTM u a -> IO a
atomicallyRwSTM hdl (RwSTMI f) = IO $ \s0 ->
  case newSmallArray# initSz (error "RwSTM:u:empty") s0 of
    (# s1, uArr #) -> case newSmallArray# initSz (error "RwSTM:tv:empty") s1 of
      (# s2, tvArr #) -> case newByteArray# (initSz *# 8#) s2 of
        (# s3, vArr #) -> case f uArr tvArr vArr 0# s3 of
          (# s4, finSz, finUArr, finTvArr, finVArr, res #) -> case go finSz finUArr finTvArr finVArr 0# s4 of
            s5 -> (# s5, res #)
  where
    initSz = 2#
    go len uarr tvarr varr n s
      | isTrue# (n >=# len) = s
      | otherwise =
        case readSmallArray# uarr n s of
          (# s1, u #) -> case readSmallArray# tvarr n s1 of
            (# s2, tv #) -> case readWordArray# varr n s2 of
              (# s3, v #) -> case hdl u >> atomically (writeTVar (coerce tv) (Reading (W# (v `plusWord#` 1##)))) of
                IO io -> case io s3 of
                  (# s4, () #) -> go len uarr tvarr varr (n +# 1#) s4
