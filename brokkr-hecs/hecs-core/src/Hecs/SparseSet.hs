{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE RecordWildCards #-}
module Hecs.SparseSet (
  SparseSet
, new
, insert
, lookup
, delete
) where

import Prelude hiding (lookup)

import GHC.Exts
import GHC.IO

data SparseSet value = SparseSet {
  sparseRef :: MutVar# RealWorld (MutableByteArray# RealWorld)
, denseRef  :: MutVar# RealWorld (MutableByteArray# RealWorld)
, denseVRef :: MutVar# RealWorld (MutableArray# RealWorld value)
, sizeRef   :: MutableByteArray# RealWorld
}

new :: Int -> IO (SparseSet value)
{-# NOINLINE new #-}
new (I# initSz) = IO $ \s0 ->
  case newByteArray# (4# *# initSz) s0 of
    (# s1, sparseArr #) -> case newMutVar# sparseArr s1 of
      (# s2, sparseRef #) -> case newByteArray# (4# *# initSz) s2 of
        (# s3, denseArr #) -> case newMutVar# denseArr s3 of
          (# s4, denseRef #) -> case newArray# initSz (error "SparseSet empty") s4 of
            (# s5, denseVArr #) -> case newMutVar# denseVArr s5 of
              (# s6, denseVRef #) -> case newByteArray# 8# s6 of
                (# s7, sizeRef #) -> (# writeIntArray# sizeRef 0# 0# s7, SparseSet{..} #)

insert :: SparseSet value -> Int -> value -> IO ()
{-# NOINLINE insert #-}
insert SparseSet{..} (I# ind) v = do
  IO $ \s0 ->
    case readMutVar# denseVRef s0 of
      (# s1, denseVArr0 #) -> 
        let cap = sizeofMutableArray# denseVArr0
        in if isTrue# (ind >=# cap)
          then case growByteArray# sparseRef (cap *# 4#) s1 of
            s2 -> case growByteArray# denseRef (cap *# 4#) s2 of
              s3 -> case newArray# (cap *# 2#) (error "SparseSet grow") s3 of
                (# s4, denseVArr #) -> case writeMutVar# denseVRef denseVArr s4 of
                  s5 -> (# copyMutableArray# denseVArr0 0# denseVArr 0# cap s5, () #)
          else (# s1, () #)
  IO $ \s0 ->
    case readMutVar# denseVRef s0 of
      (# s1, denseVArr #) -> case readMutVar# denseRef s1 of
        (# s2, denseArr #) -> case readMutVar# sparseRef s2 of
          (# s3, sparseArr #) -> case readWord32Array# sparseArr ind s3 of
            (# s4, denseInd #) -> case readIntArray# sizeRef 0# s4 of
              (# s5, sz #) -> if isTrue# (denseInd `ltWord32#` wordToWord32# (int2Word# sz))
                then case readInt32Array# denseArr (word2Int# (word32ToWord# denseInd)) s5 of
                  (# s6, sparseInd #) | isTrue# (int32ToInt# sparseInd ==# ind) -> (# writeArray# denseVArr (word2Int# (word32ToWord# denseInd)) v s6, () #)
                  (# s7, _ #) -> (# doInsert sparseArr denseArr denseVArr sz s7, () #)
                else (# doInsert sparseArr denseArr denseVArr sz s5, () #)
  where
    doInsert sparseArr denseArr denseVArr sz s0 =
      case writeInt32Array# denseArr sz (intToInt32# ind) s0 of
        s1 -> case writeInt32Array# sparseArr ind (intToInt32# sz) s1 of
          s2 -> case writeArray# denseVArr sz v s2 of
            s3 -> writeIntArray# sizeRef 0# (sz +# 1#) s3

growByteArray# baRef cap s0 =
  case readMutVar# baRef s0 of
    (# s1, ba #) -> case newByteArray# (cap *# 2#) s1 of
      (# s2, arr #) -> case copyMutableByteArray# ba 0# arr 0# cap s2 of
        s3 -> writeMutVar# baRef arr s3

lookup :: SparseSet value -> Int -> (value -> IO r) -> IO r -> IO r
{-# INLINE lookup #-}
lookup ss (I# i) success failure = IO $ \s -> case lookupI ss i s of
  (# s', (# v | #) #) -> case success v of IO g -> g s'
  (# s', (# | (# #) #) #) -> case failure of IO g -> g s'

lookupI :: SparseSet value -> Int# -> State# RealWorld -> (# State# RealWorld, (# value | (# #) #) #)
{-# NOINLINE lookupI #-}
lookupI SparseSet{..} ind = \s0 ->
  case readMutVar# denseVRef s0 of
    (# s1, denseVArr #) -> case readMutVar# denseRef s1 of
      (# s2, denseArr #) -> case readMutVar# sparseRef s2 of
        (# s3, sparseArr #) -> case readWord32Array# sparseArr ind s3 of
          (# s4, denseInd #) -> case readIntArray# sizeRef 0# s4 of
            (# s5, sz #) -> if isTrue# (denseInd `ltWord32#` wordToWord32# (int2Word# sz))
              then case readInt32Array# denseArr (word2Int# (word32ToWord# denseInd)) s5 of
                (# s6, sparseInd #) | isTrue# (int32ToInt# sparseInd ==# ind) ->
                  case readArray# denseVArr (word2Int# (word32ToWord# denseInd)) s6 of
                    (# s7, v #) -> (# s7, (# v | #) #)
                (# s7, _ #) -> (# s7, (# | (# #) #) #)
              else (# s5, (# | (# #) #) #)

delete :: SparseSet value -> Int -> IO ()
{-# NOINLINE delete #-} -- TODO Manually w/w to ensure this never regresses? Although ghc still applies w/w to noinline anyway
delete SparseSet{..} (I# ind) = IO $ \s0 ->
  case readMutVar# denseVRef s0 of
    (# s1, denseVArr #) -> case readMutVar# denseRef s1 of
      (# s2, denseArr #) -> case readMutVar# sparseRef s2 of
        (# s3, sparseArr #) -> case readWord32Array# sparseArr ind s3 of
          (# s4, denseInd #) -> case readIntArray# sizeRef 0# s4 of
            (# s5, sz #) -> if isTrue# (denseInd `ltWord32#` wordToWord32# (int2Word# sz))
              then case readInt32Array# denseArr (word2Int# (word32ToWord# denseInd)) s5 of
                (# s6, sparseInd #) | isTrue# (int32ToInt# sparseInd ==# ind) ->
                  case writeIntArray# sizeRef 0# (sz -# 1#) s6 of
                    s7 -> case readInt32Array# denseArr (sz -# 1#) s7 of
                      (# s8, temp #) -> case writeInt32Array# denseArr (word2Int# (word32ToWord# denseInd)) temp s8 of
                        s9 -> case writeWord32Array# sparseArr (int32ToInt# temp) denseInd s9 of
                          s10 -> case readArray# denseVArr (sz -# 1#) s10 of
                            (# s11, vTemp #) -> case writeArray# denseVArr sz (error "SparseSet delete") s11 of
                              s12 -> (# writeArray# denseVArr (word2Int# (word32ToWord# denseInd)) vTemp s12, () #)
                (# s7, _ #) -> (# s7, () #)
              else (# s5, () #)
