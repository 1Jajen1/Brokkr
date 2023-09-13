{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Hecs.Archetype.Storage (
  Storage(..)
, getColumn, getBitSet
, readStorage, writeStorage
, isEnabled, enableComponent, disableComponent
, growStorage
, moveTo
, SizesAndAlignment(..)
, newBoxed, newFlat, newTag
) where

import Control.Monad
import Control.Monad.Primitive

import Data.Bits
import Data.Coerce
import Data.Int
import Data.Primitive
import Data.Proxy
import Data.Word

import Foreign.Storable

import GHC.Exts

import Hecs.Component.Column
import Hecs.Component.Internal

import Unsafe.Coerce (unsafeCoerce)

data family Storage (kind :: ComponentKind) s

data instance Storage Boxed s =
  BoxedStorage
    (SmallMutableArray# s (MutableArray# s Any)) -- column data
    (SmallMutableArray# s (MutableByteArray# s)) -- bitsets for each column

data instance Storage Flat s =
  FlatStorage
    {-# UNPACK #-} !SizesAndAlignment            -- sizes and alignment of each column
    (SmallMutableArray# s (MutableByteArray# s)) -- column data
    (SmallMutableArray# s (MutableByteArray# s)) -- bitsets for each column

data instance Storage Tag s =
  TagStorage
    (SmallMutableArray# s (MutableByteArray# s)) -- bitsets for each tag

newtype SizesAndAlignment = SizesAndAlignment ByteArray

newBoxed :: PrimMonad m => Int -> Int -> m (Storage Boxed (PrimState m))
{-# INLINE newBoxed #-}
newBoxed nrCols@(I# nrCols#) (I# initCap) = do
  MutableArray e <- newArray 0 (error "")
  MutableByteArray eBS <- newByteArray 0
  primitive $ \s ->
    case newSmallArray# nrCols# e s of
      (# s1, cols #) -> case newSmallArray# nrCols# eBS s1 of
        (# s2, colBS #) -> (# goNewBoxed cols 0 s2, BoxedStorage cols colBS #)
  where
    goNewBoxed cols n@(I# n#) s
      | n >= nrCols = s
      | otherwise = case newArray# initCap (error "Boxed storage:empty") s of
        (# s1, arr #) -> goNewBoxed cols (n + 1) (writeSmallArray# cols n# arr s1)

newFlat :: PrimMonad m => Int -> SizesAndAlignment -> Int -> m (Storage Flat (PrimState m))
{-# INLINE newFlat #-}
newFlat nrCols@(I# nrCols#) sz@(SizesAndAlignment sizes) (I# initCap) = do
  MutableByteArray eBS <- newByteArray 0
  primitive $ \s ->
    case newSmallArray# nrCols# eBS s of
      (# s1, cols #) -> case newSmallArray# nrCols# eBS s1 of
        (# s2, colBS #) -> (# goNewFlat cols 0 s2, FlatStorage sz cols colBS #)
  where
    goNewFlat cols n@(I# n#) s
      | n >= nrCols = s
      | otherwise =
        let !(I# sizeN ) = fromIntegral $ indexByteArray @Int8 sizes (n * 2)
            !(I# alignN) = fromIntegral $ indexByteArray @Int8 sizes (n * 2 + 1)
        in case newAlignedPinnedByteArray# (initCap *# sizeN) alignN s of
        (# s1, arr #) -> goNewFlat cols (n + 1) (writeSmallArray# cols n# arr s1)

newTag :: PrimMonad m => Int -> Int -> m (Storage Tag (PrimState m))
{-# INLINE newTag #-}
newTag nrCols@(I# nrCols#) (I# initCap) = do
  MutableByteArray eBS <- newByteArray 0
  primitive $ \s ->
    case newSmallArray# nrCols# eBS s of
      (# s1, colBS #) -> (# s1, TagStorage colBS #)

getColumn :: forall c m r
  . (Component c, PrimMonad m)
  => Storage (ComponentKindFor c) (PrimState m)
  -> Int
  -> (Column (ComponentKindFor c) (PrimState m) c -> m r)
  -> m r
{-# INLINE getColumn #-}
getColumn !store (I# column) cont = backing @_ @c
  (\(BoxedStorage cols _) -> do
    col <- primitive $ \s -> case readSmallArray# cols column s of (# s', mba #) -> (# s', BoxedColumn (unsafeCoerce $ MutableArray mba) #)
    cont col
    )
  (\(FlatStorage _ cols _) -> do
    col <- primitive $ \s -> case readSmallArray# cols column s of (# s', mba #) -> (# s', FlatColumn (MutableByteArray mba) #)
    cont col
    )
  (\(TagStorage _) -> cont $ error "getColumn:TagColumn")
  store

getBitSet :: forall c m r
  . (KnownComponentKind c, PrimMonad m)
  => Storage c (PrimState m)
  -> Int
  -> (MutablePrimArray (PrimState m) Word64 -> m r)
  -> m r
  -> m r
{-# INLINE getBitSet #-}
getBitSet !store (I# column) onSucc onFail = branchKind @c
  (\(BoxedStorage _ sbs) -> do
    bs <- primitive $ \s -> case readSmallArray# sbs column s of (# s', mba #) -> (# s', MutablePrimArray mba #)
    sz <- getSizeofMutablePrimArray bs
    if sz == 0
      then onFail
      else onSucc bs
    )
  (\(FlatStorage _ _ sbs) -> do
    bs <- primitive $ \s -> case readSmallArray# sbs column s of (# s', mba #) -> (# s', MutablePrimArray mba #)
    sz <- getSizeofMutablePrimArray bs
    if sz == 0
      then onFail
      else onSucc bs
    )
  (\(TagStorage sbs) -> do
    bs <- primitive $ \s -> case readSmallArray# sbs column s of (# s', mba #) -> (# s', MutablePrimArray mba #)
    sz <- getSizeofMutablePrimArray bs
    if sz == 0
      then onFail
      else onSucc bs
    )
  store

-- The caller needs to ensure the row and column exist and the component is enabled
readStorage :: forall c m r . (Component c, PrimMonad m, Coercible (ComponentValueFor c) c) => Storage (ComponentKindFor c) (PrimState m) -> Int -> Int -> (c -> m r) -> m r
{-# INLINE readStorage #-}
readStorage !store !row (I# column) cont = backing @_ @c
  (\(BoxedStorage cols _) -> do
    boxedCol <- primitive $ \s -> case readSmallArray# cols column s of (# s', mba #) -> (# s', BoxedColumn (unsafeCoerce $ MutableArray mba) #)
    readColumn boxedCol row cont)
  (\(FlatStorage _ cols _) -> do
    flatCol <- primitive $ \s -> case readSmallArray# cols column s of (# s', mba #) -> (# s', FlatColumn (MutableByteArray mba) #)
    readColumn flatCol row cont
    )
  (\_ -> pure (error "readStorage:Tag"))
  store

-- The caller needs to ensure the row and column exist and the component is enabled
writeStorage :: forall c m . (Component c, PrimMonad m, Coercible (ComponentValueFor c) c) => Storage (ComponentKindFor c) (PrimState m) -> Int -> Int -> c -> m ()
{-# INLINE writeStorage #-}
writeStorage !store !row (I# column) el = backing @_ @c
  (\(BoxedStorage cols _) -> do
    boxedCol <- primitive $ \s -> case readSmallArray# cols column s of (# s', mba #) -> (# s', BoxedColumn (MutableArray (unsafeCoerce# mba)) #)
    writeColumn boxedCol row el
    )
  (\(FlatStorage _ cols _) -> do
    flatCol <- primitive $ \s -> case readSmallArray# cols column s of (# s', mba #) -> (# s', FlatColumn (MutableByteArray mba) #)
    writeColumn flatCol row el
    )
  (\_ -> pure ())
  store

growStorage :: forall c m . (KnownComponentKind c, PrimMonad m) => Storage c (PrimState m) -> Int -> m ()
{-# INLINE growStorage #-}
growStorage !store !newSz = go 0
  where
    sz = branchKind @c
          (\(BoxedStorage sm _) -> I# (sizeofSmallMutableArray# sm))
          (\(FlatStorage _ sm _) -> I# (sizeofSmallMutableArray# sm))
          (\(TagStorage sm) -> I# (sizeofSmallMutableArray# sm))
          store 
    go n@(I# n#)
      | n >= sz = pure ()
      | otherwise = do
        () <- branchKind @c
          (\(BoxedStorage cols _) -> do
            -- column
            col <- primitive $ \s -> case readSmallArray# cols n# s of (# s', arr #) -> (# s', MutableArray arr #)
            let oldSz = sizeofMutableArray col
            MutableArray col' <- do
              mar <- newArray newSz (error "growStorage:empty")
              copyMutableArray mar 0 col 0 oldSz
              pure mar
            primitive $ \s -> (# writeSmallArray# cols n# col' s, () #)
            )
          (\(FlatStorage (SizesAndAlignment sizes) cols _) -> do
            -- column
            col <- primitive $ \s -> case readSmallArray# cols n# s of (# s', arr #) -> (# s', MutableByteArray arr #)
            oldSz <- getSizeofMutableByteArray col
            let sizeEl    = fromIntegral $ indexByteArray @Int8 sizes (n * 2)
                alignment = fromIntegral $ indexByteArray @Int8 sizes (n * 2 + 1)
            MutableByteArray col' <- do
              mar <- newAlignedPinnedByteArray (newSz * sizeEl) alignment
              copyMutableByteArray mar 0 col 0 oldSz
              pure mar
            primitive $ \s -> (# writeSmallArray# cols n# col' s, () #)
            )
          (\_ -> pure ())
          store
        -- bitsets
        let sm = branchKind @c (\(BoxedStorage _ sm) -> sm) (\(FlatStorage _ _ sm) -> sm) (\(TagStorage sm) -> sm) store
        bitset <- primitive $ \s -> case readSmallArray# sm n# s of (# s', bs #) -> (# s', MutablePrimArray bs #)
        MutablePrimArray bitset' <- growBitSet bitset newSz
        primitive $ \s -> (# writeSmallArray# sm n# bitset' s, () #)
        -- next
        go (n + 1)

-- Few invariants:
-- the movemask at the end has exactly one int per component in the from storage
-- Removed components in the mask are set to -1
-- If a bitset row is removed, the value at it is always set to enabled/one
-- All unused rows are also all enabled/one
moveTo :: forall c m . (KnownComponentKind c, PrimMonad m) => Storage c (PrimState m) -> Storage c (PrimState m) -> Int -> Int -> Int -> Int -> PrimArray Int -> m () -> m ()
{-# INLINE moveTo #-}
moveTo !fromStore !toStore !last !current !new !toCap !moveMask newBitSetCallback = iterateMoveMask moveMask $ \(I# from) (I# to) -> do
  () <- branchKind @c
    (\(BoxedStorage fromCols _, BoxedStorage toCols _) -> do
      fromCol <- primitive $ \s -> case readSmallArray# fromCols from s of (# s', arr #) -> (# s', MutableArray arr #)
      toCol   <- primitive $ \s -> case readSmallArray# toCols   to   s of (# s', arr #) -> (# s', MutableArray arr #)

      currentEl <- readArray fromCol current
      lastEl    <- readArray fromCol last
      writeArray fromCol last $ error "moveTo:empty"
      when (last /= current) $ writeArray fromCol current lastEl

      writeArray toCol new currentEl
      )
    (\(FlatStorage (SizesAndAlignment fromSizes) fromCols _, FlatStorage _ toCols _) -> do
      fromCol <- primitive $ \s -> case readSmallArray# fromCols from s of (# s', arr #) -> (# s', MutableByteArray arr #)
      toCol   <- primitive $ \s -> case readSmallArray# toCols   to   s of (# s', arr #) -> (# s', MutableByteArray arr #)

      -- Invariant: Sizes and alignment don't change on move
      let sizeEl = fromIntegral $ indexByteArray @Int8 fromSizes (I# from * 2)

      copyMutableByteArray toCol (new * sizeEl) fromCol (current * sizeEl) sizeEl
      when (last /= current) $ copyMutableByteArray fromCol (current * sizeEl) fromCol (last * sizeEl) sizeEl
      )
    (\_ -> pure ())
    (fromStore, toStore)

  -- move bitset values
  let (# sFromBS, sToBS #)
        = branchKind @c
          (\(BoxedStorage _ f, BoxedStorage _ t) -> (# f, t #))
          (\(FlatStorage _ _ f, FlatStorage _ _ t) -> (# f, t #))
          (\(TagStorage f, TagStorage t) -> (# f, t #))
          (fromStore, toStore)

  fromBS <- primitive $ \s -> case readSmallArray# sFromBS from s of (# s', bs #) -> (# s', MutablePrimArray bs #)
  toBS   <- primitive $ \s -> case readSmallArray# sToBS   to   s of (# s', bs #) -> (# s', MutablePrimArray bs #)
  
  szFrom <- getSizeofMutablePrimArray fromBS
  szTo   <- getSizeofMutablePrimArray toBS

  if szFrom == 0
    then pure () -- The component was enabled, the new bitset, empty or not, can stay as it is
    else if szTo == 0
      then do
        newBitSetCallback
        let !nrOfWords = (toCap + 63) `quot` 64
        arr@(MutablePrimArray arr#) <- newPrimArray nrOfWords
        setPrimArray arr 0 nrOfWords (complement 0)
        primitive $ \s -> (# writeSmallArray# sToBS to arr# s, () #)
        moveBitSetVal fromBS arr last current new
      else do
        moveBitSetVal fromBS toBS last current new

iterateMoveMask :: PrimMonad m => PrimArray Int -> (Int -> Int -> m ()) -> m ()
{-# INLINE iterateMoveMask #-}
iterateMoveMask arr f = goMoveMask 0
  where
    sz = sizeofPrimArray arr
    goMoveMask n
      | n >= sz   = pure ()
      | otherwise = do
        if el /= -1 -- Skip all removed columns
          then f n el 
          else pure ()
        goMoveMask (n + 1)
      where
        el = indexPrimArray arr n

moveBitSetVal :: PrimMonad m => MutablePrimArray (PrimState m) Word64 -> MutablePrimArray (PrimState m) Word64 -> Int -> Int -> Int -> m ()
moveBitSetVal oldBS newBS last current new = do
  let lastArrInd  = last `quot` 64
      lastWordInd = last .&. 63
      currentArrInd  = current `quot` 64
      currentWordInd = current .&. 63
      newArrInd  = new `quot` 64
      newWordInd = new .&. 63
  currentW <- readPrimArray oldBS currentArrInd
  let currentSet = unsafeTestBit currentW currentWordInd 
  lastW <- readPrimArray oldBS lastArrInd
  let lastSet = unsafeTestBit lastW lastWordInd
  case (currentSet, lastSet) of
    (True, False)  -> writePrimArray oldBS currentArrInd $ unsafeClearBit currentW currentWordInd
    (False, True)  -> writePrimArray oldBS currentArrInd $ unsafeSetBit   currentW currentWordInd
    _              -> pure ()
  -- Don't forget to keep the invariant that all unset members are enabled
  writePrimArray oldBS lastArrInd $ unsafeSetBit lastW lastWordInd
  newW <- readPrimArray newBS newArrInd
  if not currentSet
    then writePrimArray newBS newArrInd $ unsafeClearBit newW newWordInd
    else pure () -- It is an invariant that unused rows are always enabled

growBitSet :: PrimMonad m => MutablePrimArray (PrimState m) Word64 -> Int -> m (MutablePrimArray (PrimState m) Word64)
growBitSet arr newSz = do
  sz <- getSizeofMutablePrimArray arr
  if sz == 0
    then pure arr
    else do
      let nrWords  = (newSz + 63) `quot` 64
          oldWords = (sz + 63) `quot` 64
      arr' <- newPrimArray nrWords
      copyMutablePrimArray arr' 0 arr 0 oldWords
      setPrimArray arr' oldWords (nrWords - oldWords) (complement 0)
      pure arr'

isEnabled :: forall c m . (KnownComponentKind c, PrimMonad m) => Storage c (PrimState m) -> Int -> Int -> m Bool
isEnabled = branchKind @c
  (\(BoxedStorage   _ bitsets) -> go bitsets)
  (\(FlatStorage  _ _ bitsets) -> go bitsets)
  (\(TagStorage       bitsets) -> go bitsets)
  where
    go !bitsets !row (I# column) = do
      bs <- primitive $ \s -> case readSmallArray# bitsets column s of (# s', bs #) -> (# s', MutablePrimArray bs #)
      sz <- getSizeofMutablePrimArray bs
      if sz == 0
        then pure True
        else do
          let arrInd  = row `quot` 64
              wordInd = row .&. 63
          w :: Word64 <- readPrimArray bs arrInd
          pure $ unsafeTestBit w wordInd

-- "Unsafe" wrappers for Data.Bits methods. Skips the overflow check on shiftL 
unsafeTestBit :: (Bits a, Num a) => a -> Int -> Bool
unsafeTestBit a i = zeroBits /= (a .&. (1 `unsafeShiftL` i)) 

unsafeSetBit, unsafeClearBit :: (Bits a, Num a) => a -> Int -> a
unsafeSetBit   a i = a .|. (1 `unsafeShiftL` i)
unsafeClearBit a i = a .&. (complement $ 1 `unsafeShiftL` i)

enableComponent :: forall c m . (KnownComponentKind c, PrimMonad m) => Storage c (PrimState m) -> Int -> Int -> m ()
enableComponent = branchKind @c
  (\(BoxedStorage   _ bitsets) -> go bitsets)
  (\(FlatStorage  _ _ bitsets) -> go bitsets)
  (\(TagStorage       bitsets) -> go bitsets)
  where
    go !bitsets !row (I# column) = do
      bs <- primitive $ \s -> case readSmallArray# bitsets column s of (# s', bs #) -> (# s', MutablePrimArray bs #)
      sz <- getSizeofMutablePrimArray bs
      if sz == 0
        then pure ()
        else do
          let arrInd  = row `quot` 64
              wordInd = row .&. 63
          w :: Word64 <- readPrimArray bs arrInd
          writePrimArray bs arrInd $ unsafeSetBit w wordInd

disableComponent :: forall c m . (KnownComponentKind c, PrimMonad m) => Storage c (PrimState m) -> Int -> Int -> Int -> m ()
disableComponent = branchKind @c
  (\(BoxedStorage   _ bitsets) -> go bitsets)
  (\(FlatStorage  _ _ bitsets) -> go bitsets)
  (\(TagStorage       bitsets) -> go bitsets)
  where
    go !bitsets !row (I# column) !nrEntities = do
      bs <- primitive $ \s -> case readSmallArray# bitsets column s of (# s', bs #) -> (# s', MutablePrimArray bs #)
      sz <- getSizeofMutablePrimArray bs
      if sz == 0
        then do
          let nrWords = (nrEntities + 63) `quot` 64
          bs'@(MutablePrimArray bs#) <- newPrimArray nrWords
          setPrimArray bs' 0 nrWords (complement 0)
          let arrInd  = row `quot` 64
              wordInd = row .&. 63
          w :: Word64 <- readPrimArray bs' arrInd
          writePrimArray bs' arrInd $ unsafeClearBit w wordInd
          primitive $ \s -> (# writeSmallArray# bitsets column bs# s, () #)
        else do
          let arrInd  = row `quot` 64
              wordInd = row .&. 63
          w :: Word64 <- readPrimArray bs arrInd
          writePrimArray bs arrInd $ unsafeClearBit w wordInd
