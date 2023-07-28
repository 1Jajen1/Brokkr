{-# LANGUAGE MultiWayIf #-}
module Brokkr.Chunk.BlockStates (
  BlockStates(..)
, BlockUpdate(..)
, findBlockChange
, mkBlockChange
, applyBlockUpdate
) where

import Brokkr.BlockState.Internal.BlockState
import Brokkr.BlockState.Internal.Conversion

import Brokkr.PackedVector qualified as Packed
import Brokkr.PackedVector.Mutable.Internal qualified as MPacked

import Brokkr.Util.PalettedVector (PalettedVector)
import Brokkr.Util.PalettedVector qualified as Palette

import Control.Monad (foldM)
import Control.Monad.ST.Strict (runST) 

import Data.Bits
import Data.Coerce
import Data.Maybe

import Data.Primitive.PrimArray (PrimArray)
import Data.Primitive.PrimArray qualified as Prim

import Data.Proxy

import Data.Vector.Storable qualified as S
import Data.Vector.Storable.Mutable qualified as MS

import Data.Word

import GHC.Base (quotInt)
import GHC.TypeLits

import Foreign.Ptr
import Foreign.C.Types (CSize(..), CInt(..))

-- Blocks

type BlockPaletteMaxBitSize = 1 + Log2 (HighestBlockStateId - 1) 

type SectionSize = 4096

data BlockStates = BlockStates !BlockUpdate !Int !(PalettedVector SectionSize BlockPaletteMaxBitSize)
  deriving stock (Eq, Show)

-- Never shrinks the Paletted vector. We do that when writing back out to disk and update it there
applyBlockUpdate :: BlockStates -> BlockStates
applyBlockUpdate bl@(BlockStates NoUpdate _ _) = bl
{-
The copy is annoying, ideas:
- custom allocators could make this cheaper
- ref count the block array could avoid the copy
-}
applyBlockUpdate bl@(BlockStates (SingleBlock pos st) numNonAir pv) =
  case pv of
    Palette.SingleValue old
      | old == coerce st -> bl
      | otherwise ->
        -- Expand to a 4 bits per value paletted vector
        let arr' = runST $ do
              mar <- MPacked.new @('Packed.Static SectionSize) @'Packed.Dynamic 4 -- TODO Move constant 4 to type
              -- TODO Implement in PackedVector. Use SIMD and write Word64s
              !_ <- MPacked.unsafeWithPtr mar $ \ptr -> c_memset (coerce ptr) 0 2048
              MPacked.unsafeWrite mar pos 1
              Packed.unsafeFreeze mar
            palette = S.fromListN 2 [old, coerce st]
            pv' = Palette.Indirect palette arr'
        in BlockStates NoUpdate (updateNumNonAir (coerce old) st numNonAir) pv'
    -- Since we never shrink, just copy and apply the update
    Palette.Global arr -> runST $ do
      mar <- Packed.thaw arr
      old <- MPacked.unsafeExchange mar pos $ coerce st
      arr' <- Packed.unsafeFreeze mar
      pure $ BlockStates NoUpdate (updateNumNonAir (coerce old) st numNonAir) (Palette.Global arr')
    Palette.Indirect palette arr
      -- If we have the block already in the palette, use that index
      | Just ind <- S.findIndex (== coerce st) palette -> runST $ do
        mar <- Packed.thaw arr
        oldInd <- MPacked.unsafeExchange mar pos ind
        let old = S.unsafeIndex palette oldInd
        arr' <- Packed.unsafeFreeze mar
        pure $ BlockStates NoUpdate (updateNumNonAir (coerce old) st numNonAir) (Palette.Indirect palette arr')
      -- New blockstate, grow the palette
      -- PackedVector handles the new bit sizes in unsafeCopy
      | otherwise -> runST $ do
        let len = S.length palette
            newLen = len + 1
            newBitSz = countTrailingZeros newLen
        mPalette <- MS.unsafeNew newLen
        S.unsafeCopy (MS.unsafeSlice 0 len mPalette) palette
        MS.unsafeWrite mPalette len $ coerce st

        palette' <- S.unsafeFreeze mPalette
        
        mar <- MPacked.new @('Packed.Static SectionSize) @'Packed.Dynamic newBitSz
        -- only used for the copy
        oldMut <- Packed.unsafeThaw arr
        MPacked.unsafeCopy mar oldMut

        oldInd <- MPacked.unsafeExchange mar pos len
        let old = S.unsafeIndex palette oldInd

        arr' <- Packed.unsafeFreeze mar

        pure $ BlockStates NoUpdate (updateNumNonAir (coerce old) st numNonAir) (Palette.Indirect palette' arr')

applyBlockUpdate bl@(BlockStates (MultiBlock upds) numNonAir pv) =
  case pv of
    -- Easiest case, just write the new updates one by one and update the non air counter
    Palette.Global arr -> runST $ do
      mar <- Packed.thaw arr
      numNonAir' <- foldM (\acc blockChange -> do
        let pos = blockChange .&. 0xFFF
            st  = blockChange `unsafeShiftR` 12
        old <- MPacked.unsafeExchange mar pos st
        pure $ updateNumNonAir (coerce old) (coerce st) acc
        ) numNonAir $ Prim.primArrayToList upds
      arr' <- Packed.unsafeFreeze mar
      pure $ BlockStates NoUpdate numNonAir' (Palette.Global arr')
    Palette.SingleValue old -> runST $ do
      mPalette <- MS.unsafeNew (Prim.sizeofPrimArray upds + 1)
      MS.unsafeWrite mPalette 0 old
      finSz <- foldM (\sz blockChange -> do
        let st = blockChange `unsafeShiftR` 12
        temp <- S.unsafeFreeze $ MS.unsafeSlice 0 sz mPalette
        if S.elem st temp
          then pure sz
          else MS.unsafeWrite mPalette sz st >> pure (sz + 1)
        ) 1 $ Prim.primArrayToList upds
      
      let newBitSz = countTrailingZeros finSz
      if | finSz == 1 -> pure bl
         | newBitSz < 9 -> do -- TODO Actual correct number and use a constant value instead of a literal
          palette' <- S.unsafeFreeze $ MS.unsafeSlice 0 finSz mPalette
          mar <- MPacked.new @('Packed.Static SectionSize) @'Packed.Dynamic newBitSz
          let perWord = quotInt 64 newBitSz
              len = fromIntegral $ natVal (Proxy @SectionSize)
              wSz = (len + perWord - 1) `quotInt` perWord
          -- TODO Implement in PackedVector. Use SIMD and write Word64s
          !_ <- MPacked.unsafeWithPtr mar $ \ptr -> c_memset (coerce ptr) 0 . fromIntegral $ wSz * 8

          numNonAir' <- foldM (\acc blockChange -> do
            let pos = blockChange .&. 0xFFF
                st  = blockChange `unsafeShiftR` 12
                ind = fromMaybe (error "TODO Errors. Block not in updated palette") $ S.findIndex (== st) palette'
            MPacked.unsafeWrite mar pos ind
            pure $ updateNumNonAir (coerce old) (coerce st) acc
            ) numNonAir $ Prim.primArrayToList upds

          arr' <- Packed.unsafeFreeze mar
          pure $ BlockStates NoUpdate numNonAir' (Palette.Indirect palette' arr')
         | otherwise -> do
          -- Global palette
          _mar <- MPacked.new @('Packed.Static SectionSize) @('Packed.Static BlockPaletteMaxBitSize)
          -- TODO This is not done! We need to write old value everywhere
          error "Not done! SingleValue -> Global update"

          -- numNonAir' <- foldM (\acc blockChange -> do
          --   let pos = blockChange .&. 0xFFF
          --       st  = blockChange `unsafeShiftR` 12
          --   MPacked.unsafeWrite mar pos st
          --   pure $ updateNumNonAir (coerce old) (coerce st) acc
          --   ) numNonAir $ Prim.primArrayToList upds

          -- arr' <- Packed.unsafeFreeze mar
          -- pure $ BlockStates NoUpdate numNonAir' (Palette.Global arr')
    Palette.Indirect palette arr -> runST $ do
      mPalette <- MS.unsafeNew (S.length palette + Prim.sizeofPrimArray upds)
      S.unsafeCopy (MS.unsafeSlice 0 (S.length palette) mPalette) palette 
      finSz <- foldM (\sz blockChange -> do
        let st = blockChange `unsafeShiftR` 12
        temp <- S.unsafeFreeze $ MS.unsafeSlice 0 sz mPalette
        if S.elem st temp
          then pure sz
          else MS.unsafeWrite mPalette sz st >> pure (sz + 1)
        ) (S.length palette) $ Prim.primArrayToList upds
      
      let newBitSz = countTrailingZeros finSz
      palette' <- if finSz == S.length palette
        then pure palette
        else S.unsafeFreeze mPalette
      
      if newBitSz < 9
        then do
          mar <- MPacked.new @('Packed.Static SectionSize) @'Packed.Dynamic newBitSz
          tmpOld <- Packed.unsafeThaw arr
          MPacked.unsafeCopy mar tmpOld

          numNonAir' <- foldM (\acc blockChange -> do
            let pos = blockChange .&. 0xFFF
                st  = blockChange `unsafeShiftR` 12
                ind = fromMaybe (error "TODO Errors. Block not in updated palette") $ S.findIndex (== st) palette'
            oldInd <- MPacked.unsafeExchange mar pos ind
            let old = S.unsafeIndex palette' oldInd
            pure $ updateNumNonAir (coerce old) (coerce st) acc
            ) numNonAir $ Prim.primArrayToList upds

          arr' <- Packed.unsafeFreeze mar
          pure $ BlockStates NoUpdate numNonAir' (Palette.Indirect palette' arr')
        else do
          _mar <- MPacked.new @('Packed.Static SectionSize) @'Packed.Dynamic newBitSz
          error "Not done yet. Paletted -> Global update"


foreign import ccall unsafe "string.h memset" c_memset :: Ptr Word8 -> CInt -> CSize -> IO (Ptr Word8)

updateNumNonAir :: BlockState -> BlockState -> Int -> Int
updateNumNonAir old new = case (isAir old, isAir new) of
  (False, True) -> \x -> x - 1
  (True, False) -> (+ 1)
  _ -> id
  where
    -- Check how this is compiled? if it is a single case statement, great, if not, make ghc generate that
    isAir Air     = True
    isAir CaveAir = True
    isAir VoidAir = True
    isAir _ = False

data BlockUpdate =
    NoUpdate
  | SingleBlock {-# UNPACK #-} !Int {-# UNPACK #-} !BlockState
  | MultiBlock {-# UNPACK #-} !(PrimArray Int)
  deriving stock (Show, Eq)

mkBlockChange :: Int -> Int -> Int -> BlockState -> BlockUpdate
{-# INLINE mkBlockChange #-}
mkBlockChange x y z = SingleBlock (x `unsafeShiftL` 8 .|. z `unsafeShiftL` 4 .|. y)

instance Monoid BlockUpdate where
  mempty = NoUpdate

instance Semigroup BlockUpdate where
  NoUpdate <> m = m
  m <> NoUpdate = m

  SingleBlock posL _ <> sr@(SingleBlock posR _)
    | posL == posR = sr
  SingleBlock posL stateL <> SingleBlock posR stateR = MultiBlock $ Prim.runPrimArray $ do
    mar <- Prim.newPrimArray initSz
    let leftChange  = unsafeToBlockChange posL stateL
        rightChange = unsafeToBlockChange posR stateR
        (a,b) = if posL > posR then (rightChange, leftChange) else (leftChange, rightChange)
    Prim.writePrimArray mar 0 a
    Prim.writePrimArray mar 1 b
    pure mar
    where initSz = 4
  
  SingleBlock pos state <> mr@(MultiBlock arr) = findBlockChange pos arr
    -- Block is updated on the right, so ignore the left side
    (\_ _ -> mr)
    -- Need to insert the block
    $ \i -> MultiBlock $ Prim.runPrimArray $ do
      let sz = Prim.sizeofPrimArray arr
      mar <- Prim.newPrimArray (sz + 1)

      Prim.copyPrimArray mar 0 arr 0 i
      Prim.writePrimArray mar i $ unsafeToBlockChange pos state
      Prim.copyPrimArray mar (i + 1) arr i (sz - i)

      pure mar
  
  MultiBlock arr <> SingleBlock pos state = findBlockChange pos arr
    (\i _ -> MultiBlock $ Prim.runPrimArray $ do
      let sz = Prim.sizeofPrimArray arr
      mar <- Prim.thawPrimArray arr 0 sz
      Prim.writePrimArray mar i $ unsafeToBlockChange pos state
      pure mar
      )
    $ \i -> MultiBlock $ Prim.runPrimArray $ do
      let sz = Prim.sizeofPrimArray arr
      mar <- Prim.newPrimArray (sz + 1)

      Prim.copyPrimArray mar 0 arr 0 i
      Prim.writePrimArray mar i $ unsafeToBlockChange pos state
      Prim.copyPrimArray mar (i + 1) arr i (sz - i)

      pure mar

  MultiBlock lArr <> MultiBlock rArr = MultiBlock $ runST $ do
    let szL = Prim.sizeofPrimArray lArr
        szR = Prim.sizeofPrimArray rArr
    mar <- Prim.newPrimArray (szL + szR)
    let go !l !r !acc
          | l >= szL && r >= szR = pure acc
          | l >= szL = do
            let rRem = szR - r
            Prim.copyPrimArray mar acc rArr r rRem
            pure $ acc + rRem
          | r >= szR = do
            let lRem = szL - l
            Prim.copyPrimArray mar acc lArr l lRem
            pure $ acc + lRem
          | otherwise = do
            let lEl = Prim.indexPrimArray lArr l
            let rEl = Prim.indexPrimArray rArr r
            case compare (lEl .&. 0xFFF) (rEl .&. 0xFFF) of
              EQ -> do
                Prim.writePrimArray mar acc rEl
                go (l + 1) (r + 1) (acc + 1)
              LT -> do
                Prim.writePrimArray mar acc lEl
                go (l + 1) r (acc + 1)
              GT -> do
                Prim.writePrimArray mar acc rEl
                go l (r + 1) (acc + 1)

    n <- go 0 0 0

    if n == szL + szR then Prim.unsafeFreezePrimArray mar
    else Prim.freezePrimArray mar 0 n

unsafeToBlockChange :: Int -> BlockState -> Int
{-# INLINE unsafeToBlockChange #-}
unsafeToBlockChange p st = coerce st `unsafeShiftL` 12 .|. p

findBlockChange :: Int -> PrimArray Int -> (Int -> BlockState -> r) -> (Int -> r) -> r
{-# INLINE findBlockChange #-}
findBlockChange pos arr onFind onFail = go 0
  where
    sz = Prim.sizeofPrimArray arr
    go !n
      | n >= sz = onFail n
      | otherwise =
        let el = Prim.indexPrimArray arr n
        in case compare (el .&. 0xFFF) pos of
          EQ -> onFind n . coerce $ el `unsafeShiftR` 12
          LT -> go (n + 1)
          GT -> onFail n
