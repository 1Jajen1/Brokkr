{-# LANGUAGE MagicHash, UnliftedDatatypes, UnboxedTuples #-}
module Brokkr.NBT.Slice (
  Slice(..)
, fromList
, emptySlice
, findWithIndex
) where

import GHC.Exts hiding (fromList)
import GHC.ST

emptySlice :: Slice a
emptySlice = runST $ ST $ \s ->
  case newSmallArray# 0# (error "Empty slice") s of
    (# s', mar #) -> case unsafeFreezeSmallArray# mar s' of
      (# s'', arr #) -> (# s'', Slice arr 0# #)

data Slice a = Slice (SmallArray# a) Int#

fromList :: [a] -> Slice a
fromList [] = emptySlice
fromList xs0 = runST $ ST $ \s0 ->
  case newSmallArray# len (error "Slice.fromList") s0 of
    (# s1, mar #) -> case unsafeFreezeSmallArray# mar (go mar 0# xs0 s1) of
      (# s2, arr #) -> (# s2, Slice arr len #) 
  where
    !(I# len) = length xs0
    go _ _ [] s = s
    go mar n (x:xs) s = go mar (n +# 1#) xs (writeSmallArray# mar n x s)

findWithIndex :: Ord b => (a -> b) -> b -> Slice a -> (# Int#, (# a | (##) #) #)
findWithIndex onEl el (Slice arr sz) = goBin 0# sz
  where
    -- maxBinSearchLen = 8#
    goBin l u
      | isTrue# (l >=# u) = (# l, (# | (##) #) #)
      -- | isTrue# ((u -# l) ==# maxBinSearchLen) = goLin u l
      | otherwise =
        let (# a #) = indexSmallArray# arr mid#
        in case compare (onEl a) el of
          LT -> goBin (mid# +# 1#) u
          GT -> goBin l mid#
          EQ -> (# mid#, (# a | #) #)
      where mid# = (l +# u) `quotInt#` 2#
    -- goLin l u
    --   | isTrue# (l >=# u) = (# l, (# | (##) #) #)
    --   | otherwise =
    --     let (# a #) = indexSmallArray# arr l
    --     in case compare el (onEl a) of
    --       LT -> (# l, (# | (##) #) #)
    --       GT -> goLin (l +# 1#) u
    --       EQ -> (# l, (# a | #) #)

instance Foldable Slice where
  length (Slice _ sz) = I# sz
  {-# INLINE length #-}
  foldMap f (Slice arr sz) = go 0# mempty
    where
      go n acc
        | isTrue# (n >=# sz) = acc
        | otherwise = f el <> go (n +# 1#) acc
          where (# el #) = indexSmallArray# arr n
  {-# INLINE foldMap #-}

instance Eq a => Eq (Slice a) where
  Slice larr lsz == Slice rarr rsz = isTrue# (lsz ==# rsz) && go 0#
    where
      go n
        | isTrue# (n >=# lsz) = True
        | otherwise = l == r && go (n +# 1#)
          where
            (# l #) = indexSmallArray# larr n
            (# r #) = indexSmallArray# rarr n

instance Show a => Show (Slice a) where
  show = show . foldr (:) []
