{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE CPP #-}
module Brokkr.NBT.Tape (
  NBTTape
, NBTParser
, newNBTParser
, ensureCapacity
, parseTape
) where

import Brokkr.ModifiedUtf8
import Brokkr.NBT.NBTString.Internal

import Control.DeepSeq


import Data.Bits
import Data.ByteString
import Data.ByteString.Internal qualified as BS
import Data.Coerce
import Data.Int
import Data.Primitive
import Data.Vector.Storable qualified as S

import FlatParse.Basic qualified as FP
import Foreign.C.Types

import GHC.Exts
import GHC.Float
import GHC.ForeignPtr
import GHC.Int
import GHC.ST
import GHC.IO
import GHC.Word

import Text.Show (showListWith)

import Debug.Trace

-- Rename to on demand/lazy or something?

data NBTTape = NBTTape {
  nbtTape :: {-# UNPACK #-} !(ForeignPtr TapeElement)
, nbtTapeSize :: {-# UNPACK #-} !Int
, nbtData :: {-# UNPACK #-} !ByteString
}

instance Show NBTTape where
  show = showTape

instance NFData NBTTape where
  {-# INLINE rnf #-}
  rnf !_ = ()

data NBTParser = NBTParser {
  parserTape :: {-# UNPACK #-} !(ForeignPtr TapeElement)
, parserCap :: {-# UNPACK #-} !Int
}

instance NFData NBTParser where
  {-# INLINE rnf #-}
  rnf !_ = ()

newNBTParser :: Int -> IO NBTParser
-- TODO Use malloc here instead? Does ghc write and thus access the virtual memory?
newNBTParser byteLen = NBTParser <$> mallocPlainForeignPtrBytes cap <*> pure byteLen
  where cap = byteLen * 2 * 8

ensureCapacity :: Int -> NBTParser -> IO NBTParser
ensureCapacity byteLen p@(NBTParser _ cap)
  | byteLen <= cap = pure p
  | otherwise = newNBTParser byteLen

data ListOrCompound = IsList | IsCompound
  deriving stock (Eq, Show)

data TapeElement
  = TapeByte {-# UNPACK #-} !Int8
  | TapeShort {-# UNPACK #-} !Int16
  | TapeInt {-# UNPACK #-} !Int32
  | TapeLong {-# UNPACK #-} !Int64
  | TapeFloat {-# UNPACK #-} !Word32 -- Not reading as float. Just let users convert as its not free
  | TapeDouble {-# UNPACK #-} !Word64 -- Same as for float
  | TapeByteArray {-# UNPACK #-} !Int32
  | TapeIntArray {-# UNPACK #-} !Int32
  | TapeLongArray {-# UNPACK #-} !Int32
  | TapeString {-# UNPACK #-} !Int32 {-# UNPACK #-} !Word16
  | TapeOpenCompound {-# UNPACK #-} !Int32
  | TapeCloseCompound {-# UNPACK #-} !Int32
  | TapeOpenList {-# UNPACK #-} !Word8 {-# UNPACK #-} !Int32
  | TapeCloseList {-# UNPACK #-} !Int32
  deriving stock (Eq, Show)

-- Uncomment if you want to see the values in the original data 
showTape :: NBTTape -> String
-- showTape tape@(NBTTape _ _ (BS.BS (ForeignPtr start fp) (I# bSz))) = runST $ ST $ \s0 ->
--   keepAlive# fp s0 $ \s -> (# s, ($ []) . showListWith showEl $ foldrTape (:) [] tape #)
showTape = ($ []) . showListWith showEl . foldrTape (:) []
  where
    -- stringAt (I32# i) =
    --   let off = bSz -# int32ToInt# i
    --       curr = plusAddr# start off
    --       sz = fromIntegral . byteSwap16 $ W16# (indexWord16OffAddr# curr 0#)
    --   in NBTString $ unsafeMkModifiedUtf8 $ BS.BS (ForeignPtr (plusAddr# curr 2#) fp) sz
    -- arrAt (I32# i) =
    --   let off = bSz -# int32ToInt# i
    --       curr = plusAddr# start off
    --       sz = fromIntegral . byteSwap32 $ W32# (indexWord32OffAddr# curr 0#)
    --   in S.unsafeFromForeignPtr0 (ForeignPtr (plusAddr# curr 4#) fp) sz
    showEl :: TapeElement -> String -> String
    showEl tapeEl = case tapeEl of
      TapeByte b -> showString "Byte " . shows b
      TapeShort s -> showString "Short " . shows s
      TapeInt i -> showString "Int " . shows i
      TapeLong l -> showString "Long " . shows l
      TapeFloat f -> showString "Float " . shows (castWord32ToFloat f)
      TapeDouble d -> showString "Double " . shows (castWord64ToDouble d)
      TapeString si sl -> showString "String " . shows (si, sl) -- (stringAt si)
      TapeByteArray bi -> showString "ByteArray " . shows bi -- (arrAt bi :: S.Vector Int8)
      TapeIntArray ii -> showString "IntArray " .shows ii -- (arrAt ii :: S.Vector Int32)
      TapeLongArray li -> showString "LongArray " . shows li -- (arrAt li :: S.Vector Int64)
      TapeOpenList t i -> showString "OpenList " . shows t . showString " " . shows i
      TapeCloseList i -> showString "CloseList " . shows i
      TapeOpenCompound i -> showString "OpenCompound " . shows i
      TapeCloseCompound i -> showString "CloseCompound " . shows i

foldrTape :: (TapeElement -> r -> r) -> r -> NBTTape -> r
{-# INLINE foldrTape #-}
foldrTape f z tape = iterateTape# tape f z

iterateTape# :: NBTTape -> (TapeElement -> r -> r) -> r -> r
{-# INLINE iterateTape# #-}
iterateTape# (NBTTape (ForeignPtr start fp) (I# sz) _) f z0 = runST $ ST $ \s0 -> keepAlive# fp s0 $ \s -> (# s, goIterate start z0 #) 
  where
    end = plusAddr# start (sz *# 8#)
    goIterate ptr z
      | isTrue# (end `eqAddr#` ptr) = z0
      | otherwise =
        let w = indexWordOffAddr# ptr 0#
            w2 = indexWordOffAddr# ptr 1#
            tag = w `uncheckedShiftRL#` 56#
            low = w `and#` (1## `uncheckedShiftL#` 32# `minusWord#` 1##)
            w16 = wordToWord16# (w `uncheckedShiftRL#` 32# `and#` (1## `uncheckedShiftL#` 16# `minusWord#` 1##))
            listTag = wordToWord8# (w `uncheckedShiftRL#` 48#)
        in case W# tag of
          TAG_BYTE -> f (TapeByte $ I8# (word8ToInt8# (wordToWord8# low))) $ goIterate (plusAddr# ptr 8#) z
          TAG_SHORT -> f (TapeShort $ I16# (word16ToInt16# (wordToWord16# low))) $ goIterate (plusAddr# ptr 8#) z
          TAG_INT -> f (TapeInt $ I32# (word32ToInt32# (wordToWord32# low))) $ goIterate (plusAddr# ptr 8#) z
          TAG_LONG -> f (TapeLong $ I64# (word64ToInt64# (wordToWord64# w2))) $ goIterate (plusAddr# ptr 16#) z
          TAG_FLOAT -> f (TapeFloat $ W32# (wordToWord32# low)) $ goIterate (plusAddr# ptr 8#) z
          TAG_DOUBLE -> f (TapeDouble $ W64# (wordToWord64# w2)) $ goIterate (plusAddr# ptr 16#) z
          TAG_STRING -> f (TapeString (I32# (word32ToInt32# (wordToWord32# low))) (W16# w16)) $ goIterate (plusAddr# ptr 8#) z
          TAG_BYTE_ARRAY -> f (TapeByteArray $ I32# (word32ToInt32# (wordToWord32# low))) $ goIterate (plusAddr# ptr 8#) z
          TAG_INT_ARRAY -> f (TapeIntArray $ I32# (word32ToInt32# (wordToWord32# low))) $ goIterate (plusAddr# ptr 8#) z
          TAG_LONG_ARRAY -> f (TapeLongArray $ I32# (word32ToInt32# (wordToWord32# low))) $ goIterate (plusAddr# ptr 8#) z
          TAG_OPEN_LIST -> f (TapeOpenList (W8# listTag) $ I32# (word32ToInt32# (wordToWord32# low))) $ goIterate (plusAddr# ptr 8#) z
          TAG_CLOSE_LIST -> f (TapeCloseList $ I32# (word32ToInt32# (wordToWord32# low))) $ goIterate (plusAddr# ptr 8#) z
          TAG_OPEN_COMPOUND -> f (TapeOpenCompound $ I32# (word32ToInt32# (wordToWord32# low))) $ goIterate (plusAddr# ptr 8#) z
          TAG_CLOSE_COMPOUND -> f (TapeCloseCompound $ I32# (word32ToInt32# (wordToWord32# low))) $ goIterate (plusAddr# ptr 8#) z

          !t -> error $ "unhandled tag " ++ show t ++ " with words " ++ show (W# w, W# w2) ++ " at index " ++ (show $ I# (minusAddr# end ptr))

parseTape :: NBTParser -> FP.Parser e NBTTape
parseTape !NBTParser{..} = do
  whole <- FP.ParserT $ \fp end curr s -> FP.OK# s (BS.BS (ForeignPtr curr fp) (I# (minusAddr# end curr))) curr
  Ptr end <- FP.ParserT $ \_ end curr s -> FP.OK# s (Ptr end) curr
  Ptr curr <- FP.ParserT $ \_ _ curr s -> FP.OK# s (Ptr curr) curr

  tapeLastC <- localST $ FP.liftST $ unsafeIOToST (c_parseIntoTape (Ptr curr) (fromIntegral $ I# (minusAddr# end curr)) 0 (coerce $ unsafeForeignPtrToPtr parserTape))
  let tapeLast = fromIntegral tapeLastC
  if tapeLast == 0
    then FP.empty
    else do
      FP.ParserT $ \_ _ _ s -> FP.OK# s () end;
      pure $ NBTTape parserTape tapeLast whole
-- parseTape !NBTParser{..} = localST $ do
--   whole <- FP.ParserT $ \fp end curr s -> FP.OK# s (BS.BS (ForeignPtr curr fp) (I# (minusAddr# end curr))) curr
--   tag0 <- FP.anyWord8
-- 
--   !(I# keySz0) <- fromIntegral <$> FP.anyWord16be
--   _ <- FP.takeUnsafe# keySz0
-- 
--   ind0 <- FP.unPos <$> FP.getPos
-- 
--   let tapePtr = unsafeForeignPtrToPtr parserTape
-- 
--   --traceM "Start Tape"
-- 
--   goParseTag tapePtr 0 ind0 tag0 $ \tapeLast -> do
--     --traceM $ "End Tape " ++ show tapeLast 
--     pure $ NBTTape parserTape tapeLast whole

unpackParserInt# :: ForeignPtrContents -> Addr# -> Addr# -> State# s -> FP.ParserST s e Int -> (# (# State# s, Addr#, Int# #) | (# State# s #) #)
{-# INLINE unpackParserInt# #-}
unpackParserInt# fp end curr s p = case FP.runParserT# p fp end curr s of
      FP.OK# s' (I# i) curr' -> (# (# s', curr', i #) | #)
      FP.Fail# s' -> (# | (# s' #) #)
      FP.Err# s' _ -> (# | (# s' #) #)

goParseTag :: Ptr TapeElement -> Int -> Int -> Word8 -> (Int -> FP.ParserST s e r) -> FP.ParserST s e r
{-# INLINE goParseTag #-}
goParseTag !tapePtr !tapeIndex !ind !tagTop f = FP.ParserT $ \fp end curr s -> case goParseTag# tapePtr tapeIndex ind tagTop fp end curr s of
  (# | (# s' #) #) -> FP.Fail# s'
  (# (# s', curr', i #) | #) -> case f (I# i) of FP.ParserT g -> g fp end curr' s'
 -- where tapeIndex = trace ("Ind: " ++ show tapeIndex0 ++ " Tag: " ++ show tagTop) tapeIndex0
-- TODO Give this a smaller core to reduce stack moves

goParseTag# :: Ptr TapeElement -> Int -> Int -> Word8 -> ForeignPtrContents -> Addr# -> Addr# -> State# s -> (# (# State# s, Addr#, Int# #) | (# State# s #) #)
goParseTag# !tapePtr !tapeIndex !ind !tagTop fpco end curr s = case tagTop of
  1 -> unpackParserInt# fpco end curr s $ do
    w8 <- FP.anyInt8
    writeTape tapePtr tapeIndex $ TapeByte w8
  2 -> unpackParserInt# fpco end curr s $ do
    w16 <- FP.anyInt16be
    writeTape tapePtr tapeIndex $ TapeShort w16
  3 -> unpackParserInt# fpco end curr s $ do
    w32 <- FP.anyInt32be
    writeTape tapePtr tapeIndex $ TapeInt w32
  4 -> unpackParserInt# fpco end curr s $ do
    w64 <- FP.anyInt64be
    writeTape tapePtr tapeIndex $ TapeLong w64
  5 -> unpackParserInt# fpco end curr s $ do
    f <- FP.anyWord32be
    writeTape tapePtr tapeIndex $ TapeFloat f
  6 -> unpackParserInt# fpco end curr s $ do
    d <- FP.anyWord64be
    writeTape tapePtr tapeIndex $ TapeDouble d
  7 -> unpackParserInt# fpco end curr s $ withAnyWord32be $ \(W32# w32) -> do
    let sz = word2Int# (word32ToWord# w32)
    _ <- FP.takeUnsafe# sz
    writeTape tapePtr tapeIndex . TapeByteArray $ fromIntegral ind
  11 -> unpackParserInt# fpco end curr s $ withAnyWord32be $ \(W32# w32) -> do
    let sz = word2Int# (word32ToWord# w32)
    _ <- FP.takeUnsafe# (sz *# 4#)
    writeTape tapePtr tapeIndex . TapeIntArray $ fromIntegral ind
  12 -> unpackParserInt# fpco end curr s $ withAnyWord32be $ \(W32# w32) -> do
    let sz = word2Int# (word32ToWord# w32)
    _ <- FP.takeUnsafe# (sz *# 8#)
    writeTape tapePtr tapeIndex . TapeLongArray $ fromIntegral ind
  8 -> unpackParserInt# fpco end curr s $ withAnyWord16be $ \(W16# w16) -> do
    let sz = word2Int# (word16ToWord# w16)
    _ <- FP.takeUnsafe# sz
    writeTape tapePtr tapeIndex $ TapeString (fromIntegral ind) (W16# w16)
  9 -> unpackParserInt# fpco end curr s $ withWord8AndWord32 $ \tag (W32# sz) -> do
    let goList n0 z0 f =
          let go n z
                | isTrue# (n <=# 0#) = f $ I# z
                | otherwise = do
                  ind' <- FP.unPos <$> FP.getPos
                  goParseTag tapePtr (I# z) ind' tag $ \(I# i) -> go (n -# 1#) i
          in go n0 z0
    --traceM $ "Open list " ++ show tapeIndex
    goList (int32ToInt# (word32ToInt32# sz)) (let !(I# i) = tapeIndex + 1 in i) $ \tapeIndex' -> do
      --traceM $ "Close list " ++ show (tapeIndex, tapeIndex')
      _ <- writeTape tapePtr tapeIndex $ TapeOpenList tag (fromIntegral tapeIndex')
      writeTape tapePtr tapeIndex' . TapeCloseList $ fromIntegral tapeIndex
  10 -> unpackParserInt# fpco end curr s $ do
    let goCompound z0 f =
          let go z = do
                FP.anyWord8 >>= \case
                  0 -> f $ I# z
                  tag -> withAnyWord16be $ \(W16# w16) -> do
                    let sz = word2Int# (word16ToWord# w16)
                    _ <- FP.takeUnsafe# sz
                    ind' <- FP.unPos <$> FP.getPos
                    goParseTag tapePtr (I# z) ind' tag $ \(I# tapeI') -> go tapeI'
          in go z0
    --traceM $ "Open comp " ++ show tapeIndex
    goCompound (let !(I# i) = tapeIndex + 1 in i) $ \tapeIndex' -> do
      --straceM $ "Close comp " ++ show (tapeIndex, tapeIndex')
      _ <- writeTape tapePtr tapeIndex $ TapeOpenCompound (fromIntegral tapeIndex')
      writeTape tapePtr tapeIndex' . TapeCloseCompound $ fromIntegral tapeIndex

  !t -> error $ "Unhandled Tag " ++ show t

withWord8AndWord32 :: (Word8 -> Word32 -> FP.ParserT st e a) -> FP.ParserT st e a
{-# INLINE withWord8AndWord32 #-}
withWord8AndWord32 f = FP.ParserT $ \fp end curr s ->
  case 3# <=# minusAddr# end curr of
    1# -> case f (W8# (indexWord8OffAddr# curr 0#)) (byteSwap32 $ W32# (indexWord32OffAddr# (plusAddr# curr 1#) 0#)) of
      FP.ParserT g -> g fp end (plusAddr# curr 5#) s
    _ -> FP.Fail# s

withAnyWord16be :: (Word16 -> FP.ParserT st e a) -> FP.ParserT st e a
{-# INLINE withAnyWord16be #-}
#ifdef WORDS_BIGENDIAN
withAnyWord16be = FP.withAnyWord16 pure
#else
-- Going byteSwap16 does wordToWord16 . byteSwap16 . word16ToWord
-- This involves an and# (2^16 - 1) which should be redundant?
-- We can remove that by converting the 'Word' result of byteSwap16#
-- directly, but that gives basically no speed up, so not worth
withAnyWord16be f = FP.withAnyWord16 (f . byteSwap16)
#endif

withAnyWord32be :: (Word32 -> FP.ParserT st e a) -> FP.ParserT st e a
{-# INLINE withAnyWord32be #-}
#ifdef WORDS_BIGENDIAN
withAnyWord32be = FP.withAnyWord32 pure
#else
-- Going byteSwap16 does wordToWord16 . byteSwap16 . word16ToWord
-- This involves an and# (2^16 - 1) which should be redundant?
-- We can remove that by converting the 'Word' result of byteSwap16#
-- directly, but that gives basically no speed up, so not worth
withAnyWord32be f = FP.withAnyWord32 (f . byteSwap32)
#endif

writeTape :: Ptr TapeElement -> Int -> TapeElement -> FP.ParserST s e Int
{-# INLINE writeTape #-}
writeTape (Ptr addr) o@(I# off) = \case
  TapeOpenList tag ind ->
    let !(W# el) = (TAG_OPEN_LIST `unsafeShiftL` 56) .|. (fromIntegral tag `unsafeShiftL` 48) .|. fromIntegral ind
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeCloseList ind ->
    let !(W# el) = (TAG_CLOSE_LIST `unsafeShiftL` 56) .|. fromIntegral ind
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeOpenCompound ind ->
    let !(W# el) = (TAG_OPEN_COMPOUND `unsafeShiftL` 56) .|. fromIntegral ind
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeCloseCompound ind ->
    let !(W# el) = (TAG_CLOSE_COMPOUND `unsafeShiftL` 56) .|. fromIntegral ind
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeByte b ->
    let !(W# el) = (TAG_BYTE `unsafeShiftL` 56) .|. fromIntegral b
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeShort sh ->
    let !(W# el) = (TAG_SHORT `unsafeShiftL` 56) .|. fromIntegral sh
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeInt i ->
    let !(W# el) = (TAG_INT `unsafeShiftL` 56) .|. fromIntegral i
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeLong (I64# l) ->
    let !(W# el) = (TAG_LONG `unsafeShiftL` 56)
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el (writeInt64OffAddr# addr (off +# 1#) l s), o + 2 #)
  TapeFloat f ->
    let !(W# el) = (TAG_FLOAT `unsafeShiftL` 56) .|. fromIntegral f
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeDouble (W64# d) ->
    let !(W# el) = (TAG_DOUBLE `unsafeShiftL` 56)
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el (writeWord64OffAddr# addr (off +# 1#) d s), o + 2 #)
  TapeString ind len ->
    let !(W# el) = (TAG_STRING `unsafeShiftL` 56) .|. (fromIntegral len `unsafeShiftL` 32) .|. fromIntegral ind
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeByteArray ind ->
    let !(W# el) = (TAG_BYTE_ARRAY `unsafeShiftL` 56) .|. fromIntegral ind
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeIntArray ind ->
    let !(W# el) = (TAG_INT_ARRAY `unsafeShiftL` 56) .|. fromIntegral ind
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)
  TapeLongArray ind ->
    let !(W# el) = (TAG_LONG_ARRAY `unsafeShiftL` 56) .|. fromIntegral ind
    in FP.liftST . ST $ \s -> (# writeWordOffAddr# addr off el s , o + 1 #)

localST :: forall st e a . (forall s . FP.ParserST s e a) -> FP.ParserT st e a
{-# INLINE localST #-}
localST p = FP.ParserT $ \fp end curr st -> case runRW# (FP.runParserT# p fp end curr) of
  (# _, res #) -> (# st, res #)

pattern TAG_BYTE, TAG_SHORT, TAG_INT, TAG_LONG, TAG_FLOAT, TAG_DOUBLE, TAG_STRING, TAG_BYTE_ARRAY, TAG_INT_ARRAY, TAG_LONG_ARRAY, TAG_OPEN_LIST, TAG_CLOSE_LIST, TAG_OPEN_COMPOUND, TAG_CLOSE_COMPOUND :: Word
pattern TAG_BYTE = 1
pattern TAG_SHORT = 2
pattern TAG_INT = 3
pattern TAG_LONG = 4
pattern TAG_FLOAT = 5
pattern TAG_DOUBLE = 6
pattern TAG_STRING = 7
pattern TAG_BYTE_ARRAY = 8
pattern TAG_INT_ARRAY = 11
pattern TAG_LONG_ARRAY = 12
pattern TAG_OPEN_LIST = 9
pattern TAG_CLOSE_LIST = 13
pattern TAG_OPEN_COMPOUND = 10
pattern TAG_CLOSE_COMPOUND = 14

foreign import ccall unsafe "parseIntoTape" c_parseIntoTape :: Ptr Word8 -> CSize -> CInt -> Ptr Word8 -> IO CInt
