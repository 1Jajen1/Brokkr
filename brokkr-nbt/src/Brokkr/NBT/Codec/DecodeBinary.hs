{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module Brokkr.NBT.Codec.DecodeBinary (
  genParser
) where

import Brokkr.NBT.Internal
import Brokkr.NBT.ByteOrder
import Brokkr.NBT.NBTError

import Brokkr.NBT.NBTString.Internal

import Brokkr.NBT.Codec.Internal

import Control.Monad

import Data.Bifunctor (first)

import Data.Bits

import Data.ByteString          qualified as BS
import Data.ByteString.Internal qualified as BS

import Data.Foldable
import Data.List (sortOn, groupBy, deleteBy)
import Data.Maybe

import Data.Text (Text)

import Data.Vector.Storable qualified as S

import Data.Primitive.SmallArray

import FlatParse.Basic qualified as FP

import GHC.Exts
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.ForeignPtr
import GHC.Int
import GHC.Word

import Language.Haskell.TH qualified as TH

-- | Generate a parser which reads binary 'NBT'
genParser :: forall i0 o st0 . NBTCodec Value i0 o -> Code (FP.ParserT st0 NBTError o)
genParser c0 = [|| FP.anyWord8 >>= \t -> parseNBTString >> $$(go [] c0) t ||]
  where
    go :: forall i x st . [Text] -> NBTCodec Value i x -> Code (Word8 -> FP.ParserT st NBTError x)
    go _ TagCodec = [|| parseTag ||]

    go path (ByteCodec  name) = [|| \t -> if t == 1 then FP.anyInt8    else FP.err $ invalidType path "byte"  name ||]
    go path (ShortCodec name) = [|| \t -> if t == 2 then FP.anyInt16be else FP.err $ invalidType path "short" name ||]
    go path (IntCodec   name) = [|| \t -> if t == 3 then FP.anyInt32be else FP.err $ invalidType path "int"   name ||]
    go path (LongCodec  name) = [|| \t -> if t == 4 then FP.anyInt64be else FP.err $ invalidType path "long"  name ||]

    go path (FloatCodec  name) = [|| \t -> if t == 5 then castWord32ToFloat  <$> FP.anyWord32be else FP.err $ invalidType path "float"  name ||]
    go path (DoubleCodec name) = [|| \t -> if t == 6 then castWord64ToDouble <$> FP.anyWord64be else FP.err $ invalidType path "double" name ||]

    go path (ByteArrayCodec name) = [|| \t -> if t == 7  then takeArray @Int8    else FP.err $ invalidType path "byte array" name ||]
    go path (IntArrayCodec name)  = [|| \t -> if t == 11 then takeArray @Int32BE else FP.err $ invalidType path "int array"  name ||]
    go path (LongArrayCodec name) = [|| \t -> if t == 12 then takeArray @Int64BE else FP.err $ invalidType path "long array" name ||]

    go path (StringCodec name) = [|| \t -> if t == 8 then parseNBTString else FP.err $ invalidType path "string" name ||]

    go path (ListCodec name inner) = [|| \t -> if t == 9
      then do
        innerT <- FP.anyWord8
        len@(I# len#) <- fromIntegral <$> FP.anyInt32be
        localST $ do
          -- TODO Check against stupidly large lists
          SmallMutableArray mut <- FP.liftST $ newSmallArray len (error "parse nbt list")

          let goList   _ 0# = pure ()
              goList i n  = do
                el <- $$(go ("<list" <> maybe "" ("#"<>) name <> ">" : path) inner) innerT
                FP.liftST $ writeSmallArray (SmallMutableArray mut) (I# i) el
                goList (i +# 1#) (n -# 1#)

          goList 0# len#
          FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut)
      else FP.err $ invalidType path "list" name ||]
    
    go path (CompoundCodec name inner) = [|| \t -> if t == 10
      then $$(goCompound ("<compound" <> maybe "" ("#"<>) name <> ">" : path) inner)
      else FP.err $ invalidType path "compound" name
      ||]
    
    go path (RmapCodec f inner) = [|| fmap $$(f) . $$(go path inner) ||]

    go path (RmapEitherCodec f inner) = [|| $$(go path inner) >=> (\case
      Left e -> FP.err e
      Right a -> pure a) . $$(f)
      ||]

    go path (LmapCodec _ inner) = go path inner

    goCompound :: forall i1 x0 st . [Text] -> NBTCodec Compound i1 x0 -> Code (FP.ParserT st NBTError x0)
    goCompound path inner'
      | PureCodec a <- inner = [|| skipCompound >> pure $$(a) ||]
      | [] <- keysToParsers = error "Codec for a compound that can never succeed" -- TODO Show codec (needs prettyprinter dep to look decent)
      -- This generates slightly bad code and isn't any more efficient than the trie default

      -- | [(k,def,cont)] <- keysToParsers = [||
      --   let goRec = do
      --         t <- FP.anyWord8
      --         if t == 0
      --           then $$(maybe [|| FP.empty ||] (\x -> TH.unsafeCodeCoerce [| pure $ $(topLevelF) $(x) |]) def)
      --           else withNBTString $ \k' ->
      --             if k == k' then
      --               $$(TH.unsafeCodeCoerce $ cont [| pure . $(topLevelF) |]) t >>= \x -> skipCompound >> pure x
      --             else
      --               skipTag t >> goRec
      --     in goRec
      --   ||]

      -- How does this work?
      -- We generate code such as this:
      --
      -- let go arg1 ... argN bitMap
      --     go arg1 ... argN 0 = topLevel arg1 ... argN
      --     go arg1 ... argN bm = {...}
      -- in go def1 ... defN initBitMap
      --
      -- The initial bitmap has a bit set for each key that we need to parse.
      -- The loop now iterates each key value pair in the compound. If it finds a key
      --  that we need, it replaces the default value and unsets the bit in the bitmap.
      -- If an unknown key is encountered, it and its value are validated but skipped.
      -- The default value for required keys is an error closure and at some point
      --  primitives will get a different default
      --
      -- Key matching is implemented with a trie. Single runs in a trie are compressed to wide
      --  matches (word64-word16) until we need to match single bytes. This way we take only
      --  large strides towards keys. The length is always checked before a key is tested and
      --  only same length keys end up in a trie. There is also a check if we have enough bytes
      --  for the key at the very beginning, even before we check which length we have, so that
      --  invalid nbt fails fast. This also means we can ignore length checks from then on.
      | otherwise = TH.unsafeCodeCoerce $ do
          funcN <- TH.newName "go"
          bm <- TH.newName "bm"
          -- [(strict?,name)]
          -- If a value is strict, it will get a bang pattern and ghc is likely to unbox it
          varNames' <- traverse (\(_,_,mdef,_) -> (isJust mdef,) <$> TH.newName "x") keysToParsers
          let varNames = varNames' ++ [(True, bm)]
              initBM :: Int = (1 `unsafeShiftL` length keysToParsers) - 1
              -- 0 for all keys that are optional, 1 for all others
              reqBM :: Int = foldr (\(n, (_,opt,_,_)) b -> if opt then b else setBit b n) 0 $ zip [0..] keysToParsers
              -- TODO For trie matching:
              --   When we have
              -- a -> b -> c -> (d,e) don't generate
              -- abc -> (d | e)
              -- but instead generate
              -- abcd | abce
              -- Has one less branch, and one less memory read
              -- 
              -- Maybe even go for max size reads all the time and let ghc create
              -- the lookup then? (Actually the below is impossible because I have a eq size guarantee)
              -- a -> (b -> d | c -> e | f -> j -> l)
              -- abd | ace | (afj -> l) 
              genBranch validKey tagId trie0 =
                let go64 [] cont = cont
                    go64 (x:xs) cont
                      = [| withAnyWord64Unsafe $ \w ->
                          if w == x then $(go64 xs cont)
                          else $(validKey)
                          |]
                    goTail [] cont = cont
                    goTail (a:b:c:d:e:f:g:h:xs) cont
                      = [| withAnyWord64Unsafe $ \w ->
                          if w == w64 then $(goTail xs cont)
                          else $(validKey)
                          |]
                      where w64 :: Word64 = foldr (\x acc -> fromIntegral x .|. (acc `unsafeShiftL` 8)) 0 [a,b,c,d,e,f,g,h]
                    goTail (a:b:c:d:xs) cont
                      = [| withAnyWord32Unsafe $ \w ->
                          if w == w32 then $(goTail xs cont)
                          else $(validKey)
                          |]
                      where w32 :: Word32 = foldr (\x acc -> fromIntegral x .|. (acc `unsafeShiftL` 8)) 0 [a,b,c,d]
                    goTail (a:b:xs) cont
                      = [| withAnyWord16Unsafe $ \w ->
                          if w == w16 then $(goTail xs cont)
                          else $(validKey)
                          |]
                      where w16 :: Word16 = foldr (\x acc -> fromIntegral x .|. (acc `unsafeShiftL` 8)) 0 [a,b]
                    goTail (a:xs) cont
                      = [| withAnyWord8Unsafe $ \w ->
                          if w == a then $(goTail xs cont)
                          else $(validKey)
                          |]
                    goTrie trie =
                      let (pre,post) = extractPrefix trie
                          (pre64, preTail) = chunked pre
                      in go64 pre64 [|
                         $(case post of
                           -- These two are filtered out by extractPrefix
                           Empty   -> error "Empty"
                           All _ _ -> error "All"
                           -- A trie with a single element only
                           Leaf (ind,(_,_,_,withCont)) -> goTail preTail [| $(withCont [|
                             \el -> $(TH.appE (snd $ foldl' (\(i, acc) x ->
                               if i == ind then
                                 (i + 1, TH.appE acc [| el |])
                               else (i + 1, TH.appE acc (TH.varE x))) (0, TH.varE funcN) $ fmap snd varNames') [| clearBit @Int $(TH.varE bm) ind |])
                               |]) $(tagId) |]
                           -- Or a branch...
                           Branch branches ->
                             let (preTail', scrutF, branches') = case preTail of
                                   [a] ->             ([], [| withAnyWord16Unsafe |], (\(w, t) -> ([a,w],t)) <$> branches)
                                   [a,b,c] ->         ([], [| withAnyWord32Unsafe |], (\(w, t) -> ([a,b,c,w],t)) <$> branches)
                                   [a,b,c,d,e,f,g] -> ([], [| withAnyWord64Unsafe |], (\(w, t) -> ([a,b,c,d,e,f,g,w],t)) <$> branches)
                                   _ ->               (preTail, [| withAnyWord8Unsafe |], (\(w,t) -> ([w],t)) <$> branches)
                             in goTail preTail' [|
                               $(scrutF) $ \scrut ->
                                 $(let mkMatch bs t = TH.match
                                         (TH.litP $ TH.IntegerL $ foldr (\a acc -> fromIntegral a .|. (acc `unsafeShiftL` 8)) 0 bs)
                                         (TH.normalB $ goTrie t) []
                                       fb = TH.match TH.wildP (TH.normalB [| $(validKey) |]) []
                                   in TH.caseE [| scrut |] $ foldr (\(bs, t) acc -> mkMatch bs t : acc) [fb] branches')
                               |])
                         |]
                  in goTrie trie0
          TH.letE [
            let bod = [|
                  FP.withAnyWord8 $ \tag -> do
                    if tag == 0
                      then if ($(TH.varE bm) .&. reqBM) /= 0 then
                          -- we are still missing some required keys
                          -- TODO Map the bitmap back to the keys
                          let toMissing x = catMaybes $ $(foldr (\(n, (bs,opt,_,_)) acc ->
                                if opt
                                  then acc
                                  else let t = toText bs in [| (if x .&. (1 `unsafeShiftL` n) /= 0 then Just t else Nothing) : $acc |] 
                                ) [| [] |] $ zip ([0..] :: [Int]) keysToParsers)
                          in FP.err $ missingKey path (toMissing $(TH.varE bm))
                        else
                          -- we are done, the only missing keys were optional
                          $(TH.unTypeCode . fst $ mkFinishFun inner (fmap snd varNames'))
                      else FP.withAnyWord16 $ \w -> do
                        let w' = fromIntegral $ byteSwap16 w
                        FP.ensure w'
                        -- Skipping back to the state as of this moment is equal to skipping back x amount of bytes,
                        -- but doing it this way results in far less code and equal performance
                        Ptr addr <- FP.ParserT $ \_ _ s st -> (# st, (# (# Ptr s, s #) | | #) #)
                        let validateKey = do
                              skipKeyWithSizeWithBack w' addr $ do
                                skipTag tag
                                $(foldl' (\acc x -> TH.appE acc (TH.varE x)) (TH.varE funcN) $ fmap snd varNames)
                        $(let fb = TH.match TH.wildP (TH.normalB [| validateKey |]) []
                              buildMatch (sz, trie) = TH.match (TH.litP $ TH.IntegerL (fromIntegral sz)) (TH.normalB $ genBranch [| validateKey |] [| tag |] trie) []
                          in TH.caseE [| w' |] $ foldr (\x acc -> buildMatch x : acc) [fb] tries)
                  |]
                doneBod = [| skipCompound >> $(TH.unTypeCode . fst $ mkFinishFun inner (fmap snd varNames')) |]
            in TH.funD funcN
                [ TH.clause (((\(bang,n) -> (if bang then TH.bangP else id) $ TH.varP n) <$> varNames') ++ [TH.litP $ TH.IntegerL 0]) (TH.normalB doneBod) []
                , TH.clause ( (\(bang,n) -> (if bang then TH.bangP else id) $ TH.varP n) <$> varNames) (TH.normalB bod) []
                ]
            ] $ TH.appE (foldl' (\acc (_,_,def,_) -> TH.appE acc $ fromMaybe [| error "def" |] def) (TH.varE funcN) keysToParsers) [| initBM |]
      where
        inner = simplifyCodec inner'

        -- [key,default,cont -> parser]
        keysToParsers = collectKeys inner

        collectKeys :: forall i x . NBTCodec Compound i x -> [(NBTString, Bool, Maybe (TH.Q TH.Exp), TH.Q TH.Exp -> TH.Q TH.Exp)]
        collectKeys (PureCodec _) = []
        collectKeys (RmapCodec _ i) = collectKeys i
        collectKeys (RmapEitherCodec _ i)
                                    = collectKeys i
        collectKeys (LmapCodec _ i) = collectKeys i
        collectKeys (RequiredKeyCodec key i _descr) =
          [(key, False, defValue i, \cont -> [| $(TH.unTypeCode $ go (toText key : path) i) >=> $(cont) |])]
        collectKeys (OptionalKeyCodec key i _descr) =
          [(key, True, Just [| Nothing |], \cont -> [|
            \t -> do
              let onMatch = $(TH.unTypeCode $ go (toText key : path) i) t >>= $(cont) . Just
                  -- onFail = skipTag t >> pure Nothing
              -- TODO Decide what to do here:
              -- Is the entire inner codec optional or just the key?
              -- Can the inner codec fail and still be considered valid
              -- because it is optional?
              onMatch -- FP.<|> onFail
              |])]
        collectKeys (ApCodec ff fa) = collectKeys ff <> collectKeys fa

        defValue :: forall i o0 . NBTCodec Value i o0 -> Maybe (TH.Q TH.Exp)
        -- We can only default things that we can explicitly type, otherwise ghc
        -- cannot infer the types when coerce is used 
        defValue ByteCodec{}   = Just [| 0 :: Int8   |]
        defValue ShortCodec{}  = Just [| 0 :: Int16  |]
        defValue IntCodec{}    = Just [| 0 :: Int32  |]
        defValue LongCodec{}   = Just [| 0 :: Int64  |]
        defValue FloatCodec{}  = Just [| 0 :: Float  |]
        defValue DoubleCodec{} = Just [| 0 :: Double |]
        defValue StringCodec{} = Just [| NBTString mempty |]
        defValue ByteArrayCodec{} = Just [| mempty :: S.Vector Int8    |]
        defValue IntArrayCodec{}  = Just [| mempty :: S.Vector Int32BE |]
        defValue LongArrayCodec{} = Just [| mempty :: S.Vector Int64BE |]

        defValue TagCodec        = Nothing
        defValue ListCodec{}     = Nothing
        defValue CompoundCodec{} = Nothing

        defValue (RmapCodec f i)       = (\e -> [| $(TH.unTypeCode f) $e |]) <$> defValue i
        -- TODO This is a bit unfortunate
        defValue (RmapEitherCodec _ _) = Nothing

        defValue (LmapCodec _ i) = defValue i

        -- | Invariant. The argument names are passed in the order they are defined in in the codec
        mkFinishFun :: forall i o0 . NBTCodec Compound i o0 -> [TH.Name] -> (Code (FP.ParserT st NBTError o0), [TH.Name])
        mkFinishFun (PureCodec a) args = ([|| pure $$a ||], args)
        mkFinishFun (ApCodec ff fa) args =
          let (eFf, args' ) = mkFinishFun ff args
              (eFa, args'') = mkFinishFun fa args'
          in ([|| $$eFf <*> $$eFa ||], args'')
        mkFinishFun (RmapCodec f i) args =
          let (eFi, args') = mkFinishFun i args
          in ([|| $$f <$> $$eFi ||], args')
        mkFinishFun (RmapEitherCodec f i) args =
          let (eFi, args') = mkFinishFun i args
          in ([|| $$eFi >>= (\case
              Left e  -> FP.err e
              Right a -> pure a
            ) . $$f
            ||], args')
        mkFinishFun (LmapCodec _ i) args = mkFinishFun i args
        mkFinishFun RequiredKeyCodec{} [] = error "Finish fun had no further arguments!"
        mkFinishFun (RequiredKeyCodec _ _i _) (x:xs) =
          ([|| pure $$(TH.unsafeCodeCoerce (TH.varE x)) ||], xs)
          -- ([|| $$(TH.unsafeCodeCoerce $ mkValFun i) $$(TH.unsafeCodeCoerce (TH.varE x)) ||], xs)
        mkFinishFun OptionalKeyCodec{} [] = error "Finish fun had no further arguments!"
        mkFinishFun (OptionalKeyCodec _ _i _) (x:xs) =
          ([|| pure $$(TH.unsafeCodeCoerce (TH.varE x)) ||], xs)
          -- ([|| case $$(TH.unsafeCodeCoerce (TH.varE x)) of
          --   Nothing -> pure Nothing
          --   Just x -> Just <$> $$(TH.unsafeCodeCoerce $ mkValFun i) x
          --   ||], xs)

        mkValFun :: NBTCodec Value i o0 -> TH.Q TH.Exp
        mkValFun (LmapCodec _ i) = mkValFun i
        mkValFun (RmapCodec _ i) = mkValFun i
        mkValFun (RmapEitherCodec f i) = [| $(mkValFun i) >=> (\case
          Left e -> FP.err e
          Right a -> pure a) . $(TH.unTypeCode f)
          |]
        mkValFun _ = [| pure |]

        tries = buildTries (\(key,_,_,_) -> coerce key) keysToParsers

        buildTries :: (b -> BS.ByteString) -> [b] -> [(Int, Trie (Int, b))]
        buildTries on xs0 = fmap (\xs -> (depth xs, goBuildTrie xs Empty)) grouped
          where
            expandedBytes = (\(i, b) -> (BS.unpack $ on b, (i, b))) <$> zip [0..] xs0
            grouped = groupBy (\(k1,_) (k2,_) -> length k1 == length k2) $ sortOn (\(k,_) -> length k) expandedBytes
            depth [] = 0
            depth ((bs,_):_) = length bs
            goBuildTrie [] acc = acc
            goBuildTrie ((bs,b):xs) acc = insert bs b $ goBuildTrie xs acc
            insert [] v = \case
              Empty -> Leaf v
              _ -> error "Two elements have the exact same key" -- TODO Add more debug information
            insert (x:xs) v = \case
              Empty -> All x (insert xs v Empty)
              All y t
                | y == x    -> All x (insert xs v t)
                | otherwise -> Branch [(y,t),(x, insert xs v Empty)]
              Branch ys
                | Just e@(_,t) <- find (\(k,_) -> k == x) ys -> Branch $ (x,insert xs v t) : deleteBy (\a b -> fst a == fst b) e ys
                | otherwise -> Branch $ (x,insert xs v Empty) : ys
              Leaf _ -> error "Two elements have the exact same key."
        
        extractPrefix :: Trie a -> ([Word8], Trie a)
        extractPrefix (All x t) = first (x :) $ extractPrefix t
        extractPrefix b@Branch{} = ([],b)
        extractPrefix l@Leaf{} = ([], l)
        extractPrefix Empty = ([], Empty)

        chunked :: [Word8] -> ([Word64], [Word8])
        chunked (a:b:c:d:e:f:g:h:xs) =
          let (ys, preTail) = chunked xs
          in (foldr (\x acc -> fromIntegral x .|. (acc `unsafeShiftL` 8)) 0 [a,b,c,d,e,f,g,h] : ys, preTail)
        chunked xs = ([],xs)

data Trie a = All !Word8 !(Trie a) | Branch [(Word8, Trie a)] | Leaf a | Empty
  deriving stock (Show, Functor)

skipCompound :: FP.ParserT st NBTError ()
{-# INLINE skipCompound #-}
skipCompound = do
  t <- FP.anyWord8
  if t == 0 then pure () else skipKey >> skipTag t >> skipCompound

skipKey :: FP.ParserT st NBTError ()
{-# INLINE skipKey #-}
skipKey = withNBTString $ \_ -> pure ()

-- Invariant: We have at least sz bytes available
-- Invariant: addr points to right after the size prefix
skipKeyWithSizeWithBack :: Int -> Addr# -> FP.ParserT st e r -> FP.ParserT st e r
{-# INLINE skipKeyWithSizeWithBack #-}
skipKeyWithSizeWithBack (I# sz) addr cont
  = FP.ParserT (\fp eob _ st ->
    case takeExtraUnsafe# sz >>= \bs ->
      if isValidModifiedUtf8 bs then cont
      else FP.empty of
        FP.ParserT g -> g fp eob addr st
    )

-- | takeUnsafe# skips both the check that the size is non-negative and
-- the check that we have enough bytes remaining
takeExtraUnsafe# :: Int# -> FP.ParserT st e BS.ByteString
{-# INLINE takeExtraUnsafe# #-}
takeExtraUnsafe# sz = FP.ParserT $ \fp _ s st ->
  FP.OK# st (BS.BS (ForeignPtr s fp) (I# sz)) (plusAddr# s sz)

-- TODO I need to perform a heap check here to avoid hogging a thread.
-- This is 100% allocation free and thus very dangerous when applied to
-- user input, especially if that input came compressed.
skipTag :: Word8 -> FP.ParserT st NBTError ()
{-# INLINE skipTag #-}
-- int8, int16, int32, int64, float, double
skipTag 1 = FP.skip 1
skipTag 2 = FP.skip 2
skipTag 3 = FP.skip 4
skipTag 4 = FP.skip 8
skipTag 5 = FP.skip 4
skipTag 6 = FP.skip 8
-- byte/int/long array
skipTag 7  = FP.anyInt32be >>= void . FP.skip . fromIntegral
skipTag 11 = FP.anyInt32be >>= \sz -> void . FP.skip $ fromIntegral sz * 4
skipTag 12 = FP.anyInt32be >>= \sz -> void . FP.skip $ fromIntegral sz * 8
-- strings
skipTag 8 = skipKey
-- lists
skipTag 9 = do
  t <- FP.anyWord8
  sz <- FP.anyInt32be
  let go  0 = pure ()
      go !n = skipTag t >> go (n - 1)
  go sz
-- compounds
skipTag 10 = skipCompound
-- anything else is bad
skipTag _ = FP.empty

-- TODO remember to yield on skipTag calls to prevent exploits

localST :: forall st e a . (forall s . FP.ParserST s e a) -> FP.ParserT st e a
{-# INLINE localST #-}
localST p = FP.ParserT $ \fp end curr st -> case runRW# (FP.runParserT# p fp end curr) of
  (# _, res #) -> (# st, res #)

withAnyWord64Unsafe :: (Word64 -> FP.ParserT st e a) -> FP.ParserT st e a
{-# INLINE withAnyWord64Unsafe #-}
withAnyWord64Unsafe = FP.withAnySizedUnsafe# 8# (\addr i -> W64# (indexWord64OffAddr# addr i))

withAnyWord32Unsafe :: (Word32 -> FP.ParserT st e a) -> FP.ParserT st e a
{-# INLINE withAnyWord32Unsafe #-}
withAnyWord32Unsafe = FP.withAnySizedUnsafe# 4# (\addr i -> W32# (indexWord32OffAddr# addr i))

withAnyWord16Unsafe :: (Word16 -> FP.ParserT st e a) -> FP.ParserT st e a
{-# INLINE withAnyWord16Unsafe #-}
withAnyWord16Unsafe = FP.withAnySizedUnsafe# 2# (\addr i -> W16# (indexWord16OffAddr# addr i))

withAnyWord8Unsafe :: (Word8 -> FP.ParserT st e a) -> FP.ParserT st e a
{-# INLINE withAnyWord8Unsafe #-}
withAnyWord8Unsafe = FP.withAnySizedUnsafe# 1# (\addr i -> W8# (indexWord8OffAddr# addr i))
