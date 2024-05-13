{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
-- {-# OPTIONS_GHC -dsuppress-all -ddump-simpl #-}
module Brokkr.NBT.Codec.DecodeBinary (
  genParser
) where

import Brokkr.NBT.Internal hiding (Compound)
import Brokkr.NBT.ByteOrder
import Brokkr.NBT.NBTError
import Brokkr.NBT.Validate

import Brokkr.NBT.NBTString.Internal

import Brokkr.NBT.Codec.Internal

import Control.Monad

import Data.Bits

import Data.ByteString          qualified as BS
import Data.ByteString.Internal qualified as BS

import Data.Foldable
import Data.List (sortOn, groupBy, deleteBy)
import Data.Maybe

import Data.Text (Text)

import Data.Vector.Storable qualified as S
import Foreign.Storable qualified as S

import Data.Primitive.SmallArray

import FlatParse.Basic qualified as FP

import GHC.Exts
import GHC.Float (castWord32ToFloat, castWord64ToDouble)
import GHC.ForeignPtr
import GHC.Int
import GHC.Word

import Language.Haskell.TH qualified as TH

newtype ContParserT st e a = ContParserT { runContParserT :: forall r . (a -> FP.ParserT st e r) -> FP.ParserT st e r }
instance Functor (ContParserT st e) where
  fmap f (ContParserT g) = ContParserT $ \h -> g (h . f)

-- | Generate a parser which reads binary 'NBT'
genParser :: forall i0 o st0 . NBTCodec Value i0 o -> Code (FP.ParserT st0 NBTError o)
genParser c0 = [|| FP.anyWord8 >>= \t -> parseNBTString >> runContParserT ($$(go [] c0) t) pure ||]
  where
    go :: forall i x st . [Text] -> NBTCodec Value i x -> Code (Word8 -> ContParserT st NBTError x)
    go _ TagCodec = [|| \t -> ContParserT $ \f -> parseTag t >>= f ||]

    go path (ByteCodec  name) = [|| \t -> ContParserT $ \f -> if t == 1 then FP.anyInt8    >>= f else FP.err $ invalidType path "byte"  name ||]
    go path (ShortCodec name) = [|| \t -> ContParserT $ \f -> if t == 2 then FP.anyInt16be >>= f else FP.err $ invalidType path "short" name ||]
    go path (IntCodec   name) = [|| \t -> ContParserT $ \f -> if t == 3 then FP.anyInt32be >>= f else FP.err $ invalidType path "int"   name ||]
    go path (LongCodec  name) = [|| \t -> ContParserT $ \f -> if t == 4 then FP.anyInt64be >>= f else FP.err $ invalidType path "long"  name ||]

    go path (FloatCodec  name) = [|| \t -> ContParserT $ \f -> if t == 5 then castWord32ToFloat  <$> FP.anyWord32be >>= f else FP.err $ invalidType path "float"  name ||]
    go path (DoubleCodec name) = [|| \t -> ContParserT $ \f -> if t == 6 then castWord64ToDouble <$> FP.anyWord64be >>= f else FP.err $ invalidType path "double" name ||]

    go path (ByteArrayCodec name) = [|| \t -> ContParserT $ \f -> if t == 7  then withArray @Int8              f else FP.err $ invalidType path "byte array" name ||]
    go path (IntArrayCodec name)  = [|| \t -> ContParserT $ \f -> if t == 11 then withArray @(BigEndian Int32) f else FP.err $ invalidType path "int array"  name ||]
    go path (LongArrayCodec name) = [|| \t -> ContParserT $ \f -> if t == 12 then withArray @(BigEndian Int64) f else FP.err $ invalidType path "long array" name ||]

    go path (StringCodec name) = [|| \t -> ContParserT $ \f -> if t == 8 then withNBTString f else FP.err $ invalidType path "string" name ||]

    go path (ListCodec name inner) = [|| \t -> if t == 9
      then $$(goList ("<list" <> maybe "" ("#"<>) name <> ">" : path) inner)
      else ContParserT $ \_ -> FP.err $ invalidType path "list" name ||]
    
    go path (CompoundCodec name inner) = [|| \t -> ContParserT $ \f -> if t == 10
      then $$(goCompound ("<compound" <> maybe "" ("#"<>) name <> ">" : path) inner) >>= f
      else FP.err $ invalidType path "compound" name
      ||]

    go path (RmapCodec f inner) = [|| fmap $$(f) . $$(go path inner) ||]

    go path (RmapEitherCodec f inner) = [|| \t -> ContParserT $ \fi -> runContParserT ($$(go path inner) t) $ \x ->
      case $$(f) x of
        Left e -> FP.err e
        Right a -> fi a
      ||]

    go path (LmapCodec _ inner) = go path inner

    -- This wasn't worth it for the intermediate structure, but here it is, likely because of code size
    goList :: forall i1 x0 st . [Text] -> NBTCodec Value i1 x0 -> Code (ContParserT st NBTError (ListFor x0))
    goList path inner = [|| ContParserT $ \f -> do
      listTag <- FP.anyWord8
      FP.withAnyWord32 $ \w -> do
        let len = fromIntegral $ byteSwap32 w
        if len == 0
          then f $$(goEmpty inner)
          else runContParserT ($$(goInner inner) len listTag) f
      ||]
      where
        goEmpty :: NBTCodec Value i2 x0 -> Code (ListFor x0)
        goEmpty (ByteCodec _) = [|| S.empty ||]
        goEmpty (ShortCodec _) = [|| S.empty ||]
        goEmpty (IntCodec _) = [|| S.empty ||]
        goEmpty (LongCodec _) = [|| S.empty ||]
        goEmpty (FloatCodec _) = [|| S.empty ||]
        goEmpty (DoubleCodec _) = [|| S.empty ||]
        goEmpty (RmapCodec _ _) = error "ListCodec rmap inner"
        goEmpty (RmapEitherCodec _ _) = error "ListCodec rmap either inner"
        goEmpty (LmapCodec _ i) = goEmpty i
        goEmpty TagCodec = [|| emptySmallArray ||]
        goEmpty (ByteArrayCodec _) = [|| emptySmallArray ||]
        goEmpty (IntArrayCodec _) = [|| emptySmallArray ||]
        goEmpty (LongArrayCodec _) = [|| emptySmallArray ||]
        goEmpty (StringCodec _) = [|| emptySmallArray ||]
        goEmpty (ListCodec _ _) = [|| emptySmallArray ||]
        goEmpty (CompoundCodec _ _) = [|| emptySmallArray ||]

        goInner :: NBTCodec Value i2 x1 -> Code (Int -> Word8 -> ContParserT st NBTError (ListFor x1))
        goInner (ByteCodec name) = [|| \sz -> \case
          1 -> ContParserT $ \f -> parseArray @Int8 sz f
          _ -> ContParserT $ \_ -> FP.err $ invalidType path "byte" name
          ||]
        goInner (ShortCodec name) = [|| \sz -> \case
          2 -> ContParserT $ \f -> parseArray @(BigEndian Int16) sz f
          _ -> ContParserT $ \_ -> FP.err $ invalidType path "short" name
          ||]
        goInner (IntCodec name) = [|| \sz -> \case
          3 -> ContParserT $ \f -> parseArray @(BigEndian Int32) sz f
          _ -> ContParserT $ \_ -> FP.err $ invalidType path "int" name
          ||]
        goInner (LongCodec name) = [|| \sz -> \case
          4 -> ContParserT $ \f -> parseArray @(BigEndian Int64) sz f
          _ -> ContParserT $ \_ -> FP.err $ invalidType path "long" name
          ||]
        goInner (FloatCodec name) = [|| \sz -> \case
          5 -> ContParserT $ \f -> parseArray @(BigEndian Float) sz f
          _ -> ContParserT $ \_ -> FP.err $ invalidType path "float" name
          ||]
        goInner (DoubleCodec name) = [|| \sz -> \case
          6 -> ContParserT $ \f -> parseArray @(BigEndian Double) sz f
          _ -> ContParserT $ \_ -> FP.err $ invalidType path "double" name
          ||]
        goInner (ByteArrayCodec name) = [|| \sz -> \case
          7 -> ContParserT $ \f -> $$(parseSmallArray [|| takeArray @Int8 ||]) sz >>= f
          _ -> ContParserT $ \_ -> FP.err $ invalidType path "byte array" name
          ||]
        goInner (IntArrayCodec name) = [|| \sz -> \case
          11 -> ContParserT $ \f -> ($$(parseSmallArray [|| takeArray @(BigEndian Int32) ||]) sz) >>= f
          _  -> ContParserT $ \_ -> FP.err $ invalidType path "int array" name
          ||]
        goInner (LongArrayCodec name) = [|| \sz -> \case
          12 -> ContParserT $ \f -> ($$(parseSmallArray [|| takeArray @(BigEndian Int64) ||]) sz) >>= f
          _  -> ContParserT $ \_ -> FP.err $ invalidType path "long array" name
          ||]
        goInner (StringCodec name) = [|| \sz -> \case
          8 -> ContParserT $ \f -> ($$(parseSmallArray [|| parseNBTString ||]) sz) >>= f
          _  -> ContParserT $ \_ -> FP.err $ invalidType path "string" name
          ||]
        goInner TagCodec = [|| \sz t -> ContParserT $ \f -> ($$(parseSmallArray [|| parseTag t ||]) sz) >>= f ||]
        goInner (ListCodec name i) = [|| \sz -> \case
          9  -> ContParserT $ \f -> ($$(parseSmallArray [|| runContParserT ($$(goList ("<list" <> maybe "" ("#"<>) name <> ">" : path) i)) pure ||]) sz) >>= f
          _  -> ContParserT $ \_ -> FP.err $ invalidType path "list" name
          ||]
        goInner (CompoundCodec name i) = [|| \sz -> \case
          10 -> ContParserT $ \f -> ($$(parseSmallArray (goCompound ("<compound" <> maybe "" ("#"<>) name <> ">" : path) i)) sz) >>= f
          _  -> ContParserT $ \_ -> FP.err $ invalidType path "list" name
          ||]
        goInner (RmapCodec _ _) = error "ListCodec rmap inner"
        goInner (RmapEitherCodec _ _) = error "ListCodec rmap either inner"
        goInner (LmapCodec _ i) = goInner i

        parseSmallArray (inner :: forall s . Code (FP.ParserT s NBTError a)) = [|| \sz@(I# sz#) -> do
          localST $ do
            -- TODO Check against stupidly large lists
            SmallMutableArray mut <- FP.liftST $ newSmallArray sz (error "parse nbt list")

            let goListI i
                  | isTrue# (i >=# sz#) = pure ()
                  | otherwise = do
                      el <- $$(inner)
                      FP.liftST $ writeSmallArray (SmallMutableArray mut) (I# i) el
                      goListI (i +# 1#)

            goListI 0#
            FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut)
          ||]

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
              genBranch validKey tagId d0 trie0 = go d0 trie0
                where
                  go d t
                    | d >= 8
                    , matches <- extractWord64Match t
                      = [| withAnyWord64Unsafe $ \w -> $(TH.caseE [| w |] $ (do
                            (w64, t') <- matches
                            pure $ TH.match
                              (TH.litP $ TH.IntegerL (fromIntegral w64))
                              (TH.normalB $ go (d - 8) t')
                              []) <> [TH.match TH.wildP (TH.normalB validKey) []]
                          ) |]
                    | d >= 4
                    , matches <- extractWord32Match t
                      = [| withAnyWord32Unsafe $ \w -> $(TH.caseE [| w |] $ (do
                            (w32, t') <- matches
                            pure $ TH.match
                              (TH.litP $ TH.IntegerL (fromIntegral w32))
                              (TH.normalB $ go (d - 4) t')
                              []) <> [TH.match TH.wildP (TH.normalB validKey) []]
                          ) |]
                    | d >= 2
                    , matches <- extractWord16Match t
                      = [| withAnyWord16Unsafe $ \w -> $(TH.caseE [| w |] $ (do
                            (w16, t') <- matches
                            pure $ TH.match
                              (TH.litP $ TH.IntegerL (fromIntegral w16))
                              (TH.normalB $ go (d - 2) t')
                              []) <> [TH.match TH.wildP (TH.normalB validKey) []]
                          ) |]
                    | d >= 1
                    , matches <- extractWord8Match t
                      = [| withAnyWord8Unsafe $ \w -> $(TH.caseE [| w |] $ (do
                            (w8, t') <- matches
                            pure $ TH.match
                              (TH.litP $ TH.IntegerL (fromIntegral w8))
                              (TH.normalB $ go (d - 1) t')
                              []) <> [TH.match TH.wildP (TH.normalB validKey) []]
                          ) |]
                    | d == 0 = case t of
                      Leaf (ind, (_,_,_, withCont)) -> [| $(withCont [|
                          \el -> $(TH.appE (snd $ foldl' (\(i, acc) x ->
                              if i == ind then
                                (i + 1, TH.appE acc [| el |])
                              else (i + 1, TH.appE acc (TH.varE x))) (0, TH.varE funcN) $ fmap snd varNames') [| clearBit @Int $(TH.varE bm) ind |])
                        |]) $tagId |]
                      _ -> error "Weird ass trie!"
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
                              buildMatch (sz, trie) = TH.match (TH.litP $ TH.IntegerL (fromIntegral sz)) (TH.normalB $ genBranch [| validateKey |] [| tag |] sz trie) []
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
          [(key, False, defValue i, \cont -> [| \t -> runContParserT ($(TH.unTypeCode $ go (toText key : path) i) t) $(cont) |])]
        collectKeys (OptionalKeyCodec key i _descr) =
          [(key, True, Just [| Nothing |], \cont -> [|
            \t -> do
              let onMatch = runContParserT ($(TH.unTypeCode $ go (toText key : path) i) t) ($(cont) . Just)
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
        defValue IntArrayCodec{}  = Just [| mempty :: S.Vector (BigEndian Int32) |]
        defValue LongArrayCodec{} = Just [| mempty :: S.Vector (BigEndian Int64) |]

        defValue TagCodec        = Nothing
        defValue (ListCodec _ i) = case i of
          ByteCodec{}  -> Just [| mempty :: S.Vector Int8 |]
          ShortCodec{} -> Just [| mempty :: S.Vector (BigEndian Int16) |]
          IntCodec{}   -> Just [| mempty :: S.Vector (BigEndian Int32) |]
          LongCodec{}  -> Just [| mempty :: S.Vector (BigEndian Int64) |]
          FloatCodec{}  -> Just [| mempty :: S.Vector (BigEndian Float ) |]
          DoubleCodec{} -> Just [| mempty :: S.Vector (BigEndian Double) |]
          _ -> Nothing
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

        extractWord64Match :: Trie a -> [(Word64, Trie a)]
        extractWord64Match t0 = go 0 t0
          where
            go n t
              | n >= 8 = pure (0, t)
              | All w8 t' <- t = do
                (acc, cont) <- go (n + 1) t'
                pure (acc .|. (fromIntegral w8 `unsafeShiftL` (n * 8)), cont)
              | Branch xs <- t = do
                (w8, t') <- xs
                (acc, cont) <- go (n + 1) t'
                pure (acc .|. (fromIntegral w8 `unsafeShiftL` (n * 8)), cont)
              | otherwise = error "extractWord64Match but not 8 bytes worth of stuff!"
        
        extractWord32Match :: Trie a -> [(Word32, Trie a)]
        extractWord32Match t0 = go 0 t0
          where
            go n t
              | n >= 4 = pure (0, t)
              | All w8 t' <- t = do
                (acc, cont) <- go (n + 1) t'
                pure (acc .|. (fromIntegral w8 `unsafeShiftL` (n * 8)), cont)
              | Branch xs <- t = do
                (w8, t') <- xs
                (acc, cont) <- go (n + 1) t'
                pure (acc .|. (fromIntegral w8 `unsafeShiftL` (n * 8)), cont)
              | otherwise = error "extractWord64Match but not 4 bytes worth of stuff!"

        extractWord16Match :: Trie a -> [(Word16, Trie a)]
        extractWord16Match t0 = go 0 t0
          where
            go n t
              | n >= 2 = pure (0, t)
              | All w8 t' <- t = do
                (acc, cont) <- go (n + 1) t'
                pure (acc .|. (fromIntegral w8 `unsafeShiftL` (n * 8)), cont)
              | Branch xs <- t = do
                (w8, t') <- xs
                (acc, cont) <- go (n + 1) t'
                pure (acc .|. (fromIntegral w8 `unsafeShiftL` (n * 8)), cont)
              | otherwise = error "extractWord64Match but not 2 bytes worth of stuff!"

        extractWord8Match :: Trie a -> [(Word8, Trie a)]
        extractWord8Match = \case
          All w8 t -> pure (w8, t)
          Branch xs -> xs
          _ -> error "extractWord8Match but not 1 byte worth of stuff"

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

parseArray :: forall x1 st r . S.Storable x1 => Int -> (S.Vector x1 -> FP.ParserT st NBTError r) -> FP.ParserT st NBTError r
{-# INLINE parseArray #-}
parseArray len f = do
  let szEl = S.sizeOf (undefined :: x1)
  BS.BS fp _ <- FP.take (len * szEl)
  f $ S.unsafeFromForeignPtr0 (coerce fp) len

data Trie a = All !Word8 !(Trie a) | Branch [(Word8, Trie a)] | Leaf a | Empty
  deriving stock (Show, Functor)

-- Invariant: We have at least sz bytes available
-- Invariant: addr points to right after the size prefix
skipKeyWithSizeWithBack :: Int -> Addr# -> FP.ParserT st e r -> FP.ParserT st e r
{-# INLINE skipKeyWithSizeWithBack #-}
skipKeyWithSizeWithBack (I# sz) addr cont
  = FP.ParserT (\fp eob _ st ->
    -- TODO This works, and generates good code just fine,
    -- but since we already deconstruct the parser anyway,
    -- I should just take the bytestring directly instead
    -- of going through takeExtraUnsafe#
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
