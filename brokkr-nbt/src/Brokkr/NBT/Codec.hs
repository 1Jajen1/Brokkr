{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-splices -fforce-recomp #-}
module Brokkr.NBT.Codec (
  NBTCodec
, CodecContext(..)
-- api
, dimapCodec
, rmapCodec
, lmapCodec
, (<$#>)
, pureC
, (<*#>)
-- class
, HasCodec(..)
-- combinators
, compound
, requiredField
, optionalField
, (.=)
, utf8String
, unsafeIntArray
, unsafeLongArray
-- gen
, genParser
-- , genNBTReader
-- , genBuilder
-- , genNBTWriter
) where

import Brokkr.NBT.ByteOrder
import Brokkr.NBT.Internal
import Brokkr.NBT.NBTString.Internal
import Brokkr.NBT.Slice

import Control.Monad
import Control.Monad.ST.Strict (runST)

import Data.Bits
import Data.Bool (bool)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS

import Data.Foldable (find,foldl')

import Data.Int
import Data.List (deleteBy, groupBy, sortOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive

import Data.Text (Text)
import Data.Text qualified as T

import Data.Vector.Storable qualified as S

import Data.Word

import FlatParse.Basic qualified as FP

import GHC.Exts
import GHC.Float
import GHC.Word

import Language.Haskell.TH qualified as TH

data CodecContext = Compound | Value

-- | A template haskell based nbt codec
data NBTCodec (c :: CodecContext) i o where
  TagCodec :: NBTCodec Value Tag Tag

  ByteCodec   :: Maybe Text -> NBTCodec Value Int8   Int8
  ShortCodec  :: Maybe Text -> NBTCodec Value Int16  Int16
  IntCodec    :: Maybe Text -> NBTCodec Value Int32  Int32
  LongCodec   :: Maybe Text -> NBTCodec Value Int64  Int64

  FloatCodec  :: Maybe Text -> NBTCodec Value Float  Float
  DoubleCodec :: Maybe Text -> NBTCodec Value Double Double
  
  StringCodec :: Maybe Text -> NBTCodec Value NBTString NBTString

  ByteArrayCodec :: Maybe Text -> NBTCodec Value (S.Vector Int8)    (S.Vector Int8)
  IntArrayCodec  :: Maybe Text -> NBTCodec Value (S.Vector Int32BE) (S.Vector Int32BE)
  LongArrayCodec :: Maybe Text -> NBTCodec Value (S.Vector Int64BE) (S.Vector Int64BE)

  ListCodec :: Maybe Text -> NBTCodec Value i o -> NBTCodec Value (SmallArray i) (SmallArray o)
  
  CompoundCodec :: Maybe Text -> NBTCodec Compound i o -> NBTCodec Value i o

  RequiredKeyCodec :: NBTString -> NBTCodec Value i o -> Maybe Text -> Maybe (Code o) -> NBTCodec Compound i o
  
  OptionalKeyCodec :: NBTString -> NBTCodec Value i o -> Maybe Text -> NBTCodec Compound (Maybe i) (Maybe o)

  PureCodec :: Code o -> NBTCodec Compound i o
  ApCodec :: NBTCodec Compound i (o -> o') -> NBTCodec Compound i o -> NBTCodec Compound i o'

  -- TODO Add another codec for Dimap that may fail on the output
  RmapCodec :: Code (o -> o') -> NBTCodec c i o -> NBTCodec c i o'
  LmapCodec :: Code (i' -> i) -> NBTCodec c i o -> NBTCodec c i' o

-- Api
dimapCodec :: Code (o -> o') -> Code (i' -> i) -> NBTCodec c i o -> NBTCodec c i' o'
dimapCodec r l = rmapCodec r . lmapCodec l

lmapCodec :: Code (i' -> i) -> NBTCodec c i o -> NBTCodec c i' o
lmapCodec = LmapCodec

rmapCodec :: Code (o -> o') -> NBTCodec c i o -> NBTCodec c i o'
rmapCodec = RmapCodec

infixl 4 <$#>
(<$#>) :: Code (o -> o') -> NBTCodec c i o -> NBTCodec c i o'
(<$#>) = rmapCodec

pureC :: Code o -> NBTCodec Compound i o
pureC = PureCodec

infixl 4 <*#>
(<*#>) :: NBTCodec Compound i (o -> o') -> NBTCodec Compound i o -> NBTCodec Compound i o'
(<*#>) = ApCodec

-- class
class HasCodec a where
  codec :: NBTCodec Value a a
  -- It makes sense to define codecDef if
  -- - a small default for 'a' exists
  -- - 'a' is likely to be unboxed if strict 
  codecDef :: Maybe (Code a)
  codecDef = Nothing

instance HasCodec Bool where
  codec = dimapCodec [|| (/= 0) ||] [|| bool 1 0 ||] $ codec @Int8

instance HasCodec Int8 where
  codec = ByteCodec Nothing
  codecDef = Just [|| 0 ||]
instance HasCodec Int16 where
  codec = ShortCodec Nothing
  codecDef = Just [|| 0 ||]
instance HasCodec Int32 where
  codec = IntCodec Nothing
  codecDef = Just [|| 0 ||]
instance HasCodec Int64 where
  codec = LongCodec Nothing
  codecDef = Just [|| 0 ||]
instance HasCodec Float where
  codec = FloatCodec Nothing
  codecDef = Just [|| 0 ||]
instance HasCodec Double where
  codec = DoubleCodec Nothing
  codecDef = Just [|| 0 ||]

instance HasCodec NBTString where
  codec = StringCodec Nothing
  codecDef = Just [|| NBTString (BS.empty) ||]
instance HasCodec Text where
  codec = dimapCodec [|| toText ||] [|| fromText ||] $ StringCodec Nothing
  codecDef = Just [|| T.empty ||]

instance HasCodec (S.Vector Int8) where
  codec = ByteArrayCodec Nothing
  codecDef = Just [|| S.empty ||]

instance HasCodec (S.Vector Int32BE) where
  codec = IntArrayCodec Nothing
  codecDef = Just [|| S.empty ||]
instance HasCodec (S.Vector Int32) where
  codec = dimapCodec [|| arrSwapBE32 ||] [|| arrSwapBE32 ||] codec
  codecDef = Just [|| S.empty ||]

instance HasCodec (S.Vector Int64BE) where
  codec = LongArrayCodec Nothing
  codecDef = Just [|| S.empty ||]
instance HasCodec (S.Vector Int64) where
  codec = dimapCodec [|| arrSwapBE64 ||] [|| arrSwapBE64 ||] codec
  codecDef = Just [|| S.empty ||]

instance HasCodec a => HasCodec [a] where
  codec = dimapCodec [|| toList ||] [|| smallArrFromList ||] $ ListCodec Nothing codec

-- TODO Fuse
smallArrFromList :: [a] -> SmallArray a
{-# INLINE smallArrFromList #-}
smallArrFromList [] = runST $ newSmallArray 0 (error "SmallArray empty") >>= unsafeFreezeSmallArray
smallArrFromList xs =
  let len = length xs
  in runST $ do
    mar <- newSmallArray len (error "SmallArray fromList init")
    let go !_ [] = pure ()
        go !n (y:ys) = writeSmallArray mar n y >> go (n + 1) ys
    go 0 xs
    unsafeFreezeSmallArray mar

instance HasCodec a => HasCodec (SmallArray a) where
  codec = ListCodec Nothing codec
  codecDef = Just [|| emptySmallArray ||]

instance HasCodec Tag where
  codec = TagCodec

-- Combinators

compound :: Text -> NBTCodec Compound i o -> NBTCodec Value i o
compound = CompoundCodec . Just

requiredField :: HasCodec a => NBTString -> NBTCodec Compound a a
requiredField key = RequiredKeyCodec key codec Nothing codecDef

optionalField :: HasCodec a => NBTString -> NBTCodec Compound (Maybe a) (Maybe a)
optionalField key = OptionalKeyCodec key codec Nothing

infixr 8 .=
(.=) :: NBTCodec Compound i o -> Code (i' -> i) -> NBTCodec Compound i' o
(.=) = flip lmapCodec

-- | Read 'NBTString' and convert into a utf8 'ByteString'
--
-- If the underlying data is already valid utf8 this method will not allocate.
utf8String :: Maybe Text -> NBTCodec Value ByteString ByteString
utf8String = dimapCodec [|| toUtf8 ||] [|| fromUtf8 ||] . StringCodec

unsafeIntArray :: Maybe Text -> NBTCodec Value (S.Vector Int32) (S.Vector Int32)
unsafeIntArray = dimapCodec [|| arrSwapBE32 ||] [|| arrSwapBE32 ||] . IntArrayCodec

unsafeLongArray :: Maybe Text -> NBTCodec Value (S.Vector Int64) (S.Vector Int64)
unsafeLongArray = dimapCodec [|| arrSwapBE64 ||] [|| arrSwapBE64 ||] . LongArrayCodec

-- Generate a parser

-- | Generate a parser which reads binary 'NBT'
genParser :: forall i0 o st0 . NBTCodec Value i0 o -> Code (FP.ParserT st0 DecodeError o)
genParser c0 = [|| FP.anyWord8 >>= \t -> parseNBTString >> $$(go [] c0) t ||]
  where
    go :: forall i x st . [Text] -> NBTCodec Value i x -> Code (Word8 -> FP.ParserT st DecodeError x)
    go _ TagCodec = [|| parseTag ||]

    go path (ByteCodec  name) = [|| \t -> if t == 1 then FP.anyInt8    else FP.err $ invalidType path "byte"  name ||]
    go path (ShortCodec name) = [|| \t -> if t == 2 then FP.anyInt16be else FP.err $ invalidType path "short" name ||]
    go path (IntCodec   name) = [|| \t -> if t == 3 then FP.anyInt32be else FP.err $ invalidType path "int"   name ||]
    go path (LongCodec  name) = [|| \t -> if t == 4 then FP.anyInt64be else FP.err $ invalidType path "long"  name ||]

    go path (FloatCodec  name) = [|| \t -> if t == 5 then castWord32ToFloat  <$> FP.anyWord32be else FP.err $ invalidType path "float"  name ||]
    go path (DoubleCodec name) = [|| \t -> if t == 6 then castWord64ToDouble <$> FP.anyWord64be else FP.err $ invalidType path "double" name ||]

    go path (ByteArrayCodec name) = [|| \t -> if t == 7  then takeArray else FP.err $ invalidType path "byte array" name ||]
    go path (IntArrayCodec name)  = [|| \t -> if t == 11 then takeArray else FP.err $ invalidType path "int array"  name ||]
    go path (LongArrayCodec name) = [|| \t -> if t == 12 then takeArray else FP.err $ invalidType path "long array" name ||]

    go path (StringCodec name) = [|| \t -> if t == 8 then parseNBTString else FP.err $ invalidType path "string" name ||]

    go path (ListCodec name inner) = [|| \t -> if t == 9
      then do
        innerT <- FP.anyWord8
        len@(I# len#) <- fromIntegral <$> FP.anyInt32be
        localST $ do
          SmallMutableArray mut <- FP.liftST $ newSmallArray len (error "parse nbt list")

          let goList   _ 0# = pure ()
              goList i n  = do
                el <- $$(go ("<list" <> maybe "" ("#"<>) name <> ">" : path) inner) innerT
                FP.liftST $ writeSmallArray (SmallMutableArray mut) (I# i) el
                goList (i +# 1#) (n -# 1#)

          goList 0# len#
          arr <- FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut)
          pure arr
      else FP.err $ invalidType path "list" name ||]
    
    go path (CompoundCodec name inner) = [|| \t -> if t == 10
      then $$(goCompound ("<compound" <> maybe "" ("#"<>) name <> ">" : path) inner)
      else FP.err $ invalidType path "compound" name
      ||]
    
    go path (RmapCodec f inner) = [|| fmap $$(f) . $$(go path inner) ||]
    go path (LmapCodec _ inner) = go path inner

    goCompound :: forall i1 x0 st . [Text] -> NBTCodec Compound i1 x0 -> Code (FP.ParserT st DecodeError x0)
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
          varNames' <- traverse (\(_,mdef,_) -> (isJust mdef,) <$> TH.newName "x") keysToParsers
          let varNames = varNames' ++ [(True, bm)]
              validKey tagId back sz = [| do
                skipKeyWithSizeWithBack $(sz) back $ do
                  skipTag $(tagId)
                  $(foldl' (\acc x -> TH.appE acc (TH.varE x)) (TH.varE funcN) $ fmap snd varNames) |]
              initBM :: Int = (1 `unsafeShiftL` (length keysToParsers)) - 1
              genBranch tagId sz trie0 =
                let go64 numRead [] cont = cont numRead
                    go64 (numRead :: Int) (x:xs) cont
                      = [| withAnyWord64Unsafe $ \w ->
                          if w == x then $(go64 (numRead + 8) xs cont)
                          else $(validKey tagId (numRead + 8) [| sz |])
                          |]
                    goTail numRead [] cont = cont numRead
                    goTail (numRead :: Int) (a:b:c:d:e:f:g:h:xs) cont
                      = [| withAnyWord64Unsafe $ \w ->
                          if w == w64 then $(goTail (numRead + 8) xs cont)
                          else $(validKey tagId (numRead + 8) [| sz |])
                          |]
                      where w64 :: Word64 = foldr (\x acc -> fromIntegral x .|. (acc `unsafeShiftL` 8)) 0 [a,b,c,d,e,f,g,h]
                    goTail (numRead :: Int) (a:b:c:d:xs) cont
                      = [| withAnyWord32Unsafe $ \w ->
                          if w == w32 then $(goTail (numRead + 4) xs cont)
                          else $(validKey tagId (numRead + 4) [| sz |])
                          |]
                      where w32 :: Word32 = foldr (\x acc -> fromIntegral x .|. (acc `unsafeShiftL` 8)) 0 [a,b,c,d]
                    goTail (numRead :: Int) (a:b:xs) cont
                      = [| withAnyWord16Unsafe $ \w ->
                          if w == w16 then $(goTail (numRead + 2) xs cont)
                          else $(validKey tagId (numRead + 2) [| sz |])
                          |]
                      where w16 :: Word16 = foldr (\x acc -> fromIntegral x .|. (acc `unsafeShiftL` 8)) 0 [a,b]
                    goTail (numRead :: Int) (a:xs) cont
                      = [| withAnyWord8Unsafe $ \w ->
                          if w == a then $(goTail (numRead + 1) xs cont)
                          else $(validKey tagId (numRead + 1) [| sz |])
                          |]
                    goTrie trie =
                      let (pre,post) = extractPrefix trie
                          (pre64, preTail) = chunked pre
                      in go64 0 pre64 $ \takenInp -> [|
                         $(case post of
                           -- These two are filtered out by extractPrefix
                           Empty   -> error "Empty"
                           All _ _ -> error "All"
                           -- A trie with a single element only
                           Leaf (ind,_,withCont) -> goTail takenInp preTail . const $ [| $(withCont $ [|
                             \el -> $(TH.appE (snd $ foldl' (\(i, acc) x ->
                               if i == ind then
                                 (i + 1, TH.appE acc ([| el |]))
                               else (i + 1, TH.appE acc (TH.varE x))) (0, TH.varE funcN) $ fmap snd varNames')
                               [| clearBit @Int $(TH.varE bm) ind |])
                               |]) $(tagId) |]
                           -- Or a branch...
                           Branch branches ->
                             let (preTail', branchByteSz, scrutF, branches') = case preTail of
                                   [a] -> ([], 2, [| withAnyWord16Unsafe |], (\(w, t) -> ([a,w],t)) <$> branches)
                                   [a,b,c] -> ([], 4, [| withAnyWord32Unsafe |], (\(w, t) -> ([a,b,c,w],t)) <$> branches)
                                   [a,b,c,d,e,f,g] -> ([], 8, [| withAnyWord64Unsafe |], (\(w, t) -> ([a,b,c,d,e,f,g,w],t)) <$> branches)
                                   _ -> (preTail, 1, [| withAnyWord8Unsafe |], (\(w,t) -> ([w],t)) <$> branches)
                             in goTail takenInp preTail' $ \takenInp' -> [|
                               $(scrutF) $ \scrut ->
                                 $(let mkMatch bs t = TH.match
                                         (TH.litP $ TH.IntegerL $ foldr (\a acc -> fromIntegral a .|. (acc `unsafeShiftL` 8)) 0 bs)
                                         (TH.normalB $ goTrie t) []
                                       fb = TH.match TH.wildP (TH.normalB [| $(validKey tagId (takenInp' + branchByteSz) [| sz |]) |]) []
                                   in TH.caseE [| scrut |] $ foldr (\(bs, t) acc -> mkMatch bs t : acc) [fb] branches')
                               |])
                         |]
                  in goTrie trie0
          TH.letE [
            let bod = [|
                  FP.withAnyWord8 $ \tag -> do
                    if tag == 0
                      then FP.empty -- we abort this loop early when we are done, so done here means fail
                      else FP.withAnyWord16 $ \w -> do
                        let w' = fromIntegral $ byteSwap16 w
                        FP.ensure w'
                        $(let fb = TH.match TH.wildP (TH.normalB $ validKey [| tag |] 0 [| w' |]) []
                              buildMatch (sz, trie) = TH.match (TH.litP $ TH.IntegerL (fromIntegral sz)) (TH.normalB $ genBranch [| tag |] sz trie) []
                          in TH.caseE [| w' |] $ foldr (\x acc -> buildMatch x : acc) [fb] buildTries)
                  |]
                topF = extractTopLevelFunc inner
                doneBod = [| skipCompound >> pure $(foldl' (\acc x -> TH.appE acc (TH.varE x)) topF (fmap snd varNames')) |]
            in TH.funD funcN
                [ TH.clause (((\(bang,n) -> (if bang then TH.bangP else id) $ TH.varP n) <$> varNames') ++ [TH.litP $ TH.IntegerL 0]) (TH.normalB doneBod) []
                , TH.clause ((\(bang,n) -> (if bang then TH.bangP else id) $ TH.varP n) <$> varNames) (TH.normalB bod) []
                ]
            ] $ TH.appE (foldl' (\acc (_,mdef,_) -> TH.appE acc $ fromMaybe [| error "unitialized" |] mdef) (TH.varE funcN) keysToParsers) [| initBM |]
      where
        inner = simplifyCodec inner'

        -- [key,default,cont -> parser]
        keysToParsers = collectKeys True inner

        collectKeys :: forall i x . Bool -> NBTCodec Compound i x -> [(NBTString, Maybe (TH.Q TH.Exp), TH.Q TH.Exp -> TH.Q TH.Exp)]
        collectKeys _ (PureCodec _) = []
        collectKeys False (RmapCodec f i) = (\(k,d,e) -> (k,d,\e' -> [| fmap $(TH.unTypeCode f) . $(e e') |])) <$> collectKeys False i
        collectKeys True  (RmapCodec _ i) = collectKeys True i
        collectKeys b (LmapCodec _ i) = collectKeys b i
        collectKeys _ (RequiredKeyCodec key i _descr def) =
          [(key, fmap TH.unTypeCode def, \cont -> [| \t -> $(TH.unTypeCode $ go (toText key : path) i) t >>= $(cont) |])]
        -- TODO Failure should map to Maybe instead!
        collectKeys _ (OptionalKeyCodec key i _descr) =
          [(key, Just [| Nothing |], \cont -> [| \t -> $(TH.unTypeCode $ go (toText key : path) i) t >>= $(cont) |])]
        collectKeys b ( ApCodec ff fa) = collectKeys b ff <> collectKeys False fa

        buildTries :: [(Int, Trie (Int, Maybe (TH.Q TH.Exp), TH.Q TH.Exp -> TH.Q TH.Exp))]
        buildTries = fmap (\xs -> (depth xs, goBuildTrie xs Empty)) grouped
          where
            expandedBytes = (fmap (\(i, (NBTString k,d,c)) -> (BS.unpack k,i,d,c)) $ zip [0..] keysToParsers)
            grouped = groupBy (\(k1,_,_,_) (k2,_,_,_) -> length k1 == length k2) $ sortOn (\(k,_,_,_) -> length k) expandedBytes
            depth [] = error "Empty trie"
            depth ((bs,_,_,_):_) = length bs
            goBuildTrie [] acc = acc
            goBuildTrie ((bs,i,d,c):xs) acc = insert bs (i,d,c) $ goBuildTrie xs acc
            insert [] v = \case
              Empty -> Leaf v
              _ -> error "Uneq length"
            insert (x:xs) v = \case
              Empty -> All x (insert xs v Empty)
              All y t
                | y == x    -> All x (insert xs v t)
                | otherwise -> Branch [(y,t),(x, insert xs v Empty)]
              Branch ys
                | Just e@(_,t) <- find (\(k,_) -> k == x) ys -> Branch $ (x,insert xs v t) : deleteBy (\a b -> fst a == fst b) e ys
                | otherwise -> Branch $ (x,insert xs v Empty) : ys
              Leaf _ -> error "Uneq length"
        
        extractPrefix :: Trie a -> ([Word8], Trie a)
        extractPrefix (All x t) = (\(pre,prefixTail) -> (x:pre,prefixTail)) $ extractPrefix t
        extractPrefix b@Branch{} = ([],b)
        extractPrefix l@Leaf{} = ([], l)
        extractPrefix Empty = ([], Empty)

        extractTopLevelFunc :: forall i x . NBTCodec Compound i x -> TH.Q TH.Exp
        extractTopLevelFunc (ApCodec ff _) = extractTopLevelFunc ff
        extractTopLevelFunc (RmapCodec f _) = TH.unTypeCode f
        extractTopLevelFunc (LmapCodec _ i) = extractTopLevelFunc i
        extractTopLevelFunc RequiredKeyCodec{} = [| pure |]
        extractTopLevelFunc _ = error "Cannot extract!"

        chunked :: [Word8] -> ([Word64], [Word8])
        chunked (a:b:c:d:e:f:g:h:xs) =
          let (ys, preTail) = chunked xs
          in (foldr (\x acc -> fromIntegral x .|. (acc `unsafeShiftL` 8)) 0 [a,b,c,d,e,f,g,h] : ys, preTail)
        chunked xs = ([],xs)

data Trie a = All !Word8 !(Trie a) | Branch [(Word8, Trie a)] | Leaf a | Empty
  deriving stock (Show, Functor)

simplifyCodec :: forall i x . NBTCodec Compound i x -> NBTCodec Compound i x
simplifyCodec (LmapCodec l i) = constantFold (LmapCodec l (simplifyCodec i))
simplifyCodec (RmapCodec r i) = constantFold (RmapCodec r (simplifyCodec i))
simplifyCodec (ApCodec ff fa) = constantFold (ApCodec (simplifyCodec ff) (simplifyCodec fa))
simplifyCodec p = constantFold p

constantFold :: forall i x . NBTCodec Compound i x -> NBTCodec Compound i x

-- Try to collapse <*> and try to combine constant expressions
-- We try to move rmap further inside, closer towards constants and lmap outwards
-- We also reassociate <*> towards the left

-- f <$> pure a = pure (f a)
constantFold (RmapCodec f (PureCodec a)) = PureCodec [|| $$(f) $$(a) ||]
-- lmap f (pure a) = pure (f a)
constantFold (LmapCodec _ (PureCodec a)) = PureCodec [|| $$(a) ||]
-- f <$> (g <$> fa) = (f . g) <$> fa
constantFold (RmapCodec f (RmapCodec g fa)) = RmapCodec [|| $$(f) . $$(g) ||] fa
-- lmap f . lmap g = lmap (g . f)
constantFold (LmapCodec f (LmapCodec g fa)) = LmapCodec [|| $$(g) . $$(f) ||] fa
-- pure f <*> fa = f <$> fa
constantFold (ApCodec (PureCodec f) fa) = constantFold $ RmapCodec f fa
-- fl <*> (fr <*> fa) = (((.) <$> fl) <*> fr) <*> fa
constantFold (ApCodec fl (ApCodec fr fa)) = constantFold $ ApCodec (constantFold $ ApCodec (constantFold $ RmapCodec [|| (.) ||] fl) fr) fa
-- f <$> (fl <*> fa) = ((. f) <$> fl) <*> g
constantFold (RmapCodec f (ApCodec ff fa)) = constantFold $ ApCodec (constantFold $ RmapCodec [|| (.) $$(f) ||] ff) fa
-- f <$> (lmap r fa) = lmap r (f <$> fa)
constantFold (RmapCodec f (LmapCodec r fa)) = constantFold $ LmapCodec r (constantFold $ RmapCodec f fa)

constantFold p = p

skipCompound :: FP.ParserT st e ()
{-# INLINE skipCompound #-}
skipCompound = do
  t <- FP.anyWord8
  if t == 0 then pure () else skipKey >> skipTag t >> skipCompound

skipKey :: FP.ParserT st e ()
{-# INLINE skipKey #-}
skipKey = withNBTString $ \_ -> pure ()

skipKeyWithSizeWithBack :: Int -> Int -> FP.ParserT st e r -> FP.ParserT st e r
{-# INLINE skipKeyWithSizeWithBack #-}
skipKeyWithSizeWithBack (I# sz) (I# back) cont
  = FP.skipBack# back >> FP.takeUnsafe# sz >>= \bs ->
    if (isValidModifiedUtf8 bs) then cont
    else FP.empty

-- TODO I need to perform a heap check here to avoid hogging a thread.
-- This is 100% allocation free and thus very dangerous when applied to
-- user input, especially if that input came compressed.
skipTag :: Word8 -> FP.ParserT st e ()
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
skipTag 11 = FP.anyInt32be >>= void . FP.skip . fromIntegral
skipTag 12 = FP.anyInt32be >>= void . FP.skip . fromIntegral
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

-- | Generate a parser which reads already parsed 'NBT'
genNBTReader :: forall i0 o . NBTCodec Value i0 o -> Code (NBT -> Either DecodeError o)
genNBTReader c = [|| \(NBT _ t) -> $$(go [] c) t ||]
  where
    go :: forall i x . [Text] -> NBTCodec Value i x -> Code (Tag -> Either DecodeError x)
    go _ TagCodec   = [|| Right ||]

    go path (ByteCodec name) = [|| \case
      TagByte b -> Right b
      _ -> Left $ invalidType path "byte" name
      ||]
    go path (ShortCodec name) = [|| \case
      TagShort b -> Right b
      _ -> Left $ invalidType path "short" name
      ||]
    go path (IntCodec name) = [|| \case
      TagInt b -> Right b
      _ -> Left $ invalidType path "int" name
      ||]
    go path (LongCodec name) = [|| \case
      TagLong b -> Right b
      _ -> Left $ invalidType path "long" name
      ||]
    go path (FloatCodec name) = [|| \case
      TagFloat b -> Right b
      _ -> Left $ invalidType path "float" name
      ||]
    go path (DoubleCodec name) = [|| \case
      TagDouble b -> Right b
      _ -> Left $ invalidType path "double" name
      ||]
    
    go path (StringCodec name) = [|| \case
      TagString b -> Right b
      _ -> Left $ invalidType path "string" name
      ||]
    
    go path (ByteArrayCodec name) = [|| \case
      TagByteArray b -> Right b
      _ -> Left $ invalidType path "byte array" name
      ||]
    go path (IntArrayCodec name) = [|| \case
      TagIntArray b -> Right b
      _ -> Left $ invalidType path "int array" name
      ||]
    go path (LongArrayCodec name) = [|| \case
      TagLongArray b -> Right b
      _ -> Left $ invalidType path "long array" name
      ||]

    -- TODO
    go path (ListCodec name _inner) = [|| \case
      -- TagList v
      --   | V.null v -> Right V.empty
      --   | otherwise -> traverse ($$(go ("<list" <> maybe "" ("#"<>) name <> ">" : path) inner)) v
      _ -> Left $ invalidType path "list" name
      ||]
    
    go path (CompoundCodec name inner) = [|| \case
      TagCompound v -> $$(goCompound ("<compound" <> maybe "" ("#"<>) name <> ">" : path) inner) v
      _ -> Left $ invalidType path "compound" name
      ||]

    go path (RmapCodec f inner) = [|| fmap $$(f) . $$(go path inner) ||]
    go path (LmapCodec _ inner) = go path inner

    goCompound :: forall i x . [Text] -> NBTCodec Compound i x -> Code (Slice NBT -> Either DecodeError x)
    goCompound _ (PureCodec x) = [|| const (Right $$(x)) ||]

    goCompound path (ApCodec ff fa) = [|| \v -> $$(goCompound path ff) v <*> $$(goCompound path fa) v ||]

    goCompound path (RmapCodec f inner) = [|| fmap $$(f) . $$(goCompound path inner) ||]
    goCompound path (LmapCodec _ inner) = goCompound path inner

    goCompound path (RequiredKeyCodec key inner _descr _def) = [|| \slice ->
      case findWithIndexNBT key slice of
        (# _, (# (NBT _ t) | #) #) -> $$(go (toText key : path) inner) t
        (# _, (# | (##) #) #) -> Left . missingKey path $ toText key
      ||]

    goCompound path (OptionalKeyCodec key inner _descr) = [|| \slice ->
      case findWithIndexNBT key slice of
        (# _, (# (NBT _ t) | #) #) -> Just <$> $$(go (toText key : path) inner) t
        (# _, (# | (##) #) #) -> Right Nothing
      ||]

findWithIndexNBT :: NBTString -> Slice NBT -> (# Int#, (# NBT | (# #) #) #)
findWithIndexNBT = findWithIndex(\(NBT k _) -> k) 

-- genNBTWriter :: forall i0 o0 . NBTCodec Value i0 o0 -> Code (i0 -> NBT)
-- genNBTWriter c0 = [|| \i -> NBT "" $ $$(go c0) i ||]
--   where
--     go :: forall i o . NBTCodec Value i o -> Code (i -> Tag)
--     go TagCodec = [|| id ||]
--     go (ByteCodec _)  = [|| TagByte ||]
--     go (ShortCodec _) = [|| TagShort ||]
--     go (IntCodec _)   = [|| TagInt ||]
--     go (LongCodec _)  = [|| TagLong ||]
--     go (FloatCodec _)  = [|| TagFloat ||]
--     go (DoubleCodec _) = [|| TagDouble ||]
--     go (ByteArrayCodec _) = [|| TagByteArray ||]
--     go (IntArrayCodec _)  = [|| TagIntArray  ||]
--     go (LongArrayCodec _) = [|| TagLongArray ||]
--     go (StringCodec _) = [|| TagString ||]
--     go (ListCodec _ _inner) = error "TODO"
--     go (CompoundCodec _ _inner) = error "TODO"
--     go (RmapCodec _ inner) = go inner
--     go (LmapCodec f inner) = [|| $$(go inner) . $$(f) ||]


newtype DecodeError = DecodeError Text
  deriving newtype Show

missingKey :: [Text] -> Text -> DecodeError
missingKey path key = DecodeError $ "Missing key " <> key <> "." <> showPath path

invalidType :: [Text] -> Text -> Maybe Text -> DecodeError
invalidType path ty Nothing = DecodeError $ "Invalid type. Expected " <> ty <> "." <> showPath path
invalidType path ty (Just name) = DecodeError $ "Invalid type for " <> name <> ". Expected " <> ty <> "." <> showPath path

showPath :: [Text] -> Text
showPath (Reverse []) = " At path <root>"
showPath (Reverse (x:xs)) = " At path " <> x <> foldr (\y acc -> "." <> y <> acc) "" xs

-- Honestly this only exists to make ghc shut up
--  on a warning on incomplete patterns
pattern Reverse :: [a] -> [a]
pattern Reverse xs <- (reverse -> xs)
  where Reverse xs = reverse xs
{-# COMPLETE Reverse #-}


-- TH utils

type Code a = TH.Code TH.Q a


