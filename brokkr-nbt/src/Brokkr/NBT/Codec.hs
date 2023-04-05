{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all -ddump-splices -fforce-recomp #-}
module Brokkr.NBT.Codec (
  NBTCodec
, CodecContext(..)
, Code
-- api
, dimapCodec
, rmapCodec
, lmapCodec
, (<$#>)
, pureC
, (<*#>)
-- class
, HasCodec(..)
, ViaSizedInt(..)
-- combinators
, compound
, requiredField
, requiredFieldVia
, optionalField
, (.=)
, viaCodec
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
import Brokkr.NBT.NBTError
import Brokkr.NBT.NBTString.Internal
import Brokkr.NBT.Slice

import Control.Monad
import Control.Monad.ST.Strict (runST)

import Data.Bifunctor (first)
import Data.Bits
import Data.Bool (bool)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Internal qualified as BS

import Data.Foldable (find,foldl')

import Data.Int
import Data.List (deleteBy, groupBy, sortOn)
import Data.Maybe (fromMaybe, isJust)
import Data.Primitive

import Data.Text (Text)
import Data.Text qualified as T

import Data.Vector          qualified as V
import Data.Vector.Storable qualified as S

import Data.Word

import FlatParse.Basic qualified as FP

import GHC.Exts
import GHC.Float
import GHC.ForeignPtr
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

  RequiredKeyCodec :: NBTString -> NBTCodec Value i o -> Maybe Text -> NBTCodec Compound i o
  
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

instance HasCodec Bool where
  codec = dimapCodec [|| (/= 0) ||] [|| bool 1 0 ||] $ codec @Int8

instance HasCodec Int8 where
  codec = ByteCodec Nothing
instance HasCodec Int16 where
  codec = ShortCodec Nothing
instance HasCodec Int32 where
  codec = IntCodec Nothing
instance HasCodec Int64 where
  codec = LongCodec Nothing
instance HasCodec Float where
  codec = FloatCodec Nothing
instance HasCodec Double where
  codec = DoubleCodec Nothing
instance HasCodec NBTString where
  codec = StringCodec Nothing
instance HasCodec Text where
  codec = dimapCodec [|| toText ||] [|| fromText ||] $ StringCodec Nothing
  -- TODO Check what happens if this is the first argument. It may break...
  -- codecDef = Just [|| T.empty ||]

instance HasCodec (S.Vector Int8) where
  codec = ByteArrayCodec Nothing

instance HasCodec (S.Vector Int32BE) where
  codec = IntArrayCodec Nothing
instance HasCodec (S.Vector Int32) where
  codec = dimapCodec [|| arrSwapBE32 @Int32BE ||] [|| arrSwapBE32 @Int32 ||] codec

instance HasCodec (S.Vector Int64BE) where
  codec = LongArrayCodec Nothing
instance HasCodec (S.Vector Int64) where
  codec = dimapCodec [|| arrSwapBE64 @Int64BE ||] [|| arrSwapBE64 @Int64 ||] codec

instance HasCodec a => HasCodec [a] where
  codec = dimapCodec [|| toList ||] [|| smallArrFromList ||] $ ListCodec Nothing codec

-- TODO Fuse
smallArrFromList :: [a] -> SmallArray a
{-# INLINE smallArrFromList #-}
smallArrFromList [] = emptySmallArray
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

instance HasCodec a => HasCodec (V.Vector a) where
  -- In the future just unsafe coerce this... SmallArray# == Array# on the heap?
  codec = dimapCodec
    [|| \xs -> V.fromListN (sizeofSmallArray xs) $ toList xs ||]
    [|| \v -> smallArrayFromListN (V.length v) $ V.toList v ||] $ ListCodec Nothing codec
  -- codecDef = Just [|| V.empty ||]

instance HasCodec Tag where
  codec = TagCodec

newtype ViaSizedInt a = ViaSizedInt Int

instance (HasCodec a, Integral a) => HasCodec (ViaSizedInt a) where
  codec = dimapCodec [|| ViaSizedInt . fromIntegral ||] [|| \(ViaSizedInt n) -> fromIntegral n ||] (codec @a)

-- This more general instance will throw you into the depths of hell
-- TODO Retry this but without using coerce and explicitly doing the wrapping
-- instance (HasCodec a, Integral a, Integral b) => HasCodec (ViaIntegral a b) where
--   codec = dimapCodec (TH.unsafeCodeCoerce [| coerce . fromIntegral |]) (TH.unsafeCodeCoerce [| fromIntegral . coerce |]) (codec @a)
--   codecDef = Just [|| ViaIntegral 0 ||]

-- Combinators

compound :: Text -> NBTCodec Compound i o -> NBTCodec Value i o
compound = CompoundCodec . Just

requiredField :: HasCodec a => NBTString -> NBTCodec Compound a a
requiredField key = RequiredKeyCodec key codec Nothing

requiredFieldVia :: forall a b . (HasCodec a, Coercible a b) => NBTString -> NBTCodec Compound b b
requiredFieldVia key = RequiredKeyCodec key (dimapCodec [|| coerce ||] [|| coerce ||] $ codec @a) Nothing

optionalField :: HasCodec a => NBTString -> NBTCodec Compound (Maybe a) (Maybe a)
optionalField key = OptionalKeyCodec key codec Nothing

infixr 8 .=
(.=) :: NBTCodec Compound i o -> Code (i' -> i) -> NBTCodec Compound i' o
(.=) = flip lmapCodec

viaCodec :: Coercible a b => NBTCodec ctx a a -> NBTCodec ctx b b
viaCodec = dimapCodec [|| coerce ||] [|| coerce ||]

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

    go path (ByteArrayCodec name) = [|| \t -> if t == 7  then takeArray @Int8 else FP.err $ invalidType path "byte array" name ||]
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
                           Leaf (ind,_,withCont) -> goTail preTail [| $(withCont [|
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
                          FP.err $ missingKey path (T.pack $ show $(TH.varE bm))
                        else
                          -- we are done, the only missing keys were optional
                          pure $(foldl' (\acc x -> TH.appE acc (TH.varE x)) topF (fmap snd varNames'))
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
                          in TH.caseE [| w' |] $ foldr (\x acc -> buildMatch x : acc) [fb] buildTries)
                  |]
                topF = extractTopLevelFunc inner
                doneBod = [| skipCompound >> pure $(foldl' (\acc x -> TH.appE acc (TH.varE x)) topF (fmap snd varNames')) |]
            in TH.funD funcN
                [ TH.clause (((\(bang,n) -> (if bang then TH.bangP else id) $ TH.varP n) <$> varNames') ++ [TH.litP $ TH.IntegerL 0]) (TH.normalB doneBod) []
                , TH.clause ( (\(bang,n) -> (if bang then TH.bangP else id) $ TH.varP n) <$> varNames) (TH.normalB bod) []
                ]
            ] $ TH.appE (foldl' (\acc (_,_,def,_) -> TH.appE acc $ fromMaybe [| error "def" |] def) (TH.varE funcN) keysToParsers) [| initBM |]
      where
        inner = simplifyCodec inner'

        -- [key,default,cont -> parser]
        keysToParsers = collectKeys True inner

        collectKeys :: forall i x . Bool -> NBTCodec Compound i x -> [(NBTString, Bool, Maybe (TH.Q TH.Exp), TH.Q TH.Exp -> TH.Q TH.Exp)]
        collectKeys _ (PureCodec _) = []
        collectKeys False (RmapCodec f i) =
            (\(k,opt,d,e) -> (k,opt,(\dv -> [| $(TH.unTypeCode f) $(dv) |]) <$> d, \e' -> e [| $e' . $(TH.unTypeCode f) |])) <$> collectKeys False i
        collectKeys True  (RmapCodec _ i) = collectKeys True i
        collectKeys b (LmapCodec _ i) = collectKeys b i
        collectKeys _ (RequiredKeyCodec key i _descr) =
          [(key, False, TH.unTypeCode <$> defValue i, \cont -> [| $(TH.unTypeCode $ go (toText key : path) i) >=> $(cont) |])]
        collectKeys _ (OptionalKeyCodec key i _descr) =
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
        collectKeys b ( ApCodec ff fa) = collectKeys b ff <> collectKeys False fa

        defValue :: forall i x . NBTCodec Value i x -> Maybe (Code x)
        defValue TagCodec = Nothing
        defValue (ByteCodec _)   = Just [|| (0 :: Int8  ) ||]
        defValue (ShortCodec _)  = Just [|| (0 :: Int16 ) ||]
        defValue (IntCodec _)    = Just [|| (0 :: Int32 ) ||]
        defValue (LongCodec _)   = Just [|| (0 :: Int64 ) ||]
        defValue (FloatCodec _)  = Just [|| (0 :: Float ) ||]
        defValue (DoubleCodec _) = Just [|| (0 :: Double) ||]
        defValue (StringCodec _) = Just [|| NBTString BS.empty ||]
        defValue (ByteArrayCodec _) = Just [|| S.empty @Int8 ||]
        defValue (IntArrayCodec _)  = Just [|| S.empty @Int32BE ||]
        defValue (LongArrayCodec _) = Just [|| S.empty @Int64BE ||]
        defValue (ListCodec _ _) = Nothing
        defValue (CompoundCodec _ _) = Nothing
        defValue (RmapCodec f i) = (\dv -> [|| $$f $$(dv) ||]) <$> defValue i
        defValue (LmapCodec _ i) = defValue i

        buildTries :: [(Int, Trie (Int, Maybe (TH.Q TH.Exp), TH.Q TH.Exp -> TH.Q TH.Exp))]
        buildTries = fmap (\xs -> (depth xs, goBuildTrie xs Empty)) grouped
          where
            expandedBytes = (\(i, (NBTString k,_,d,c)) -> (BS.unpack k,i,d,c)) <$> zip [0..] keysToParsers
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
        extractPrefix (All x t) = first (x :) $ extractPrefix t
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
-- simplifyCodec p = p

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

-- | Generate a parser which reads already parsed 'NBT'
genNBTReader :: forall i0 o . NBTCodec Value i0 o -> Code (NBT -> Either NBTError o)
genNBTReader c = [|| \(NBT _ t) -> $$(go [] c) t ||]
  where
    go :: forall i x . [Text] -> NBTCodec Value i x -> Code (Tag -> Either NBTError x)
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

    goCompound :: forall i x . [Text] -> NBTCodec Compound i x -> Code (Slice NBT -> Either NBTError x)
    goCompound _ (PureCodec x) = [|| const (Right $$(x)) ||]

    goCompound path (ApCodec ff fa) = [|| \v -> $$(goCompound path ff) v <*> $$(goCompound path fa) v ||]

    goCompound path (RmapCodec f inner) = [|| fmap $$(f) . $$(goCompound path inner) ||]
    goCompound path (LmapCodec _ inner) = goCompound path inner

    goCompound path (RequiredKeyCodec key inner _descr) = [|| \slice ->
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


-- TH utils

type Code a = TH.Code TH.Q a


