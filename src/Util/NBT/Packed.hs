{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MultiWayIf #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Util.NBT.Packed (
  mkParser
, HasCodec(..)
, Codec(..)
, Context(..)
) where

import Util.NBT.Internal

import Data.Int
import Data.Void
import qualified Language.Haskell.TH as TH
import qualified FlatParse.Basic as FP
import qualified Util.Binary as Binary
import Control.Monad (when, void)
import Data.Kind
import GHC.Exts (dataToTag#, Int (I#), (+#))
import qualified Foreign as Ptr
import Data.Coerce (coerce)
import Data.List (nubBy, nub)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import Data.Word
import Control.Monad.Primitive
import qualified Data.Primitive as Ptr
import qualified Control.Monad.Primitive as Ptr
import qualified Data.Vector.Storable as S
import GHC.ForeignPtr (ForeignPtr(..))
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import Data.IORef
import Data.Maybe
import Data.Proxy

class HasCodec a where
  codec :: Codec Value a a

data Context = Value | Compound

data Codec (ctx :: Context) (inp :: Type) (out :: Type) where
  ByteCodec        :: Codec Value Int8 Int8
  ShortCodec       :: Codec Value Int16 Int16
  IntCodec         :: Codec Value Int32 Int32
  LongCodec        :: Codec Value Int64 Int64
  FloatCodec       :: Codec Value Float Float
  DoubleCodec      :: Codec Value Double Double
  ByteArrayCodec   :: Codec Value (S.Vector Int8) (S.Vector Int8)
  StringCodec      :: Codec Value NBTString NBTString
  ListCodec        :: Maybe Int -> Codec Value a a -> Codec Value (V.Vector a) (V.Vector a)
  CompoundCodec    :: Codec Compound a a -> Codec Value a a
  IntArrayCodec   :: Codec Value (S.Vector Int32) (S.Vector Int32)
  LongArrayCodec   :: Codec Value (S.Vector Int64) (S.Vector Int64)
  RequiredKeyCodec :: NBTString -> Codec Value a a -> Codec Compound a a
  PureCodec        :: Code a -> Codec Compound void a
  ApCodec          :: Codec Compound inp (out -> out') -> Codec Compound inp out -> Codec Compound inp out'
  DimapCodec       :: Code (a -> b) -> Code (c -> d) -> Codec ctx b c -> Codec ctx a d

type Code a = TH.Code TH.Q a

codecTag :: Codec ctx inp out -> Int
codecTag !c = I# (dataToTag# c +# 1#)

compoundId :: Int8
compoundId = fromIntegral $! codecTag (CompoundCodec $ PureCodec [|| () ||])

{-
How does this work?
We generate a parser that parses in 2 steps, first it writes all values we will need into a single chunk of memory in order that the codec needs them
Next we parse this chunk and actually create the final object

This avoids most allocation that we'd normally have when parsing into NBT, where every layer needs one allocation
Some of these allocations are also quite large (Lists/Compounds). It alos avoids the final NBT -> a step where we throw away most
NBT objects again.

So how does this work in detail:
- we parse the enclosing outer compounds type and key
- we allocate a growable mutable vector of bytearrays (no bytearrays allocated yet)
  - This vector contains dynamically allocated chunks which may result when parsing lists/compounds of dynamic size
- we allocate a fixed byte array
  - This contains the top level parse results from which we construct our value later
- We parse the outer compounds values
  - first we parse the type id of the next nbt (1)
    - if the id does not match one we look for we simply parse the rest of the nbt without doing anything special and
      start over at the next nbt (1)
    - if it does contain a match we continue with parsing the key (2)
  - parsing the keys generates a trie based parse (2)
    - if our key matches one codec we look for, we write the inner codecs value into a known fixed offset from the main array (3)
    - if we don't match anything we consume the rest and continue with the next (1)
  - we parse our value and write all necessary info into the data bytearray at a known fixed offset
    - for every value: We set the first bit at the offset to 1, to indicate a value has been set
    - if our inner codec is any number codec we simply write the number after the first byte
    - if we have a Byte|Int|LongArray- or String-Codec we write the size in elements and the offset from the end of the input
      - Why offset from the end? That's related to flatparse internals where only the current and end address is stored, so doing
        offset from the end is convenient
    - if we have a ListCodec we write its size in bytes and, if it fits, the actual elements into the main data
      - if our size exceeds our estimate we instead allocate a new chunk, add it to the IOVector (so GHC does not collect it)
        and write our data in there
    - If we have a compound tag:
      - if we parse the compound tag into a list of key + tag we do the same as the list codec (allocate if outside of estimate)
      - if we parse into an actual object with known keys we simply write the object directly into main data
- Now that all data is parsed we iterate our main data and create the final object
  - This basically applies all functions from ApCodec/DimapCodec to the values we have written into our main data
-}
mkParser :: Codec Value a a -> Code (FP.Parser Void a)
mkParser (CompoundCodec inner0) = [||
  do
    tid <- Binary.get @Int8
    when (tid /= compoundId) FP.empty
    sz <- Binary.get @Int16
    when (sz /= 0) $ FP.empty
    -- TODO Also allocate a mutable vector here which keeps track of all lists that we have to dynamically allocate
    dynArrRef <- pure $! unsafeInlineIO $ MV.new 16 >>= newIORef
    dynArrIndRef <- pure $! unsafeInlineIO $ Ptr.newByteArray 8
    arr <- pure $! unsafeInlineIO $ Ptr.newPinnedByteArray sz0 >>= Ptr.unsafeFreezeByteArray
    let !ptr = Ptr.byteArrayContents arr
        allocPtr sz = do
          mar <- Ptr.newPinnedByteArray sz
          arr <- Ptr.unsafeFreezeByteArray mar
          let !lPtr = Ptr.byteArrayContents arr
          Ptr.fillBytes lPtr 0 sz
          dynArr <- readIORef dynArrRef
          dynInd <- Ptr.readByteArray @Int dynArrIndRef 0
          dynArr' <- if MV.length dynArr == dynInd
            then do
              newArr <- MV.new (2 * dynInd) -- TODO Better growth
              MV.unsafeCopy (MV.unsafeSlice 0 dynInd newArr) dynArr
              pure newArr
            else pure dynArr
          MV.unsafeWrite dynArr' dynInd arr
          pure (dynInd, lPtr)
    pure $! unsafeInlineIO $ Ptr.fillBytes ptr 0 sz0
    !res <- $$(goCompound inner0 [|| allocPtr ||] [|| (\i -> readIORef dynArrRef >>= flip MV.read i >>= pure . Ptr.byteArrayContents) ||]) ptr
    pure $! unsafeInlineIO $ Ptr.touch arr
    pure res
  ||]
  where
    sz0 = compoundSize inner0
    goCompound :: forall a . Codec Compound a a -> Code (Int -> IO (Int, Ptr.Ptr Word8))-> Code (Int -> IO (Ptr.Ptr Word8)) -> Code (Ptr.Ptr Word8 -> FP.Parser Void a)
    goCompound c allocPtr readPtr =
      let (_, alts) = cases c 0
          (fGo, _) = finalGo c alts readPtr
      in [|| \ptr ->
        do
          $$(consumeCompound alts allocPtr) ptr
          !fptr <- FP.Parser $ \fp eob s -> FP.OK# (ForeignPtr eob fp) s
          $$(fGo) fptr ptr
        ||]
    consumeCompound :: [Alt] -> Code (Int -> IO (Int, Ptr.Ptr Word8)) -> Code (Ptr.Ptr Word8 -> FP.Parser Void ())
    consumeCompound xs allocPtr = [||
      let go ptr = do
              tid <- Binary.get @Int8
              case tid of
                0 -> pure ()
                _ -> do
                  $$(TH.unsafeCodeCoerce $ TH.caseE [| tid |] $ alts allocPtr [|| ptr ||] <> matchAny [|| tid ||])
                  go ptr
      in go
      ||]
      where
        matchAny :: Code Int8 -> [TH.Q TH.Match]
        matchAny tid = [TH.match TH.wildP (TH.normalB $ TH.unTypeCode [|| anyString >> anyTag $$(tid) ||]) []]
        alts :: Code (Int -> IO (Int, Ptr.Ptr Word8)) -> Code (Ptr.Ptr Word8) -> [TH.Q TH.Match]
        alts allocPtr ptr = do
          (_,_, tag, _, _) <- nubBy (\(_,_, t1, _, _) (_,_, t2, _, _) -> t1 == t2) xs
          let relevant = filter (\(_, _, t, _, _) -> t == tag) xs
              keySzAlts = do
                (sz, _, _, _,_) <- nubBy (\(s1,_,_,_,_) (s2,_,_,_,_) -> s1 == s2) relevant
                let rel = filter (\(s,_,_,_,_) -> s == sz) relevant
                    consumeKeys :: [Alt] -> Int -> Int -> Code (FP.Parser Void ())
                    consumeKeys [] _ _ = error ""
                    consumeKeys xs rem off
                      | rem >= 8 = consumePrefix xs rem off 8
                      | rem >= 4 = consumePrefix xs rem off 4
                      | rem >= 2 = consumePrefix xs rem off 2
                      | rem == 1 = consumePrefix xs rem off 1
                      | rem == 0 && length xs == 1 = [|| $$(writeTo True (head xs) allocPtr) $$(ptr) ||]
                      | otherwise = error ""
                    consumePrefix :: [Alt] -> Int -> Int -> Int -> Code (FP.Parser Void ())
                    consumePrefix xs prevSz off sz = case sz of
                      -- TODO Clean
                      8 -> goRem [|| Binary.get @Word64 ||]
                      4 -> goRem [|| Binary.get @Word32 ||]
                      2 -> goRem [|| Binary.get @Word16 ||]
                      1 -> goRem [|| Binary.get @Word8 ||]
                      _ -> error ""
                      where
                        -- TODO Does this work?
                        goRem :: Code (FP.Parser Void a) -> Code (FP.Parser Void ())
                        goRem pEl = [|| $$(pEl) >>= \w -> $$(TH.unsafeCodeCoerce $ TH.caseE [| fromIntegral @_ @Word w |] $ prefixAlts <> matchRem [|| prevSz - sz ||]) ||]
                        prefixAlts = do
                          pre <- prefixes
                          let rel = filter (\(_, str, _, _, _) -> maybe True (\(NBTString bs) -> (BS.take sz $ BS.drop off bs) == pre) str) xs
                              preBytes = case sz of
                                8 -> toInteger $ case FP.runParser (Binary.get @Word64) pre of
                                  FP.OK i _ -> i
                                  _ -> error ""
                                4 -> toInteger $ case FP.runParser (Binary.get @Word32) pre of
                                  FP.OK i _ -> i
                                  _ -> error ""
                                2 -> toInteger $ case FP.runParser (Binary.get @Word16) pre of
                                  FP.OK i _ -> i
                                  _ -> error ""
                                1 -> toInteger $ case FP.runParser (Binary.get @Word8) pre of
                                  FP.OK i _ -> i
                                  _ -> error ""
                                _ -> error ""
                              body :: Code (FP.Parser Void ())
                              body = consumeKeys rel (prevSz - sz) (off + sz)
                          pure $ TH.match (TH.litP (TH.IntegerL $ preBytes)) (TH.normalB $ TH.unTypeCode $ body) [] 
                        prefixes = nub $ fmap (\(_, mBs, _, _, _) -> BS.take sz $ BS.drop off . coerce $ fromJust mBs) $ filter (\(_, m, _, _, _) -> isJust m) $ xs

                pure $ TH.match @TH.Q (TH.litP (TH.IntegerL $ toInteger sz)) (TH.normalB $ TH.unTypeCode $ consumeKeys rel sz 0) []
              matchRem :: Code Int -> [TH.Q TH.Match]
              matchRem sz = [TH.match TH.wildP (TH.normalB $ [| FP.takeBs $(TH.unTypeCode sz) >> anyTag tag8 |]) []]
              tag8 = fromIntegral @Int @Int8 tag
              body :: Code (FP.Parser Void ())
              body = [||
                do
                  sz <- Binary.get @Int16
                  $$(TH.unsafeCodeCoerce $ TH.caseE [| sz |] $ keySzAlts <> matchRem [|| fromIntegral sz ||])
                ||]
          pure $ TH.match (TH.litP (TH.IntegerL $ toInteger tag)) (TH.normalB $ TH.unTypeCode body) []
    writeTo :: Bool -> Alt -> Code (Int -> IO (Int, Ptr.Ptr Word8)) -> Code (Ptr.Ptr Word8 -> FP.Parser Void ())
    writeTo confirm (sz', str', t', off, SV c) allocPtr = case c of
      ByteCodec -> writePrim [|| Proxy @Int8 ||]
      ShortCodec -> writePrim [|| Proxy @Int16 ||]
      IntCodec -> writePrim [|| Proxy @Int32 ||]
      LongCodec -> writePrim [|| Proxy @Int64 ||]
      FloatCodec -> writePrim [|| Proxy @Float ||]
      DoubleCodec -> writePrim [|| Proxy @Double ||]
      ByteArrayCodec -> writeStorableVector
      IntArrayCodec -> writeStorableVector
      LongArrayCodec -> writeStorableVector
      StringCodec -> if confirm then [|| \ptr -> parseWriteString (Proxy @True) ptr off ||] else [|| \ptr -> parseWriteString (Proxy @False) ptr off ||]
      ListCodec szEstimate inner -> [|| \ptr ->
        do
          tid <- Binary.get @Int8
          len <- fromIntegral @Int32 @Int <$> Binary.get @Int32
          when (len /= 0 && fromIntegral @_ @Int tid /= cTag) FP.empty

          pure $! unsafeInlineIO $ do
            $$(writeConfirm [|| ptr ||])
            Ptr.poke (coerce $ Ptr.advancePtr ptr dataOffset) (fromIntegral @_ @Int32 len)

          if | len == 0 -> pure () 
             | estSz < len -> $$(consumeList inner off) len ptr
             | otherwise -> do
                (i, newPtr) <- pure $! unsafeInlineIO $ $$(allocPtr) $ len * innerSz
                pure $! unsafeInlineIO $ Ptr.poke (coerce $ Ptr.advancePtr ptr $ dataOffset + 4) i
                $$(consumeList inner 0) len newPtr
        ||]
        where
          innerSz = byteSize inner
          consumeList inner off = [|| \len ptr ->
            let go n ptr
                  | n < len = do
                    $$(writeTo False (0,Just $ NBTString mempty,0,off,SV inner) allocPtr) ptr
                    go (n + 1) $ Ptr.advancePtr ptr innerSz
                  | otherwise = pure ()
            in go 0 ptr
            ||]
          estSz :: Int = case szEstimate of
            Just est -> est
            Nothing -> maxBound
          cTag = codecTag inner
      CompoundCodec inner ->
        let (_, alts) = cases inner 0
        in [|| \ptr ->
          do
            pure $! unsafeInlineIO $ $$(writeConfirm [|| ptr ||])
            $$(consumeCompound alts allocPtr) (if confirm then Ptr.advancePtr ptr 1 else ptr)
          ||]
      DimapCodec _ _ inner -> writeTo confirm (sz', str', t', off, SV inner) allocPtr
      where
        dataOffset :: Int
        dataOffset = if confirm then off + 1 else off
        writeConfirm :: Code (Ptr.Ptr Word8) -> Code (IO ())
        writeConfirm ptr = [|| Ptr.pokeElemOff $$(ptr) off (1 :: Word8)  ||]
        writePrim :: forall a . (Binary.FromBinary a, Ptr.Storable a) => Code (Proxy a) -> Code (Ptr.Ptr Word8 -> FP.Parser Void ())
        writePrim p = if confirm
          then [|| \ptr -> parseWritePrim (Proxy @True) $$(p) ptr off ||]
          else [|| \ptr -> parseWritePrim (Proxy @False) $$(p) ptr off ||]
        writeStorableVector :: Code (Ptr.Ptr Word8 -> FP.Parser Void ())
        writeStorableVector =
          if confirm
            then [|| \ptr -> parseWriteStorableVector (Proxy @True) ptr off ||]
            else [|| \ptr -> parseWriteStorableVector (Proxy @False) ptr off ||]
    finalGo :: forall inp out . Codec Compound inp out -> [Alt] -> Code (Int -> IO (Ptr.Ptr Word8)) -> (Code (Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> FP.Parser Void out), [Alt])
    finalGo (ApCodec ff fa) xs readPtr =
      let (fExp, ys) = finalGo ff xs readPtr
          (aExp, zs) = finalGo fa ys readPtr
      in ([|| \fptr ptr -> $$(fExp) fptr ptr <*> $$(aExp) fptr ptr ||], zs)
    finalGo (PureCodec a) xs _ = ([|| \_ _ -> pure $$(a) ||], xs)
    finalGo (RequiredKeyCodec key inner) ((_,k,t,off,_):xs) readPtr
      | Just key == k && codecTag inner == t = (getValueOff True inner off readPtr, xs)
    finalGo (DimapCodec _ g i) xs readPtr =
      let (iExp, ys) = finalGo i xs readPtr
      in ([|| \fptr ptr -> fmap $$(g) $ $$(iExp) fptr ptr ||], ys)
    finalGo (RequiredKeyCodec _ _) _ _ = error "WUT"
    getValueOff :: forall i o . Bool -> Codec Value i o -> Int -> Code (Int -> IO (Ptr.Ptr Word8)) -> Code (Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> FP.Parser Void o)
    getValueOff confirm cod off readPtr = case cod of
      DimapCodec _ g inner -> [|| \dPtr ptr -> fmap $$(g) $ $$(getValueOff confirm inner off readPtr) dPtr ptr ||] 
      ByteCodec -> readPrim
      ShortCodec -> readPrim
      IntCodec -> readPrim
      LongCodec -> readPrim
      FloatCodec -> readPrim
      DoubleCodec -> readPrim
      ByteArrayCodec -> readStorableVector
      IntArrayCodec -> readStorableVector
      LongArrayCodec -> readStorableVector
      StringCodec -> readString
      ListCodec szEstimate inner ->
        let readList iOff = [|| \dPtr ptr sz -> do
              mar <- pure $! unsafeInlineIO $ MV.new sz
              let go n
                    | n >= sz = pure ()
                    | otherwise = do
                      !el <- $$(getValueOff False inner iOff readPtr) dPtr $ Ptr.advancePtr ptr innerSz
                      pure $! unsafeInlineIO $ MV.unsafeWrite mar n el
                      go (n + 1)
              go 0
              pure $! unsafeInlineIO $ V.unsafeFreeze mar
              ||]
            innerSz = byteSize inner
            szEst :: Int = case szEstimate of
                  Just i -> i
                  Nothing -> maxBound 
        in [|| \dPtr ptr -> do
          $$(checkWritten) ptr
          sz <- pure $! unsafeInlineIO $ Ptr.peek @Int32 . coerce $ Ptr.advancePtr ptr dataOffset
          if | sz == 0 -> pure $ mempty
              | szEst < fromIntegral @_ @Int sz -> $$(readList $ dataOffset + 1) dPtr ptr $ fromIntegral sz
              | otherwise -> do
                lPtr <- pure $! unsafeInlineIO $ Ptr.peek @Int64 (coerce $ Ptr.advancePtr ptr $ dataOffset + 4) >>= $$(readPtr) .fromIntegral
                $$(readList 0) dPtr lPtr $ fromIntegral sz
          ||]
      CompoundCodec inner ->
        let (_, alts) = cases inner dataOffset
        in [|| \dPtr ptr -> do
          $$(checkWritten) ptr
          $$(fst $ finalGo inner alts readPtr) dPtr ptr
          ||]
      where
        readStorableVector :: Ptr.Storable a => Code (Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> FP.Parser Void (S.Vector a))
        readStorableVector = if confirm
          then [|| \dPtr ptr -> parseReadStorableVector (Proxy @True) dPtr ptr off ||]
          else [|| \dPtr ptr -> parseReadStorableVector (Proxy @False) dPtr ptr off ||]
        readString :: Code (Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> FP.Parser Void NBTString)
        readString = if confirm
          then [|| \dPtr ptr -> parseReadString (Proxy @True) dPtr ptr off ||]
          else [|| \dPtr ptr -> parseReadString (Proxy @False) dPtr ptr off ||]
        readPrim :: Ptr.Storable a => Code (Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> FP.Parser Void a)
        readPrim = if confirm then [|| \_ ptr -> parseReadPrim (Proxy @True) ptr off ||] else [|| \_ ptr -> parseReadPrim (Proxy @False) ptr off ||]
        checkWritten :: Code (Ptr.Ptr Word8 -> FP.Parser Void ())
        checkWritten = if confirm then [|| \ptr -> case unsafeInlineIO $ Ptr.peekElemOff ptr off of 1 -> pure (); 0 -> FP.empty ||] else [|| \_ -> pure () ||]
        dataOffset = if confirm then off + 1 else off
    cases :: forall inp out . Codec Compound inp out -> Int -> (Int, [Alt])
    cases (RequiredKeyCodec key cod) off = (off + 1 + byteSize cod, [(BS.length $ coerce key, Just key, codecTag cod, off, SV cod)])
    cases (PureCodec _) off = (off, [])
    cases (ApCodec l r) off =
      let (off', lCases) = cases l off
      in fmap (lCases <>) $ cases r off'
    cases (DimapCodec _ _ inner) off = cases inner off
    byteSize :: forall i o . Codec Value i o -> Int
    byteSize ByteCodec = 1
    byteSize ShortCodec = 2
    byteSize IntCodec = 4
    byteSize LongCodec = 8
    byteSize FloatCodec = 4
    byteSize DoubleCodec = 8
    byteSize ByteArrayCodec = 4 + 8
    byteSize StringCodec = 2 + 8
    byteSize (ListCodec Nothing _) = 4 + 8
    byteSize (ListCodec (Just szEstimate) i) = 4 + max (szEstimate * byteSize i) 8
    byteSize (CompoundCodec i) = compoundSize i
    byteSize IntArrayCodec = 4 + 8
    byteSize LongArrayCodec = 4 + 8
    byteSize (DimapCodec _ _ i) = byteSize i
    compoundSize :: forall inp out . Codec Compound inp out -> Int
    compoundSize (RequiredKeyCodec _ i) = 1 + byteSize i
    compoundSize (PureCodec _) = 0
    compoundSize (ApCodec l r) = compoundSize l + compoundSize r
    compoundSize (DimapCodec _ _ i) = compoundSize i
mkParser _ = error "Not a top level compound" 

data SomeValueCodec = forall inp out . SV (Codec Value inp out)

type Alt = (Int, Maybe NBTString, Int, Int, SomeValueCodec)

anyArray :: Int32 -> FP.Parser Void ()
anyArray elSz = do
  sz <- Binary.get @Int32
  _ <- FP.takeBs (fromIntegral $ sz * elSz)
  pure ()
anyString :: FP.Parser Void ()
anyString = do
  sz <- Binary.get @Int16
  _ <- FP.takeBs $ fromIntegral sz
  pure ()
anyList :: FP.Parser Void ()
anyList = do
  listT <- Binary.get @Int8
  sz <- Binary.get @Int32
  if sz == 0
    then pure ()
    else
      let go n
            | n >= sz = pure ()
            | otherwise = anyTag listT >> go (n + 1)
      in go 0
anyCompound :: FP.Parser Void ()
anyCompound = do
  cTy <- Binary.get @Int8
  anyString
  anyTag cTy
anyTag :: Int8 -> FP.Parser Void ()
anyTag tid = case tid of
    1 -> void $ FP.anyWord8
    2 -> void $ FP.anyWord16
    3 -> void $ FP.anyWord32
    4 -> void $ FP.anyWord64
    5 -> void $ FP.anyWord32
    6 -> void $ FP.anyWord64
    7 -> anyArray 1
    8 -> anyString
    9 -> anyList
    10 -> anyCompound
    11 -> anyArray 4
    12 -> anyArray 8
    _ -> FP.empty

writeConfirm :: Ptr.Ptr Word8 -> Int -> IO ()
writeConfirm ptr off = Ptr.pokeElemOff ptr off (1 :: Word8) 

parseWritePrim :: forall a b . (KnownBool b, Binary.FromBinary a, Ptr.Storable a) => Proxy b -> Proxy a -> Ptr.Ptr Word8 -> Int -> FP.Parser Void ()
parseWritePrim _ _ ptr off = do
  el <- Binary.get @a
  pure $! unsafeInlineIO $ do
    branch @b (writeConfirm ptr off) $ pure ()
    Ptr.poke (coerce $ Ptr.advancePtr ptr $ branch @b (off + 1) off) el
{-# SPECIALIZE parseWritePrim :: Proxy True -> Proxy Int8 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy True -> Proxy Int16 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy True -> Proxy Int32 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy True -> Proxy Int64 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy True -> Proxy Float -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy True -> Proxy Double -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy False -> Proxy Int8 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy False -> Proxy Int16 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy False -> Proxy Int32 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy False -> Proxy Int64 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy False -> Proxy Float -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWritePrim :: Proxy False -> Proxy Double -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}

parseWriteStorableVector :: forall b . KnownBool b => Proxy b -> Ptr.Ptr Word8 -> Int -> FP.Parser Void ()
parseWriteStorableVector _ ptr off = do
  len <- Binary.get @Int32
  FP.Pos i <- FP.getPos
  pure $! unsafeInlineIO $ do
    branch @b (writeConfirm ptr off) $ pure ()
    let dataOffset = branch @b (off + 1) off
    Ptr.poke (coerce $ Ptr.advancePtr ptr dataOffset) len
    Ptr.poke (coerce $ Ptr.advancePtr ptr $ dataOffset + 4) $ fromIntegral @_ @Int64 i
{-# SPECIALIZE parseWriteStorableVector :: Proxy True -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWriteStorableVector :: Proxy False -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}

parseWriteString :: forall b . KnownBool b => Proxy b -> Ptr.Ptr Word8 -> Int -> FP.Parser Void ()
parseWriteString _ ptr off = do
  len <- Binary.get @Int16
  FP.Pos i <- FP.getPos
  pure $! unsafeInlineIO $ do
    branch @b (writeConfirm ptr off) $ pure ()
    let dataOffset = branch @b (off + 1) off
    Ptr.poke (coerce $ Ptr.advancePtr ptr dataOffset) len
    Ptr.poke (coerce $ Ptr.advancePtr ptr $ dataOffset + 2) $ fromIntegral @_ @Int64 i
{-# SPECIALIZE parseWriteString :: Proxy True -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}
{-# SPECIALIZE parseWriteString :: Proxy False -> Ptr.Ptr Word8 -> Int -> FP.Parser Void () #-}

checkWritten :: Ptr.Ptr Word8 -> Int -> FP.Parser Void ()
checkWritten ptr off =
  case unsafeInlineIO (Ptr.peekElemOff ptr off) of
    1 -> pure ()
    _ -> FP.empty

parseReadPrim :: forall a b . (KnownBool b, Ptr.Storable a) => Proxy b -> Ptr.Ptr Word8 -> Int -> FP.Parser Void a
parseReadPrim _ ptr off = do
  branch @b (checkWritten ptr off) (pure ())
  pure $! unsafeInlineIO $ do
    let dataOffset = branch @b (off + 1) off
    Ptr.peekElemOff (coerce ptr) dataOffset
{-# SPECIALIZE parseReadPrim :: Proxy True -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Int8 #-}
{-# SPECIALIZE parseReadPrim :: Proxy True -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Int16 #-}
{-# SPECIALIZE parseReadPrim :: Proxy True -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Int32 #-}
{-# SPECIALIZE parseReadPrim :: Proxy True -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Int64 #-}
{-# SPECIALIZE parseReadPrim :: Proxy True -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Float #-}
{-# SPECIALIZE parseReadPrim :: Proxy True -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Double #-}
{-# SPECIALIZE parseReadPrim :: Proxy False -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Int8 #-}
{-# SPECIALIZE parseReadPrim :: Proxy False -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Int16 #-}
{-# SPECIALIZE parseReadPrim :: Proxy False -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Int32 #-}
{-# SPECIALIZE parseReadPrim :: Proxy False -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Int64 #-}
{-# SPECIALIZE parseReadPrim :: Proxy False -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Float #-}
{-# SPECIALIZE parseReadPrim :: Proxy False -> Ptr.Ptr Word8 -> Int -> FP.Parser Void Double #-}

parseReadStorableVector :: forall a b . (KnownBool b, Ptr.Storable a) => Proxy b -> Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void (S.Vector a)
parseReadStorableVector _ fptr ptr off = do
  branch @b (checkWritten ptr off) (pure ())
  pure $! unsafeInlineIO $ do
    let dataOffset = branch @b (off + 1) off
    sz <- Ptr.peek @Int32 (coerce $ Ptr.advancePtr ptr dataOffset)
    pos <- Ptr.peek @Int64 (coerce $ Ptr.advancePtr ptr $ dataOffset + 4)
    pure $! S.unsafeFromForeignPtr (coerce fptr) (fromIntegral $ 0 - pos) (fromIntegral sz)
{-# SPECIALIZE parseReadStorableVector :: forall a . Ptr.Storable a => Proxy True -> Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void (S.Vector a) #-}
{-# SPECIALIZE parseReadStorableVector :: forall a . Ptr.Storable a => Proxy False -> Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void (S.Vector a) #-}

parseReadString :: forall b . KnownBool b => Proxy b -> Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void NBTString
parseReadString _ dptr ptr off = do
  branch @b (checkWritten ptr off) (pure ())
  pure $! unsafeInlineIO $ do
    let dataOffset = branch @b (off + 1) off
    sz <- Ptr.peek @Int16 (coerce $ Ptr.advancePtr ptr dataOffset)
    pos <- Ptr.peek @Int64 (coerce $ Ptr.advancePtr ptr $ dataOffset + 2)
    pure . NBTString $! BS.BS (Ptr.plusForeignPtr dptr . fromIntegral $ 0 - pos) (fromIntegral sz)
{-# SPECIALIZE parseReadString :: Proxy True -> Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void NBTString #-}
{-# SPECIALIZE parseReadString :: Proxy False -> Ptr.ForeignPtr Word8 -> Ptr.Ptr Word8 -> Int -> FP.Parser Void NBTString #-}

class KnownBool (b :: Bool) where
  branch :: a -> a -> a

instance KnownBool True where
  branch l _ = l
  {-# INLINE branch #-}

instance KnownBool False where
  branch _ r = r
  {-# INLINE branch #-}
