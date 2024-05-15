{-# LANGUAGE DerivingStrategies, DeriveAnyClass, RecordWildCards, OverloadedStrings, DataKinds, TemplateHaskell, MagicHash #-}
-- {-# OPTIONS_GHC -ddump-splices #-}
module Main (main) where

import Test.Tasty.Bench

import Control.Exception
import Control.Monad.ST.Strict (runST)

import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as LBS

import Data.Int
import Data.Primitive

import Data.Vector.Storable qualified as S

import Codec.Compression.GZip qualified as GZip

import FlatParse.Basic qualified as FP

import Mason.Builder qualified as B

import Control.DeepSeq
import GHC.Generics (Generic)

import Brokkr.NBT.Codec
import Brokkr.NBT.Internal
import Brokkr.NBT.ByteOrder
import Brokkr.NBT.NBTString.Internal
import Brokkr.NBT.Validate
import Brokkr.NBT.NBTError
import GHC.Word
import GHC.Exts
import GHC.ForeignPtr
import GHC.Float
import Control.Monad.Primitive

import BigTest
import Player

-- import qualified Data.Serialize as Serialize
-- import qualified Data.Nbt as NBT2

-- TODO Make config option
includeCmp :: Bool
includeCmp = True

data Env = Env {
    envBs      :: !BS.ByteString
  , envNBT     :: !NBT
  -- , envNBT2    :: !NBT2.Nbt'
  , envBigTest :: !BigTest
  , envPlayer :: !Player
  }
  deriving stock Generic
  deriving anyclass NFData

setupEnv :: String -> IO Env
setupEnv name = do
  initBs <- BS.readFile ("test/NBT/" ++ name)
  envBs <- handle (\(_ :: SomeException) -> pure initBs) . evaluate . LBS.toStrict . GZip.decompress $ LBS.fromStrict initBs
  let envNBT = case FP.runParser parseNBT envBs of
        FP.OK res "" -> res
        _ -> error "Failed to parse NBT"
  
  bigTestBs0 <- BS.readFile "test/NBT/bigtest.nbt"
  bigTestBs <- handle (\(_ :: SomeException) -> pure initBs) . evaluate . LBS.toStrict . GZip.decompress $ LBS.fromStrict bigTestBs0

  let envBigTest = parseBigTest bigTestBs
      -- envNBT2 = case Serialize.decode envBs of
      --   Left _ -> error "Failed to parse nbt using named-binary-tag"
      --   Right n -> n
  
  playerBs0 <- BS.readFile "test/NBT/complex_player.dat"
  playerBs <- handle (\(_ :: SomeException) -> pure initBs) . evaluate . LBS.toStrict . GZip.decompress $ LBS.fromStrict playerBs0

  let envPlayer = parsePlayer playerBs
      -- envNBT2 = case Serialize.decode envBs of
      --   Left _ -> error "Failed to parse nbt using named-binary-tag"
      --   Right n -> n

  return $ Env{..}

benchFile :: String -> Benchmark
benchFile name =
  env (setupEnv name) $ \ ~Env{..} -> 
  bgroup name $
    [ bench "decode (brokkr | validate)" $ nf validateBsNBT envBs
    , bench "decode (brokkr | no sorting | no mod utf8)"  $ nf parseBsNBTUnsortedNoModUtf8 envBs
    , bench "decode (brokkr | no mod utf8)"  $ nf parseBsNBTNoModUtf8 envBs
    , bench "decode (brokkr | no sorting)"  $ nf parseBsNBTUnsorted envBs
    , bench "decode (brokkr)"  $ nf parseBsNBT envBs
    , bench "encode (brokkr)"  $ nf encodeNBT envNBT 
    ]
      -- <> (if includeCmp then
      --   [ bench "decode (named-binary-tag)"  $ nf (Serialize.decode @NBT2.Nbt') envBs
      --   -- , bench "encode (named-binary-tag)"  $ nf (Serialize.runPut . Serialize.put) envNBT2 
      --   ] else [])
      <> (if name == "bigtest.nbt" then
        [ bench "decode (brokkr) (schema)" $ nf parseBigTest envBs
        -- , bench "encode (brokkr) (schema)" $ nf encodeBigTest envBigTest
        ]
      else [])
      <> (if name == "complex_player.dat" then
        [ bench "decode (brokkr) (schema)" $ nf parsePlayer envBs
        -- , bench "encode (brokkr) (schema)" $ nf encodePlayer envPlayer
        ]
      else [])

benchByteSwap :: (Num a, S.Storable a) => String -> (S.Vector a -> S.Vector b) -> Benchmark
benchByteSwap n f = bgroup n
  [ env (evaluate . force $ S.generate 4 gen) $ \ ~v ->
    bench "4" $ nf f v
  , env (evaluate . force $ S.generate 16 gen) $ \ ~v ->
    bench "16" $ nf f v
  , env (evaluate . force $ S.generate 128 gen) $ \ ~v ->
    bench "128" $ nf f v
  , env (evaluate . force $ S.generate 400 gen) $ \ ~v ->
    bench "400" $ nf f v
  , env (evaluate . force $ S.generate 4096 gen) $ \ ~v ->
    bench "4096" $ nf f v
  , env (evaluate . force $ S.generate 8000 gen) $ \ ~v ->
    bench "8000" $ nf f v
  , env (evaluate . force $ S.generate 1048576 gen) $ \ ~v ->
    bench "1048576" $ nf f v
  , env (evaluate . force $ S.generate 1500000 gen) $ \ ~v ->
    bench "1500000" $ nf f v
  ]
  where gen i = fromIntegral $ (i * i * 255 + i * 7) `mod` 100

mkRecList :: (NBT, BS.ByteString)
mkRecList =
  let hugeNbt = NBT "" $ nestedList 1000000
      smallArrEmpty = runST $ newSmallArray 0 (error "SmallArr empty") >>= unsafeFreezeSmallArray
      smallArrSingleton x = runST $ newSmallArray 1 x >>= unsafeFreezeSmallArray
      nestedList :: Int -> Tag
      nestedList 0 = TagList emptySmallArray
      nestedList !n = TagList $ smallArrSingleton $ nestedList (n - 1)
      encodedBs = encodeNBT hugeNbt
  in (hugeNbt, encodedBs)

benchRecList :: Benchmark
benchRecList = env (evaluate . force $ mkRecList) $ \ ~(hugeNbt, hugeBs) -> bgroup "huge recursive list"
  [ bench "decode" $ nf parseBsNBT hugeBs
  , bench "encode" $ nf encodeNBT hugeNbt
  ]

main :: IO ()
main = defaultMain [
    bgroup "nbt files" [
      benchFile "bigtest.nbt"
    , benchFile "complex_player.dat"
    , benchFile "hello_world.nbt"
    , benchFile "inttest3.nbt"
    , benchFile "inttest16.nbt"
    , benchFile "inttest1023.nbt"
    , benchFile "level.dat"
    , benchFile "simple_player.dat"
    , benchFile "realworld.nbt"
    ]
  , bgroup "Byteswapping" [
      -- Benchmark byteswap in place
      benchByteSwap @Int32 "bswap32 (unsafe)" unsafeArrSwapBE
      -- Benchmark copying and byteswapping
      -- First just the copy. This allocates a new vector and copies it
    , benchByteSwap @Int32 @Int32 "memcopy32" (\v -> runST $ S.thaw v >>= S.unsafeFreeze)
      -- Next benchmark copy and byteswap fused. This allocates a new vector
      -- and then copies and byteswaps at the same time
    , benchByteSwap @Int32 "bswap32" arrSwapBE
      -- Same for 64 bit numbers
    , benchByteSwap @Int64 "bswap64 (unsafe)" unsafeArrSwapBE
    , benchByteSwap @Int64 @Int64 "memcopy64" (\v -> runST $ S.thaw v >>= S.unsafeFreeze)
    , benchByteSwap @Int64 "bswap64" arrSwapBE
    ]
  , benchRecList
  -- download https://archives.haskell.org/projects.haskell.org/text/text-testdata.tar.bz2 and extract to benchmark/data. Then remove the intermediate folders
  -- to run these tests
  -- , bgroup "modified utf8" [
  --     benchModUtf8 "ascii.txt"
  --   , benchModUtf8 "bmp.txt"
  --   , benchModUtf8 "japanese.txt"
  --   , benchModUtf8 "korean.txt"
  --   ]
  -- TODO Add modified-utf-8 conversion benchmarks
  ]

benchModUtf8 :: String -> Benchmark
benchModUtf8 name = env (BS.readFile ("benchmark/data/" ++ name) >>= evaluate . force) $ \ ~bs -> bgroup name
  [ bench "isValid" $ nf isValidModifiedUtf8 bs
  , bench "isValidSimd" $ nf isValidModifiedUtf8SIMD bs
  , bench "isValid (64b)" $ nf isValidModifiedUtf8 (BS.take 64 bs)
  , bench "isValidSimd (64b)" $ nf isValidModifiedUtf8SIMD (BS.take 64 bs)
  , bench "isValid (32b)" $ nf isValidModifiedUtf8 (BS.take 32 bs)
  , bench "isValidSimd (32b)" $ nf isValidModifiedUtf8SIMD (BS.take 32 bs)
  , bench "isValid (16b)" $ nf isValidModifiedUtf8 (BS.take 16 bs)
  , bench "isValidSimd (16b)" $ nf isValidModifiedUtf8SIMD (BS.take 16 bs)
  , bench "isValid (8b)" $ nf isValidModifiedUtf8 (BS.take 8 bs)
  , bench "isValidSimd (8b)" $ nf isValidModifiedUtf8SIMD (BS.take 8 bs)
  , bench "isValid (4b)" $ nf isValidModifiedUtf8 (BS.take 4 bs)
  , bench "isValidSimd (4b)" $ nf isValidModifiedUtf8SIMD (BS.take 4 bs)
  ]

validateBsNBT :: BS.ByteString -> ()
validateBsNBT !bs = case FP.runParser skipNBT bs of
  FP.OK res "" -> ()
  _ -> error "Failed to parse NBT"

parseBsNBT :: BS.ByteString -> NBT
parseBsNBT !bs = case FP.runParser parseNBT bs of
  FP.OK res "" -> res
  _ -> error "Failed to parse NBT"

encodeNBT :: NBT -> BS.ByteString
encodeNBT !nbt = B.toStrictByteString (putNBT nbt)

parseBigTest :: BS.ByteString -> BigTest
parseBigTest bs = case FP.runParser
  $$(genParser bigTestCodec) bs of
  FP.OK res "" -> res
  FP.Err e -> error $ show e
  _ -> error "Failed to parse NBT"

-- encodeBigTest :: BigTest -> BS.ByteString
-- encodeBigTest bt = B.toStrictByteString ($(genBuilder bigTestCodec) bt)

parsePlayer :: BS.ByteString -> Player
parsePlayer bs = case FP.runParser
  $$(genParser playerCodec) bs of
  FP.OK res "" -> res
  FP.Err e -> error $ show e
  _ -> error "Failed to parse NBT"

-- encodePlayer :: Player -> BS.ByteString
-- encodePlayer p = B.toStrictByteString ($(genBuilder playerCodec) p)

-- Variants:

-- Unsorted
parseBsNBTUnsorted :: BS.ByteString -> NBT
parseBsNBTUnsorted !bs = case FP.runParser parseNBTUnsorted bs of
  FP.OK res "" -> res
  _ -> error "Failed to parse NBT"

parseNBTUnsorted :: FP.ParserT st NBTError NBT
parseNBTUnsorted = do
  tagId <- FP.anyWord8
  name <- parseNBTString
  tag <- parseTagUnsorted tagId
  pure $ NBT name tag

-- | Parse a single nbt tag given a tag type
parseTagUnsorted :: Word8 -> FP.ParserT st NBTError Tag
{-# INLINE parseTagUnsorted #-}
-- Primitive types
parseTagUnsorted  1 = TagByte  <$> FP.anyInt8
parseTagUnsorted  2 = TagShort <$> FP.anyInt16be
parseTagUnsorted  3 = TagInt   <$> FP.anyInt32be
parseTagUnsorted  4 = TagLong  <$> FP.anyInt64be
-- Floating point
parseTagUnsorted  5 = TagFloat  . castWord32ToFloat  <$> FP.anyWord32be
parseTagUnsorted  6 = TagDouble . castWord64ToDouble <$> FP.anyWord64be
-- Primitive array types
parseTagUnsorted  7 = TagByteArray <$> takeArray
parseTagUnsorted 11 = TagIntArray  <$> takeArray
parseTagUnsorted 12 = TagLongArray <$> takeArray
-- Strings
parseTagUnsorted  8 = TagString    <$> parseNBTString
-- lists
parseTagUnsorted  9 = parseList
  where
    parseList = do
      W8# tagId <- FP.anyWord8
      len@(I# len#) <- fromIntegral <$> FP.anyInt32be
      localST $ do
        -- TODO Check against stupidly large lists
        SmallMutableArray mut <- FP.liftST $ newSmallArray len $ error "parse nbt list"
        go mut 0# len# tagId
        arr <- FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut)
        pure $ TagList arr
    {-# INLINE parseList #-}
    go _   _ 0# _   = pure ()
    go mut i n  tid = do
      !el <- parseTagUnsorted (W8# tid)
      FP.liftST $ writeSmallArray (SmallMutableArray mut) (I# i) el
      go mut (i +# 1#) (n -# 1#) tid

-- compounds
parseTagUnsorted 10 = parseCompound
  where
    parseCompound :: FP.ParserT st NBTError Tag
    {-# INLINE parseCompound #-}
    parseCompound = localST $ do
      fpco <- FP.ParserT $ \fpco _ curr st -> FP.OK# st fpco curr
      mut <- FP.liftST $ newCompound 4
      go fpco mut 0
      where
        go :: ForeignPtrContents -> MutCompound s -> Int -> FP.ParserST s NBTError Tag
        go !fpco !mut !sz = do
          tagId <- FP.anyWord8
          if tagId == 0
            then do
              comp <- FP.liftST $ unsafeFreezeCompound mut sz $ OneKeyFP fpco
              pure $ TagCompound comp
            else withNBTString $ \name -> do
              tag <- parseTagUnsorted tagId
              -- grow
              ensureCompound mut sz $ \mut' -> do
                sz' <- FP.liftST $ insertUnsorted fpco mut' sz name tag
                go fpco mut' sz'
        insertUnsorted _ !mut !sz !name !tag = writeToCompound mut sz name tag sz >> pure (sz + 1)

-- anything else fails
-- Note: We don't parse TagEnd, parseCompound and parseList
-- parse it separately, so a TagEnd here would be invalid nbt
parseTagUnsorted _ = FP.empty

-- Unsorted

parseBsNBTUnsortedNoModUtf8 :: BS.ByteString -> NBT
parseBsNBTUnsortedNoModUtf8 !bs = case FP.runParser parseNBTUnsortedNoModUtf8 bs of
  FP.OK res "" -> res
  _ -> error "Failed to parse NBT"

parseNBTUnsortedNoModUtf8 :: FP.ParserT st NBTError NBT
parseNBTUnsortedNoModUtf8 = do
  tagId <- FP.anyWord8
  name <- unsafeParseNBTString
  tag <- parseTagUnsortedNoModUtf8 tagId
  pure $ NBT name tag

-- | Parse a single nbt tag given a tag type
parseTagUnsortedNoModUtf8 :: Word8 -> FP.ParserT st NBTError Tag
{-# INLINE parseTagUnsortedNoModUtf8 #-}
-- Primitive types
parseTagUnsortedNoModUtf8  1 = TagByte  <$> FP.anyInt8
parseTagUnsortedNoModUtf8  2 = TagShort <$> FP.anyInt16be
parseTagUnsortedNoModUtf8  3 = TagInt   <$> FP.anyInt32be
parseTagUnsortedNoModUtf8  4 = TagLong  <$> FP.anyInt64be
-- Floating point
parseTagUnsortedNoModUtf8  5 = TagFloat  . castWord32ToFloat  <$> FP.anyWord32be
parseTagUnsortedNoModUtf8  6 = TagDouble . castWord64ToDouble <$> FP.anyWord64be
-- Primitive array types
parseTagUnsortedNoModUtf8  7 = TagByteArray <$> takeArray
parseTagUnsortedNoModUtf8 11 = TagIntArray  <$> takeArray
parseTagUnsortedNoModUtf8 12 = TagLongArray <$> takeArray
-- Strings
parseTagUnsortedNoModUtf8  8 = TagString    <$> unsafeParseNBTString
-- lists
parseTagUnsortedNoModUtf8  9 = parseList
  where
    parseList = do
      W8# tagId <- FP.anyWord8
      len@(I# len#) <- fromIntegral <$> FP.anyInt32be
      localST $ do
        -- TODO Check against stupidly large lists
        SmallMutableArray mut <- FP.liftST $ newSmallArray len $ error "parse nbt list"
        go mut 0# len# tagId
        arr <- FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut)
        pure $ TagList arr
    {-# INLINE parseList #-}
    go _   _ 0# _   = pure ()
    go mut i n  tid = do
      !el <- parseTagUnsortedNoModUtf8 (W8# tid)
      FP.liftST $ writeSmallArray (SmallMutableArray mut) (I# i) el
      go mut (i +# 1#) (n -# 1#) tid

-- compounds
parseTagUnsortedNoModUtf8 10 = parseCompound
  where
    parseCompound :: FP.ParserT st NBTError Tag
    {-# INLINE parseCompound #-}
    parseCompound = localST $ do
      fpco <- FP.ParserT $ \fpco _ curr st -> FP.OK# st fpco curr
      mut <- FP.liftST $ newCompound 4
      go fpco mut 0
      where
        go :: ForeignPtrContents -> MutCompound s -> Int -> FP.ParserST s NBTError Tag
        go !fpco !mut !sz = do
          tagId <- FP.anyWord8
          if tagId == 0
            then do
              comp <- FP.liftST $ unsafeFreezeCompound mut sz $ OneKeyFP fpco
              pure $ TagCompound comp
            else unsafeWithNBTString $ \name -> do
              tag <- parseTagUnsortedNoModUtf8 tagId
              -- grow
              ensureCompound mut sz $ \mut' -> do
                sz' <- FP.liftST $ insertUnsorted fpco mut' sz name tag
                go fpco mut' sz'
        insertUnsorted _ !mut !sz !name !tag = writeToCompound mut sz name tag sz >> pure (sz + 1)

-- anything else fails
-- Note: We don't parse TagEnd, parseCompound and parseList
-- parse it separately, so a TagEnd here would be invalid nbt
parseTagUnsortedNoModUtf8 _ = FP.empty

-- No modified utf8 validation

parseBsNBTNoModUtf8 :: BS.ByteString -> NBT
parseBsNBTNoModUtf8 !bs = case FP.runParser parseNBTNoModUtf8 bs of
  FP.OK res "" -> res
  _ -> error "Failed to parse NBT"

parseNBTNoModUtf8 :: FP.ParserT st NBTError NBT
parseNBTNoModUtf8 = do
  tagId <- FP.anyWord8
  name <- unsafeParseNBTString
  tag <- parseTagNoModUtf8 tagId
  pure $ NBT name tag

-- | Parse a single nbt tag given a tag type
parseTagNoModUtf8 :: Word8 -> FP.ParserT st NBTError Tag
{-# INLINE parseTagNoModUtf8 #-}
-- Primitive types
parseTagNoModUtf8  1 = TagByte  <$> FP.anyInt8
parseTagNoModUtf8  2 = TagShort <$> FP.anyInt16be
parseTagNoModUtf8  3 = TagInt   <$> FP.anyInt32be
parseTagNoModUtf8  4 = TagLong  <$> FP.anyInt64be
-- Floating point
parseTagNoModUtf8  5 = TagFloat  . castWord32ToFloat  <$> FP.anyWord32be
parseTagNoModUtf8  6 = TagDouble . castWord64ToDouble <$> FP.anyWord64be
-- Primitive array types
parseTagNoModUtf8  7 = TagByteArray <$> takeArray
parseTagNoModUtf8 11 = TagIntArray  <$> takeArray
parseTagNoModUtf8 12 = TagLongArray <$> takeArray
-- Strings
parseTagNoModUtf8  8 = TagString    <$> unsafeParseNBTString
-- lists
parseTagNoModUtf8  9 = parseList
  where
    parseList = do
      W8# tagId <- FP.anyWord8
      len@(I# len#) <- fromIntegral <$> FP.anyInt32be
      localST $ do
        -- TODO Check against stupidly large lists
        SmallMutableArray mut <- FP.liftST $ newSmallArray len $ error "parse nbt list"
        go mut 0# len# tagId
        arr <- FP.liftST $ unsafeFreezeSmallArray (SmallMutableArray mut)
        pure $ TagList arr
    {-# INLINE parseList #-}
    go _   _ 0# _   = pure ()
    go mut i n  tid = do
      !el <- parseTagNoModUtf8 (W8# tid)
      FP.liftST $ writeSmallArray (SmallMutableArray mut) (I# i) el
      go mut (i +# 1#) (n -# 1#) tid

-- compounds
parseTagNoModUtf8 10 = parseCompound
  where
    parseCompound :: FP.ParserT st NBTError Tag
    {-# INLINE parseCompound #-}
    parseCompound = localST $ do
      fpco <- FP.ParserT $ \fpco _ curr st -> FP.OK# st fpco curr
      mut <- FP.liftST $ newCompound 4
      go fpco mut 0
      where
        go :: ForeignPtrContents -> MutCompound s -> Int -> FP.ParserST s NBTError Tag
        go !fpco !mut !sz = do
          tagId <- FP.anyWord8
          if tagId == 0
            then do
              comp <- FP.liftST $ unsafeFreezeCompound mut sz $ OneKeyFP fpco
              pure $ TagCompound comp
            else unsafeWithNBTString $ \name -> do
              tag <- parseTagNoModUtf8 tagId
              -- grow
              ensureCompound mut sz $ \mut' -> do
                sz' <- FP.liftST $ insertSortedLinear fpco mut' sz name tag
                go fpco mut' sz'
        insertSortedLinear :: PrimMonad m => ForeignPtrContents -> MutCompound (PrimState m) -> Int -> NBTString -> Tag -> m Int
        {-# INLINE insertSortedLinear #-}
        insertSortedLinear !fpco !mut !sz !name !tag = go_insert 0
          where
            go_insert !n
              | n >= sz   = writeToCompound mut sz name tag sz >> pure (sz + 1)
              | otherwise = do
                k' <- readCompoundKey mut n fpco
                case compare name k' of
                  EQ -> pure sz
                  GT -> go_insert (n + 1)
                  LT -> do
                    moveCompound mut n sz
                    writeToCompound mut sz name tag n
                    pure (sz + 1)
-- anything else fails
-- Note: We don't parse TagEnd, parseCompound and parseList
-- parse it separately, so a TagEnd here would be invalid nbt
parseTagNoModUtf8 _ = FP.empty
