{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module Registry.TH.Biome (
  initRegistry
) where

import qualified Data.Aeson as A
import qualified Data.ByteString as B
import qualified Data.Text as T
import Language.Haskell.TH
import System.Directory

import Registry.BiomeSettings

initRegistry :: Q [Dec]
initRegistry = do
  biomes <- fmap (either error id) <$> runIO $ do
    paths <- listDirectory "./biomes"
    sequence <$> traverse (\path -> fmap (T.dropEnd 5 (T.pack path),) . A.eitherDecodeStrict' @BiomeSettings <$> B.readFile ("./biomes/" <> path)) paths
  biomesExp <- traverse (\(n, b) -> (n,) <$> [| b |]) biomes
  let biomeExps = do
        (name, biome) <- biomesExp
        return $ FunD (mkName $ T.unpack name) [Clause [] (NormalB biome) []]
      allBiomes = FunD 
        (mkName "all_biome_settings")
        [Clause [] (NormalB allBiomeB) []]
      allBiomeB = ListE $ do
        (name, _) <- biomesExp
        return $ TupE [Just . LitE . StringL $ T.unpack name, Just $ VarE (mkName $ T.unpack name)]
      biomeSigs = do
        (name, _) <- biomesExp
        return $ SigD (mkName $ T.unpack name) (ConT $ mkName "BiomeSettings")
      allBiomesSig = SigD (mkName "all_biome_settings") (AppT ListT (AppT (AppT (TupleT 2) (ConT $ mkName "String")) (ConT $ mkName "BiomeSettings")))
  return $ biomeExps <> biomeSigs <> [allBiomes, allBiomesSig]
