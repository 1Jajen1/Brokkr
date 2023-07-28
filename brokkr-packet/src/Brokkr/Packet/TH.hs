{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
module Brokkr.Packet.TH (
  putPacketId
, mkPacketParser
, mkPacketBuilder
) where

import Brokkr.Packet.Binary

import Data.Foldable (foldl', find)

import FlatParse.Basic qualified as Flatparse

import GHC.Exts

import Language.Haskell.TH
import Language.Haskell.TH.Syntax (showName)

import Mason.Builder qualified as Mason

putPacketId :: a -> Mason.Builder
putPacketId !a = put $ VarInt $ fromIntegral (I# (dataToTag# a))
{-# INLINE putPacketId #-}

mkPacketParser :: Name -> [(Name, Q Exp)] -> Q Exp
mkPacketParser tyName manual = reify tyName >>= \case
  TyConI (DataD _ _ _ _ cs _) -> [| with @VarInt $ \n -> $(caseE [| n |] $ (do
      zip [0..] cs >>= \con -> case toCon con of
        (pId, conName, args)
          | Just (_, e) <- find (\(cN,_) -> cN == conName) manual ->
            pure $ match (litP $ IntegerL pId) (normalB e) []
          | otherwise ->
            pure $ match (litP $ IntegerL pId) (normalB $ case args of
              [] -> [| pure $(conE conName) |]
              (_:xs) -> foldl' (\acc _ -> [| $(acc) <*> get |]) [| $(conE conName) <$> get |] xs) []
    ) <> [match wildP (normalB [| Flatparse.err $ InvalidPacketId tyNameStr (fromIntegral n) |]) []]) |]
  _ -> error "Invalid packet type"
  where
    tyNameStr = showName tyName
    toCon (pId, NormalC conName args) = (pId, conName, args)
    toCon (pId, ForallC _ _ c) = toCon (pId, c)
    toCon pId = error $ "Invalid packet constructor " <> show pId

mkPacketBuilder :: Name -> [(Name, Q Match)] -> Q Exp
mkPacketBuilder n manual = reify n >>= \case
  TyConI (DataD _ _ _ _ cs _) -> do
    [| \p -> putPacketId p <> $(caseE [| p |] $
      cs >>= \con -> case toCon con of
        (conName, args)
          | Just (_, e) <- find (\(cN,_) -> cN == conName) manual -> pure e
          | otherwise -> pure $ do
          nms <- traverse (const $ newName "x") args
          match (conP conName (fmap varP nms)) (normalB $ foldl' (\acc nm -> [| $(acc) <> put $(varE nm) |]) [| mempty |] nms) []
      ) |]
  _ -> error "Invalid packet type"
  where
    toCon (NormalC conName args) = (conName, args)
    toCon (ForallC _ _ c) = toCon c
    toCon _ = error "Invalid packet constructor"
