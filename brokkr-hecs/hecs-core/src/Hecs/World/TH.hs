{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
module Hecs.World.TH (
  makeWorld
) where

import Language.Haskell.TH

import qualified Hecs.World
import Hecs.World.Internal
import Hecs.World.Has
import Hecs.Component
import Hecs.Entity.Internal

import qualified Data.Text as T
import Data.Proxy
import Hecs.Filter hiding (tag)
import Control.Monad.Base
import Data.Coerce
import Data.Bitfield

makeWorld :: String -> [Name] -> Q [Dec]
makeWorld wN names = do
  let wName = mkName wN
      worldImplName = mkName "WorldImpl"
      inbuildNum = length internalTypes
      preAllocComps = fromIntegral $ inbuildNum + length names
      wldDec = NewtypeD [] wName [] Nothing (NormalC wName [(Bang NoSourceUnpackedness NoSourceStrictness, AppT (ConT worldImplName) (LitT $ NumTyLit preAllocComps))]) []
      wCon = pure $ ConT wName
      natTy = pure . LitT $ NumTyLit preAllocComps
      processName :: Int -> Name -> Q [Dec]
      processName eid name = do
        reify name >>= \case
          DataConI{} -> [d|
              instance Has $wCon $(conT name) where
                getComponentId _ = ComponentId $ EntityId $ Bitfield eid
                {-# INLINE getComponentId #-}
              type instance CaseTag $(conT name) a _ = a
            |]
          TyConI{} -> do
            xs <- [d|
                instance Has $wCon $(conT name) where
                  getComponentId _ = ComponentId . EntityId $ Bitfield eid -- TODO Maybe pack instead
                  {-# INLINE getComponentId #-}
             |]
            ys <- reifyInstances (mkName "Component") [ConT name] >>= \case
              [] -> [d|
                  type instance CaseTag $(conT name) a _ = a
                |]
              _ -> let cCon = conT name in [d|
                  type instance CaseTag $cCon _ b = b

                  {-# SPECIALISE syncSetComponent :: WorldImpl $natTy -> EntityId -> ComponentId $cCon -> $cCon -> IO () #-}
                |]
            pure $ xs ++ ys
          _ -> error "TODO"
  compInstances <- foldr (\(i, n) acc -> acc >>= \xs -> processName i n >>= \ys -> pure $ ys ++ xs) (pure []) $ zip [inbuildNum..] names
  otherInstances <- [d|
      deriving newtype instance WorldClass $wCon

      instance (Has $wCon l, Has $wCon r) => Has $wCon (Rel l r) where
        getComponentId _ = mkRelation (getComponentId (Proxy @($wCon))) (getComponentId (Proxy @($wCon)))
        {-# INLINE getComponentId #-}
      instance Has $wCon x => Has $wCon (Tag (x :: k)) where
        getComponentId _ = coerce $ getComponentId @_ @_ @x (Proxy @($wCon))
        {-# INLINE getComponentId #-}
    |]
  newWorldD <- [d|
      newWorld :: MonadBase IO m => m $wCon
      newWorld = do
        w <- $(conE wName) <$> liftBase Hecs.World.Internal.new
        $(foldr (\(i :: Int, n) acc' -> do
          let nm = T.pack $ show n
              acc = [| $(acc') >> Hecs.World.set w (EntityId $ Bitfield i) (EntityName nm) |]
          reify n >>= \case
            DataConI{} -> acc
            TyConI{} -> reifyInstances (mkName "Component") [ConT n] >>= \case
              [] -> acc
              _ -> [| $(acc) >> Hecs.World.set w (EntityId $ Bitfield i) (IsComponent @($(conT n))) |]
            _ -> error "TODO"
          ) [| pure () |] $ zip [1..] $ internalTypes <> names)
        pure w
    |]
  specializedApi <- [d|
      getComponentId :: Has $wCon c => ComponentId c
      getComponentId = Hecs.World.Has.getComponentId (Proxy @($wCon))
      {-# INLINE getComponentId #-}

      component :: forall c . (BranchRel c, Component c, Has $wCon c) => Filter c HasMainId
      component = Hecs.Filter.component (Proxy @($wCon))
      {-# INLINE component #-}

      filterDSL :: forall xs . FilterDSL $wCon (FilterFromList xs) => Filter (FilterFromList xs) (HasMain (FilterFromList xs))
      filterDSL = Hecs.Filter.filterDSL @($wCon) @xs
      {-# INLINE filterDSL #-}

      getColumn :: forall c ty m . (Component c, Has $wCon c, TypedHas ty c, MonadBase IO m) => TypedArchetype ty -> m (Column (ComponentKind c) c)
      getColumn ty = Hecs.Filter.getColumn @c @($wCon) @ty @m ty
      {-# INLINE getColumn #-}
    |]
  pure $ wldDec : compInstances ++ otherInstances ++ specializedApi ++ newWorldD
