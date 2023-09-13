{-# LANGUAGE TemplateHaskellQuotes #-}
{-# LANGUAGE MagicHash #-}
module Hecs.World.TH (
  makeWorld
) where

import Control.Monad.Base

import Data.Bitfield
import Data.Proxy
import Data.Text qualified as T

import Hecs.Component
import Hecs.Component.Relation (Rel')
import Hecs.Component.Preset (EntityName(..))

import Hecs.Entity.Internal

import Hecs.Filter

import Hecs.Query
import Hecs.System

import Hecs.World.Class
import Hecs.World.Has
import Hecs.World.Internal

import Language.Haskell.TH

internalTypes :: [Name]
internalTypes = [''EntityName, ''SomeQuery, ''System, ''DependsOn, ''Wildcard]

-- | Create and setup a world datatype
--
-- Creates a datatype with the first argument as its name
-- and deriving the necessary instances to function as
-- an ecs world. Specifically an instance of 'WorldClass'
--
-- Then derives 'Has' for all passed components.
--
-- Components statically registered have a few benefits:
-- * Can use type application instead of passing an explicit id
-- * Pre-allocated entity-id's lessen the setup cost
-- * Faster component archetype lookups (TBD)
makeWorld :: String -> [Name] -> Q [Dec]
makeWorld wN names = do
  let wName = mkName wN
      worldImplName = mkName "WorldImpl"
      inbuildNum = length internalTypes
      preAllocComps :: Integer = fromIntegral $ inbuildNum + length names
      wldDec = NewtypeD [] wName [] Nothing (NormalC wName [(Bang NoSourceUnpackedness NoSourceStrictness, ConT worldImplName)]) []
      wCon = pure $ ConT wName
      -- natTy = pure . LitT $ NumTyLit preAllocComps
      processName :: Int -> Name -> Q [Dec]
      processName eid name = do
        reify name >>= \case
          DataConI{} -> [d|
              instance Has $wCon $(conT name) where
                getComponentId _ = ComponentId $ EntityId $ Bitfield eid
                {-# INLINE getComponentId #-}
            |]
          TyConI{} -> do
            xs <- [d|
                instance Has $wCon $(conT name) where
                  getComponentId _ = ComponentId . EntityId $ Bitfield eid -- TODO Maybe pack instead
                  {-# INLINE getComponentId #-}
             |]
            pure xs
          _ -> error "TODO"
  compInstances <- foldr (\(i, n) acc -> acc >>= \xs -> processName i n >>= \ys -> pure $ ys ++ xs) (pure []) $ zip [(inbuildNum + 1)..] names
  otherInstances <- [d|
      deriving newtype instance WorldOps $wCon
    |]
  newWorldD <- [d|
      newWorld :: MonadBase IO m => m $wCon
      newWorld = do
        w <- liftBase $ $(conE wName) <$> (Hecs.World.Internal.new $ Proxy @($(litT $ numTyLit $ preAllocComps)))
        -- $(foldr (\(i :: Int, n) acc' -> do
        --   let nm = T.pack $ show n
        --   [| $(acc') >> Hecs.World.Class.set w (EntityId $ Bitfield i) (EntityName nm) |]
        --   ) [| pure () |] $ zip [1..] $ internalTypes <> names)
        pure w
    |]
  specializedApi <- [d|
      filterDSL :: forall fi . FilterDSL $wCon fi => Filter (HasMain fi) (ToFilter fi)
      filterDSL = Hecs.Filter.filterDSL @($wCon) @fi
    |]
  pure $ wldDec : compInstances ++ otherInstances ++ specializedApi ++ newWorldD
