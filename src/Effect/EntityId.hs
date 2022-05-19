{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
module Effect.EntityId (
  FreshEntityId
, freshEntityId
, runFreshEntityId
) where

import Effectful

import Entity.Internal
import Effectful.Reader.Static
import Data.Primitive
import Control.Monad.Primitive
import GHC.Exts
import Effectful.Dispatch.Static

type FreshEntityId = Reader EntityIdRef

newtype EntityIdRef = EntityIdRef (MutableByteArray RealWorld)

runFreshEntityId :: IOE :> es => Eff (FreshEntityId : es) a -> Eff es a
runFreshEntityId eff = do
  arr <- liftIO $ newByteArray (sizeOf @Int undefined)
  runReader (EntityIdRef arr) eff

-- TODO For overflow protection (tho we really do not need it I think) we could get away with a Roaring BitSet here, problem is that it would be not nearly
-- as concurrent as this. But does that matter? 
freshEntityId :: FreshEntityId :> es => Eff es EntityId
freshEntityId = do
  EntityIdRef (MutableByteArray ref) <- ask
  newId <- unsafeEff_ $ primitive $ \s -> -- TODO Revise use of unsafeEff?
    case fetchAddIntArray# ref 0# 1# s of
      (# s', i #) -> (# s', I# i #)
  pure $ EntityId newId
{-# INLINE freshEntityId #-}
