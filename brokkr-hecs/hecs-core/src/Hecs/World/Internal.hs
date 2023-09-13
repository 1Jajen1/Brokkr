{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE OverloadedRecordDot #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
{-# OPTIONS_GHC -ddump-cmm #-}
-- {-# OPTIONS_GHC -ddump-stg #-}
module Hecs.World.Internal (
  WorldImpl(..)
, new
, newEntity, freeEntity
, getWithId, addWithId, setWithId, removeWithId
, enableWithId, disableWithId
, runFilter
) where

import Brokkr.HashTable qualified as HT

import Control.Monad (replicateM_, void, when, join)
import Control.Monad.Base
import Control.Monad.Primitive (primitive)
import Control.Monad.Trans.Control

import Data.Bitfield
import Data.Coerce
import Data.Primitive hiding (Array)
import Data.Proxy
import Data.Word

import Foreign.Storable (Storable)

import GHC.Exts
import GHC.IO (IO(..))
import GHC.TypeLits

import Hecs.Array (Array)
import Hecs.Array qualified as Array

import Hecs.Archetype.Internal (Archetype(..), Archetype#, ArchetypeEdge(..), ArchetypeMove(..), SizesAndAlignment)
import Hecs.Archetype.Internal qualified as Archetype
import Hecs.Archetype.Type (ArchetypeType(..))
import Hecs.Archetype.Type qualified as Archetype.Type

import Hecs.Component.Column qualified as Filter
import Hecs.Component.Internal
import Hecs.Component.Preset
import Hecs.Component.Relation

import Hecs.Entity.Internal (EntitySparseSet, EntityId(..))
import Hecs.Entity.Internal qualified as Entity

import Hecs.Filter.Internal (Filter)
import Hecs.Filter.Column qualified as Filter
import Hecs.Filter.Internal qualified as Filter
import Hecs.Filter.DSL qualified as Filter

import Hecs.Query.Internal (Query(..), SomeQuery(..))
import Hecs.Query.Internal qualified as Query

import Hecs.System.Internal

import Hecs.World.Class qualified as Class
import Hecs.Fold qualified as Fold
import Hecs.World.Has

import System.IO

-- TODO Allow unlifted values in hashtables

-- TODO
-- - Slowly move towards making more stuff entities
--    - Components could be given a query array
--    - Systems

data WorldImpl
  = WorldImpl {
    entityIndex    :: {-# UNPACK #-} !(EntitySparseSet EntityRecord)
  , archetypeIndex ::                !(HT.HashTable' HT.Boxed    HT.Boxed RealWorld IdArchetypeType Archetype)
  , componentIndex :: {-# UNPACK #-} !(HT.HashTable' HT.Storable HT.Boxed RealWorld IdInt (Array RealWorld ComponentRecord))
  , emptyArchetype :: {-# UNPACK #-} !Archetype
  , queryIndex     :: {-# UNPACK #-} !(HT.HashTable' HT.Storable HT.Boxed RealWorld IdInt (Array RealWorld SomeQuery))
  , systemQuery    :: {-# UNPACK #-} !(Query (Filter.And System SomeQuery))
  }

data EntityRecord
  = EntityRecord
    {-# UNPACK #-} !Archetype -- Which table
    {-# UNPACK #-} !Int       -- Which row

data ComponentRecord
  = ComponentRecord
    {-# UNPACK #-} !Archetype -- Which table
    {-# UNPACK #-} !Int       -- Which column

newtype IdInt = IdInt Int
  deriving newtype (Eq, Storable)

instance HT.Hash IdInt where hash (IdInt x) = HT.HashFn (const x)

newtype IdArchetypeType = IdArchetypeType ArchetypeType
  deriving newtype Eq

instance HT.Hash IdArchetypeType where hash (IdArchetypeType (ArchetypeType hs _ _ _ _)) = HT.HashFn (const hs)

wildcard :: ComponentId Wildcard
wildcard = getComponentId (Proxy @Int) -- TODO Is anything fine here?

new :: forall n . KnownNat n => Proxy n -> IO WorldImpl
{-# INLINABLE new #-}
new _ = do
  entityIndex <- Entity.new

  componentIndex <- HT.new nrStatic 0 0.75
  archetypeIndex <- HT.new 8 0 0.75
  emptyArchetype@(Archetype eAty#) <- Archetype.newEmpty

  HT.insert archetypeIndex (coerce $ Archetype.ty eAty#) emptyArchetype

  queryIndex <- HT.new 8 0 0.75

  -- Preallocate a number of ids. This is a setup cost, but enables statically known ids for a list of components
  -- If this is ever too slow, this can be made much more efficient
  replicateM_ (nrStatic + 1) $ do
    eid <- Entity.allocateEntityId entityIndex
    row <- Archetype.addEntity emptyArchetype eid
    Entity.insert entityIndex eid (EntityRecord emptyArchetype row)

  systemQuery <- query_ componentIndex queryIndex emptyArchetype $ inline (Filter.filterDSL @WorldImpl @('[System, SomeQuery]))

  pure WorldImpl{..}
  where
    nrStatic = fromIntegral $ natVal (Proxy @n) 

newEntity :: WorldImpl -> IO EntityId
newEntity WorldImpl{entityIndex, emptyArchetype} = do
  entity <- Entity.allocateEntityId entityIndex
  row <- Archetype.addEntity emptyArchetype entity
  Entity.insert entityIndex entity $ EntityRecord emptyArchetype row
  pure entity

freeEntity :: WorldImpl -> EntityId -> IO ()
freeEntity WorldImpl{entityIndex, componentIndex} entity = do
  Entity.deAllocateEntityId entityIndex entity >>= \case
    Nothing -> pure ()
    Just (EntityRecord aty row) -> do
      movedEntity <- Archetype.removeEntity aty row
      Entity.insert entityIndex movedEntity $ EntityRecord aty row
      HT.lookup componentIndex (coerce entity)
        (\arr -> Array.iterate_ arr $ \_ (ComponentRecord compAty col) -> do
          -- TODO Remove the column from the archetype
          pure ()
          )
        $ pure ()

hasWithId :: forall c r . Component c => WorldImpl -> EntityId -> ComponentId c -> IO Bool
{-# INLINE hasWithId #-}
hasWithId WorldImpl{entityIndex} !entity !componentId = do
  Entity.lookup entityIndex entity
    (\(EntityRecord aty row) -> do
      Archetype.lookupColumn aty componentId
        (\column -> Archetype.isComponentEnabled (Proxy @(ComponentKindFor c)) aty row column)
        $ pure False)
    $ error "Entity is not in the entity index!"

getWithId :: forall c r . (Component c, Coercible (ComponentValueFor c) c) => WorldImpl -> EntityId -> ComponentId c -> (c -> IO r) -> IO r -> IO r
{-# INLINE getWithId #-}
getWithId WorldImpl{entityIndex} !entity !componentId onSucc onFail = do
  Entity.lookup entityIndex entity
    (\(EntityRecord aty row) -> do
      Archetype.lookupColumn aty componentId
        (\column -> Archetype.isComponentEnabled (Proxy @(ComponentKindFor c)) aty row column >>= \case
          True  -> Archetype.readComponent aty row column onSucc
          False -> onFail
          )
        onFail)
    $ error "Entity is not in the entity index!"

data ArchetypeRecord
  = ArchetypeRecord
    !Archetype
    {-# UNPACK #-} !Int -- row
    {-# UNPACK #-} !Int -- column

addWithId :: forall c r . Component c => WorldImpl -> EntityId -> ComponentId c -> (ArchetypeRecord -> IO r) -> IO r
{-# INLINE addWithId #-}
addWithId w@WorldImpl{entityIndex} !entity !componentId cont = do
  -- where is the entity right now?
  Entity.lookup entityIndex entity
    (\(EntityRecord aty row) -> do
      -- does this Archetype already have our component?
      Archetype.lookupColumn aty componentId
        (\column -> cont $ ArchetypeRecord aty row column)
        $ addSlow w aty row entity componentId >>= cont)
    $ error "Entity is not in the entity index!"

-- This is split so that we can inline addWithId's fast path while keeping this slow path out of line
-- Specifically setWithId needs to call addWithId first to ensure the entity has the component and get the
-- row and column to write to. This significantly helps performance there!
addSlow :: forall c ck . Component c => WorldImpl -> Archetype -> Int -> EntityId -> ComponentId c -> IO ArchetypeRecord
addSlow WorldImpl{entityIndex, archetypeIndex, componentIndex, queryIndex} aty@(Archetype aty#) !row !entity !componentId =
  -- Need to perform a table move
  Archetype.getEdge aty componentId
    (\(ArchetypeEdge dstAty column) -> do
      -- we have a destination table edge already, move along there
      ArchetypeMove newRow movedEntity <- Archetype.moveTo aty dstAty row
      Entity.insert entityIndex movedEntity $ EntityRecord aty row
      Entity.insert entityIndex entity $ EntityRecord dstAty newRow
      pure $ ArchetypeRecord dstAty newRow column
      )
    -- No table edge
    $ do
      let fromTy = Archetype.ty aty#
          newTy  = Archetype.Type.addComponentType fromTy componentId
      -- Do we already have this archetype?
      HT.lookup archetypeIndex (coerce newTy)
        (\dstAty ->
          -- Yes, set the edge and do the move
          Archetype.lookupColumn dstAty componentId
            (\column -> do
              Archetype.setEdge aty componentId $ ArchetypeEdge dstAty column
              ArchetypeMove newRow movedEntity <- Archetype.moveTo aty dstAty row
              Entity.insert entityIndex movedEntity $ EntityRecord aty row
              Entity.insert entityIndex entity $ EntityRecord dstAty newRow
              pure $ ArchetypeRecord dstAty newRow column
              )
            (error "Archetype did not have component, but it's in the type!")
          )
        -- No, create new
        $ do
          let newSizesAndAlign :: SizesAndAlignment = backing @_ @c
                (Archetype.sizesAndAlignment aty)
                (Archetype.addComponentSizes (Proxy @(ComponentValueFor c)) $ Archetype.sizesAndAlignment aty)
                (Archetype.sizesAndAlignment aty)
          dstAty <- Archetype.newWithType newTy newSizesAndAlign
          HT.insert archetypeIndex (coerce newTy) dstAty

          Archetype.Type.iterateComponents newTy $ \cid column -> do
            -- TODO This needs to call the queries with wildcards in their main id!
            HT.lookup queryIndex (coerce cid)
              (\arr -> Array.iterate_ arr $ \_ q -> Query.addArchetype q dstAty)
              $ pure ()

            let insertToTable c = do
                  arr <- HT.lookup componentIndex (coerce c)
                          pure
                          $ do
                            arr <- Array.new 4
                            HT.insert componentIndex (coerce c) arr
                            pure arr
                  sz <- Array.size arr
                  if sz == 0
                    then Array.writeBack arr $ ComponentRecord dstAty column
                    else do
                      ComponentRecord a _ <- Array.read arr (sz - 1)
                      when (a /= dstAty) $ Array.writeBack arr $ ComponentRecord dstAty column

            when ((coerce @_ @(Bitfield Int Entity.Entity) cid).tag.isRelation) $ do
              let (fst,snd) = unwrapRelation (coerce cid)
              insertToTable (mkRelation wildcard wildcard)
              insertToTable (mkRelation fst wildcard)
              insertToTable (mkRelation wildcard snd)

            insertToTable cid

          Archetype.lookupColumn dstAty componentId
            (\column -> do
              Archetype.setEdge aty componentId $ ArchetypeEdge dstAty column
              ArchetypeMove newRow movedEntity <- Archetype.moveTo aty dstAty row
              Entity.insert entityIndex movedEntity $ EntityRecord aty row
              Entity.insert entityIndex entity $ EntityRecord dstAty newRow
              pure $ ArchetypeRecord dstAty newRow column
              )
            (error "Archetype did not have component, but it's in the type!")

-- TODO: Does setWithId also enable the component?
setWithId :: (Component c, Coercible (ComponentValueFor c) c) => WorldImpl -> EntityId -> ComponentId c -> c -> IO ()
{-# INLINE setWithId #-}
setWithId w !entity !componentId c =
  addWithId w entity componentId $ \(ArchetypeRecord aty row column) -> do
    Archetype.writeComponent aty row column c

removeWithId :: forall c . Component c => WorldImpl -> EntityId -> ComponentId c -> IO ()
removeWithId w@WorldImpl{entityIndex,archetypeIndex,componentIndex,queryIndex} !entity !componentId = do
  -- where is the entity right now?
  Entity.lookup entityIndex entity
    (\(EntityRecord aty@(Archetype aty#) row) -> do
      -- does this Archetype even have our component?
      Archetype.lookupColumn aty componentId
        (\col -> do
          -- Check if we have an edge
          Archetype.getEdge aty componentId
            (\(ArchetypeEdge dstAty _) -> do
              -- We have an edge
              ArchetypeMove newRow movedEntity <- Archetype.moveTo aty dstAty row
              Entity.insert entityIndex movedEntity $ EntityRecord aty row
              Entity.insert entityIndex entity $ EntityRecord dstAty newRow
              )
            -- No edge
            $ do
              let fromTy = Archetype.ty aty#
                  newTy  = Archetype.Type.removeComponentType fromTy componentId
              -- Do we already have this archetype?
              HT.lookup archetypeIndex (coerce newTy)
                (\dstAty -> do
                  -- Yes, do a table move
                  ArchetypeMove newRow movedEntity <- Archetype.moveTo aty dstAty row
                  Entity.insert entityIndex movedEntity $ EntityRecord aty row
                  Entity.insert entityIndex entity $ EntityRecord dstAty newRow
                  )
                -- No, create new
                $ do
                  let newSizesAndAlign :: SizesAndAlignment = backing @_ @c
                        (Archetype.sizesAndAlignment aty)
                        (Archetype.removeComponentSizes col $ Archetype.sizesAndAlignment aty)
                        (Archetype.sizesAndAlignment aty)
                  dstAty <- Archetype.newWithType newTy newSizesAndAlign
                  HT.insert archetypeIndex (coerce newTy) dstAty

                  Archetype.Type.iterateComponents newTy $ \cid column -> do
                    HT.lookup queryIndex (coerce cid)
                      (\arr -> Array.iterate_ arr $ \_ q -> Query.addArchetype q dstAty)
                      $ pure ()
                    let insertToTable c = do
                          arr <- HT.lookup componentIndex (coerce c)
                                  pure
                                  $ do
                                    arr <- Array.new 4
                                    HT.insert componentIndex (coerce c) arr
                                    pure arr
                          Array.writeBack arr $ ComponentRecord dstAty column
                    
                    when ((coerce @_ @(Bitfield Int Entity.Entity) cid).tag.isRelation) $ do
                      let (fst,snd) = unwrapRelation (coerce cid)
                      insertToTable (mkRelation wildcard wildcard)
                      insertToTable (mkRelation fst wildcard)
                      insertToTable (mkRelation wildcard snd)

                    insertToTable cid
                  
                  Archetype.setEdge aty componentId $ ArchetypeEdge dstAty col
                  ArchetypeMove newRow movedEntity <- Archetype.moveTo aty dstAty row
                  Entity.insert entityIndex movedEntity $ EntityRecord aty row
                  Entity.insert entityIndex entity $ EntityRecord dstAty newRow

          )
        $ pure ()
      )
    $ error "Entity is not in the entity index!"

isEnabledWithId :: forall c . Component c => WorldImpl -> EntityId -> ComponentId c -> IO Bool
{-# INLINE isEnabledWithId #-}
isEnabledWithId WorldImpl{entityIndex} entity componentId =
  Entity.lookup entityIndex entity
    (\(EntityRecord aty row) ->
      Archetype.lookupColumn aty componentId
        (Archetype.isComponentEnabled (Proxy @(ComponentKindFor c)) aty row)
        $ pure False -- TODO What is a missing component in this context?
      )
    $ error "Entity is not in the entity index!"

enableWithId :: forall c . Component c => WorldImpl -> EntityId -> ComponentId c -> IO ()
{-# INLINE enableWithId #-}
enableWithId WorldImpl{entityIndex} entity componentId =
  Entity.lookup entityIndex entity
    (\(EntityRecord aty row) ->
      Archetype.lookupColumn aty componentId
        (Archetype.enableComponent (Proxy @(ComponentKindFor c)) aty row)
        $ pure ())
    $ error "Entity is not in the entity index!"

disableWithId :: forall c . Component c => WorldImpl -> EntityId -> ComponentId c -> IO ()
{-# INLINE disableWithId #-}
disableWithId WorldImpl{entityIndex} entity componentId =
  Entity.lookup entityIndex entity
    (\(EntityRecord aty row) ->
      Archetype.lookupColumn aty componentId
        (Archetype.disableComponent (Proxy @(ComponentKindFor c)) aty row)
        $ pure ())
    $ error "Entity is not in the entity index!"

runFilter :: WorldImpl -> Filter 'True ty -> (Filter.TypedArchetype ty -> b -> IO b) -> IO b -> IO b
{-# INLINE runFilter #-}
runFilter WorldImpl{componentIndex} fi f mz = do
  HT.lookup componentIndex (coerce $ Filter.extractMain fi)
    (\arr ->
      -- TODO Make iterate short circuit so we can make this whole fold a right fold rather than a left fold
      Array.iterate arr (\_ (ComponentRecord aty _) z ->
        (Filter.runFilter fi aty
          (\tyAty acc -> oneShot $ \z' -> f tyAty z' >>= acc)
          (oneShot pure))
          z
        ) mz
      )
    mz

query :: WorldImpl -> Filter 'True ty -> IO (Query ty)
{-# INLINE query #-}
query WorldImpl{componentIndex,queryIndex,emptyArchetype} fi = query_ componentIndex queryIndex emptyArchetype fi

query_
  :: HT.HashTable' HT.Storable HT.Boxed RealWorld IdInt (Array RealWorld ComponentRecord)
  -> HT.HashTable' HT.Storable HT.Boxed RealWorld IdInt (Array RealWorld SomeQuery)
  -> Archetype
  -> Filter 'True ty
  -> IO (Query ty)
{-# INLINE query_ #-}
query_ componentIndex queryIndex (Archetype emptyArchetype) fi = do
  query <- HT.lookup componentIndex (coerce main)
    (\arr -> do
      atys <- Array.new 4
      inds <- Array.new 4
      Array.iterate_ arr $ \_ (ComponentRecord aty _) -> do
        Filter.runFilter fi aty
          (\(Filter.TypedArchetype _ indices) acc -> do
            Array.writeBack atys aty
            Array.writeBack inds indices
            acc
            )
          $ pure ()
      I# sz <- Array.size atys
      let ByteArray eArr = emptyByteArray
          goCopy mAtys mInds n@(I# n#) s
            | n >= I# sz = s
            | otherwise = case ((,) <$> Array.read atys n <*> Array.read inds n) of
              IO f -> case f s of
                (# s1, (Archetype aty#, PrimArray ind) #) -> case writeSmallArray# mAtys n# aty# s1 of
                  s2 -> goCopy mAtys mInds (n + 1) (writeSmallArray# mInds n# ind s2)
      primitive $ \s ->
        case newSmallArray# sz emptyArchetype s of
          (# s1, cachedMatches #) -> case newSmallArray# sz eArr s1 of
            (# s2, cachedIndices #) -> case goCopy cachedMatches cachedIndices 0 s2 of
              s3 -> case unsafeFreezeSmallArray# cachedMatches s3 of
                (# s4, cachedMatchesF #) -> case newMutVar# cachedMatchesF s4 of
                  (# s5, cachedMatchesRef #) -> case unsafeFreezeSmallArray# cachedIndices s5 of
                    (# s6, cachedIndicesF #) -> case newMutVar# cachedIndicesF s6 of
                      (# s7, cachedIndicesRef #) ->
                        (# s7
                        , SomeQuery{newArchetypeHandler = Query.createNewHandler cachedMatchesRef cachedIndicesRef fi,..}
                        #)
      )
    $ do
      let ByteArray arr = emptyByteArray
      primitive $ \s -> case newSmallArray# 0# arr s of
        (# s1, emptySmallArrayInds #) -> case newSmallArray# 0# emptyArchetype s1 of
          (# s2, emptySmallArrayMatch #) -> case unsafeFreezeSmallArray# emptySmallArrayInds s2 of
            (# s3, emptySmallArrayIndsF #) -> case newMutVar# emptySmallArrayIndsF s3 of
              (# s4, emptySmallArrayIndsRef #) -> case unsafeFreezeSmallArray# emptySmallArrayMatch s4 of
                (# s5, emptySmallArrayMatchF #) -> case newMutVar# emptySmallArrayMatchF s5 of
                  (# s6, emptySmallArrayMatchRef #) ->
                    (# s6
                    , SomeQuery
                      { cachedMatchesRef = emptySmallArrayMatchRef
                      , cachedIndicesRef = emptySmallArrayIndsRef
                      , newArchetypeHandler = Query.createNewHandler emptySmallArrayMatchRef emptySmallArrayIndsRef fi
                      }
                    #)
  HT.lookup queryIndex (coerce main)
    (\arr -> Array.writeBack arr $ coerce query)
    $ do
      arr <- Array.new 4
      Array.writeBack arr $ coerce query
      HT.insert queryIndex (coerce main) arr
  pure $ Query query
  where
    main = Filter.extractMain fi

-- Note: monadic state is discarded

-- TODO
-- system_ for iterating entities directly using FoldM in World.Class


-- Systems are scheduled based on data dependencies specified with ty, ins and outs
-- ins specifies components that we read but don't necessarily match against
-- outs specifies components that we write but don't necessarily match against
-- ty specifies read and write for each matched component but ins and outs overwrites them

system :: forall ty ins outs z . Proxy ins -> Proxy outs -> WorldImpl -> Filter 'True ty -> (Filter.TypedArchetype ty -> z -> IO z) -> IO z -> (z -> IO ()) -> IO EntityId
{-# INLINE system #-}
system _ _ w fi func mz extract = do
  Query q <- query w fi
  sys <- newEntity w
  -- system components
  setWithId w sys (getComponentId (Proxy @WorldImpl)) $! q
  setWithId w sys (getComponentId (Proxy @WorldImpl)) $! System (\aty inds z -> func (Filter.TypedArchetype (Archetype aty) (PrimArray inds)) z) mz extract
  pure sys

progress :: WorldImpl -> Int -> IO ()
{-# OPAQUE progress #-}
progress w@(WorldImpl{systemQuery}) dt = do
  -- TODO
  -- Implement deps
  -- Sort the query by deps
  -- By definition everything with the same deps can run at the same time, everything else is already sorted
  Query.runQuery systemQuery
    (\tyAty acc -> do
      Filter.withColumn @System tyAty $ \sysCol ->
        Filter.withColumn @SomeQuery tyAty $ \qCol ->
          Filter.foldTypedArchetype tyAty
            (\n _ ->
              -- Run each system
              Filter.readColumn sysCol n $ \(System f mz e) ->
                Filter.readColumn qCol n $ \q -> do
                  -- TODO Double check the left fold compiles properly. The order is now correct, but I am skeptical about performance
                  Query.runQuery2# q
                    (\(Filter.TypedArchetype (Archetype aty) (PrimArray inds)) acc -> oneShot $ \z -> f aty inds z >>= acc)
                    (oneShot pure)
                    $ \res -> mz >>= res >>= e
              )
            $ pure ()
      acc
      )
    (pure ())
    $ id

instance Class.WorldOps WorldImpl where
  newEntity = liftBase . newEntity
  {-# INLINE newEntity #-}
  freeEntity w eid = liftBase $ freeEntity w eid
  {-# INLINE freeEntity #-}
  hasWithId w eid compId = liftBase $ hasWithId w eid compId
  {-# INLINE hasWithId #-}
  getWithId w eid compId onSucc onFail = do
    st <- liftBaseWith $ \runInBase -> do
      getWithId w eid compId
        (\c -> runInBase $ onSucc c)
        (runInBase onFail)
    restoreM st
  {-# INLINE getWithId #-}
  addWithId w eid compId = liftBase $ addWithId w eid compId (\_ -> pure ())
  {-# INLINE addWithId #-}
  setWithId w eid compId c = liftBase $ setWithId w eid compId c
  {-# INLINE setWithId #-}
  removeWithId w eid compId = liftBase $ removeWithId w eid compId
  {-# INLINE removeWithId #-}
  isEnabledWithId w eid compId = liftBase $ isEnabledWithId w eid compId
  {-# INLINE isEnabledWithId #-}
  enableWithId w eid compId = liftBase $ enableWithId w eid compId
  {-# INLINE enableWithId #-}
  disableWithId w eid compId = liftBase $ disableWithId w eid compId
  {-# INLINE disableWithId #-}
  runFilter w fi f mz = do
    st <- liftBaseWith $ \runInBase -> do
      runFilter w fi (\tyAty zSt -> runInBase $ do
        z <- restoreM zSt
        f tyAty z 
        ) (runInBase mz)
    restoreM st
  {-# INLINE runFilter #-}
  query w fi = liftBase $ query w fi
  {-# INLINE query #-}
  system ins outs w fi func mz extract =
    liftBaseWith $ \runInBase -> do
      system ins outs w fi
        (\tyAty zSt -> runInBase $ restoreM zSt >>= func tyAty)
        (runInBase mz)
        (\zSt -> void . runInBase $ restoreM zSt >>= extract)
  {-# INLINE system #-}
  progress w dt = liftBase $ progress w dt
  {-# INLINE progress #-}
  
