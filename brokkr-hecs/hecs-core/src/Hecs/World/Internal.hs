{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Hecs.World.Internal (
  WorldImpl(..)
, WorldClass(..)
, syncSetComponent
, ActionType(..)
) where

import qualified Hecs.Array as Arr
import Hecs.Archetype.Internal as Archetype
import Hecs.Component.Internal
import Hecs.Entity.Internal (EntityId(..))
import qualified Hecs.Entity.Internal as EntityId
import qualified Hecs.HashTable.Boxed as HTB
import Hecs.Filter.Internal (Filter)
import qualified Hecs.Filter.Internal as Filter

import GHC.TypeLits
import Data.Proxy
import Control.Monad
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Coerce
import GHC.IO
import Foreign.Storable (sizeOf, alignment)
import GHC.Exts (Any)
import Data.IORef
import Control.Concurrent.MVar
import Data.Bits
import Data.Bitfield
import Hecs.Component.Relation
import Hecs.Component.Properties (wildcard)

-- This is going to be wrapped by 'makeWorld "World" [''Comp1, ''Comp2, ...]' which enables
-- making some component ids static. The componentMap is then only used for unknown/dynamic components

-- | ECS World
--
-- This is the internal definition of a world. Most users will generate a newtype of this with 'makeWorld'.
data WorldImpl (preAllocatedEIds :: Nat) = WorldImpl {
  freshEIdRef          :: !(MVar EntityId.FreshEntityId) -- allocate unique entity ids with reuse
, entityIndexRef       :: !(IORef (IntMap ArchetypeRecord)) -- changes often and thus needs good allround performance
, componentIndexRef    :: !(IORef (HTB.HashTable (ComponentId Any) (Arr.Array ArchetypeRecord))) -- changes infrequently if ever after the component graph stabilises, so only read perf matters, but also not too much
, archetypeIndexRef    :: !(IORef (HTB.HashTable ArchetypeTy Archetype)) -- changes infrequently once graph stabilises
, emptyArchetype       :: !Archetype
, deferredOpsRef       :: !(MVar (Arr.Array Command))
, isDeferred           :: !Bool -- TODO Experiment with this on the type level
, componentHandlersAdd :: !(IORef (HTB.HashTable (ComponentId Any) (Arr.Array (EntityId -> IO ()))))
, componentHandlersRem :: !(IORef (HTB.HashTable (ComponentId Any) (Arr.Array (EntityId -> IO ()))))
}

-- TODO This is temporary and not very efficient yet
data Command =
    CreateEntity !EntityId
  | forall c .                AddTag          !EntityId !(ComponentId c)
  | forall c . Component c => SetComponent    !EntityId !(ComponentId c) c
  | forall c .                RemoveTag       !EntityId !(ComponentId c)
  | forall c . Component c => RemoveComponent !EntityId !(ComponentId c)
  | DestroyEntity !EntityId
  | Register !ActionType !(ComponentId Any) (EntityId -> IO ())

-- TODO Revisit derring inside processing set/get etc
data ArchetypeRecord = ArchetypeRecord !Int !Int !Archetype

instance KnownNat n => WorldClass (WorldImpl n) where
  new = do
    freshEId' <- EntityId.new -- TODO Better init sz

    let entityIndex = mempty -- TODO Better init sz
    componentIndex <- HTB.new 32 -- TODO Better init sz
    archetypeIndex' <- HTB.new 32 -- TODO Better init sz
    emptyArchetype <- Archetype.empty

    archetypeIndex <- HTB.insert archetypeIndex' (Archetype.getTy emptyArchetype) emptyArchetype

    entityIndexRef <- newIORef entityIndex
    componentIndexRef <- newIORef componentIndex
    archetypeIndexRef <- newIORef archetypeIndex

    -- Preallocate a number of ids. This is a setup cost, but enables statically known ids for a list of components
    -- If this is ever too slow, this can be made much more efficient
    freshEId <- foldr (\_ feid -> do
      (st, eid) <- feid >>= EntityId.allocateEntityId
      row <- Archetype.addEntity emptyArchetype eid
      modifyIORef' entityIndexRef $ IM.insert (coerce eid) (ArchetypeRecord row 1 emptyArchetype) -- TODO Check if count=1 is a safe assumption?
      pure st
      ) (pure freshEId') [0..preAllocatedEIds]

    freshEIdRef <- newMVar freshEId

    deferred <- Arr.new 8
    deferredOpsRef <- newMVar deferred

    componentHandlersAddTable <- HTB.new 8
    componentHandlersAdd <- newIORef componentHandlersAddTable

    componentHandlersRemTable <- HTB.new 8
    componentHandlersRem <- newIORef componentHandlersRemTable

    let isDeferred = False

    pure WorldImpl{..}
    where
      preAllocatedEIds = fromIntegral @_ @Int $ natVal (Proxy @n)
  allocateEntity w@WorldImpl{..} = do
    eid <- modifyMVar freshEIdRef EntityId.allocateEntityId -- TODO Strictness

    if isDeferred
      then modifyMVar_ deferredOpsRef (`Arr.writeBack` CreateEntity eid) -- TODO Strictness
      else syncAllocateEntity w eid

    pure eid
  {-# INLINE allocateEntity #-}
  deAllocateEntity w@WorldImpl{..} eid = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` DestroyEntity eid) -- TODO Strictness
    else syncDestroyEntity w eid
  {-# INLINE deAllocateEntity #-}
  addTagI w@WorldImpl{..} eid compId = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` AddTag eid compId) -- TODO Strictness
    else syncAddTag w eid compId
  {-# INLINE addTagI #-}
  setI w@WorldImpl{..} eid compId comp = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` SetComponent eid compId comp) -- TODO Strictness
    else syncSetComponent w eid compId comp
  {-# INLINE setI #-}
  getI :: forall c r . Component c => WorldImpl n -> EntityId -> ComponentId c -> (c -> IO r) -> IO r -> IO r
  getI WorldImpl{entityIndexRef} eid compId s f = do
    readIORef entityIndexRef >>= (\case
      Just (ArchetypeRecord row _ aty) -> Archetype.lookupComponent (Proxy @(ComponentKind c)) aty compId (Archetype.readComponent (Proxy @c) aty row >=> s) f
      Nothing -> f) . IM.lookup (coerce eid)
  {-# INLINE getI #-}
  hasTagI :: forall c . WorldImpl n -> EntityId -> ComponentId c -> IO Bool
  hasTagI WorldImpl{entityIndexRef} eid compId = do
    readIORef entityIndexRef >>= (\case
      Just (ArchetypeRecord _ _ aty) -> Archetype.lookupComponent (Proxy @Tag) aty compId (const $ pure True) (pure False)
      Nothing -> pure False) . IM.lookup (coerce eid)
  {-# INLINE hasTagI #-}
  removeTagI w@WorldImpl{..} eid compId = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` RemoveTag eid compId) -- TODO Strictness
    else syncRemove (Proxy @Tag) w eid compId
  {-# INLINE removeTagI #-}
  removeComponentI :: forall c . Component c => WorldImpl n -> EntityId -> ComponentId c -> IO ()
  removeComponentI w@WorldImpl{..} eid compId = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` RemoveComponent eid compId) -- TODO Strictness
    else syncRemove (Proxy @(ComponentKind c)) w eid compId
  {-# INLINE removeComponentI #-}
  registerI w@(WorldImpl{..}) actionType cid hdl = if isDeferred
    then modifyMVar_ deferredOpsRef (`Arr.writeBack` Register actionType (coerce cid) hdl)
    else syncRegister w actionType (coerce cid) hdl
  {-# INLINE registerI #-}
  -- TODO Check if ghc removes the filter entirely
  filterI WorldImpl{componentIndexRef} fi f z = readIORef componentIndexRef >>= \componentIndex -> HTB.lookup componentIndex (Filter.extractMainId fi)
    (\arr ->
      let sz = Arr.size arr
          go !n !b
            | n >= sz   = pure b
            | otherwise = do
              ArchetypeRecord _ _ aty <- Arr.read arr n
              -- TODO Handle wildcard and isA stuff
              if Filter.evaluate fi aty
                then f (coerce aty) b >>= go (n + 1)
                else go (n + 1) b
      in z >>= go 0)
    z
  {-# INLINE filterI #-}
  -- TODO Should I sync here? I don't think so, but then I probably need to wrap this in Hecs.World!
  defer w act = act $ w { isDeferred = True }
  sync w@WorldImpl{deferredOpsRef} = modifyMVar_ deferredOpsRef $ \arr -> go arr 0 >> Arr.new (max 8 $ Arr.size arr `unsafeShiftR` 2) -- TODO Better shrinking?
    where
      go !arr !n
        | n >= Arr.size arr = pure ()
        | otherwise = do
          Arr.read arr n >>= \case
            CreateEntity e -> syncAllocateEntity w e
            AddTag e cId -> syncAddTag w e cId
            SetComponent e cId c -> syncSetComponent w e cId c
            RemoveTag e cId -> syncRemove (Proxy @Tag) w e cId
            RemoveComponent @c e cId -> syncRemove (Proxy @(ComponentKind c)) w e cId
            DestroyEntity e -> syncDestroyEntity w e
            Register actionType cid hdl -> syncRegister w actionType cid hdl
          go arr (n + 1)

syncAllocateEntity :: WorldImpl n -> EntityId -> IO ()
syncAllocateEntity WorldImpl{..} eid = do
  row <- Archetype.addEntity emptyArchetype eid
  modifyIORef' entityIndexRef $ IM.insert (coerce eid) (ArchetypeRecord row 1 emptyArchetype)  -- TODO Check if count=1 is a safe assumption?

syncAdd :: forall c n .
     (Archetype -> ArchetypeTy -> Int -> IO Archetype)
  -> (ArchetypeTy -> IO (ArchetypeTy, Int))
  -> (forall a . Archetype -> (Int -> IO a) -> IO a -> IO a)
  -> WorldImpl n -> EntityId -> ComponentId c -> IO (Archetype, Int, Int)
syncAdd newArchetype addToType lookupCol WorldImpl{..} eid compId = do
  eIndex <- readIORef entityIndexRef
  let ArchetypeRecord row _ aty = IM.findWithDefault (error "Hecs.World.Internal:syncAdd entity id not in entity index!") (coerce eid) eIndex
  lookupCol aty (\c -> pure (aty, row, c)) $ do
    -- run handlers
    hdlTable <- readIORef componentHandlersAdd
    HTB.lookup hdlTable (coerce compId) (\hdlArr -> Arr.iterate_ hdlArr $ \f -> f eid) $ pure ()

    Archetype.getEdge aty compId >>= \case
      ArchetypeEdge (Just dstAty) _-> lookupCol dstAty (\c -> do
        (newRow, movedEid) <- Archetype.moveEntity aty row c dstAty
        -- Important insert the moved first in case it is ourselves so that we overwrite it after
        writeIORef entityIndexRef $! IM.insert (coerce eid) (ArchetypeRecord newRow 1 dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row 1 aty) eIndex
        pure (dstAty, newRow, c)
        )
        (error $ "Hecs.World.Internal:syncAdd edge destination did not have component: " <> show compId <> ". Searched in " <> show (getTy dstAty) )
      ArchetypeEdge Nothing _ -> do
        (newTy, newColumn) <- addToType (Archetype.getTy aty)

        archetypeIndex <- readIORef archetypeIndexRef
        dstAty <- HTB.lookup archetypeIndex newTy (\dstAty -> do
            -- putStrLn "Cheap move (no edge)" 
            Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
            pure dstAty
          ) $ do
            -- putStrLn "Expensive move" 
            dstAty <- newArchetype aty newTy newColumn

            Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
            !newArchetypeIndex <- HTB.insert archetypeIndex newTy dstAty
            writeIORef archetypeIndexRef newArchetypeIndex

            componentIndex <- readIORef componentIndexRef
            !compIndex <- Archetype.iterateComponentIds newTy (\tyId col ind' -> do 
                -- Relation components are treated slightly different
                ind <- if (coerce @_ @(Bitfield Int EntityId.Entity) tyId).tag.isRelation
                  then do
                    let (first, second) = unwrapRelation $ coerce compId
                        writeWildCard rel ind = do
                          arr <- HTB.lookup ind rel
                            (\arr -> do
                              let i = Arr.size arr - 1
                              ArchetypeRecord _ count aty' <- Arr.read arr i
                              if aty' == dstAty
                                then Arr.write arr i (ArchetypeRecord col (count + 1) dstAty) >> pure arr
                                else Arr.writeBack arr $ ArchetypeRecord col 1 dstAty
                            )
                            $ Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col 1 dstAty)
                          HTB.insert ind rel arr
                    
                    -- add the wildcard parts to the index: Rel Type x and Rel x Type for both first and second
                    writeWildCard (coerce $ mkRelation wildcard first) ind'
                      >>= writeWildCard (coerce $ mkRelation first wildcard)
                      >>= writeWildCard (coerce $ mkRelation wildcard second)
                      >>= writeWildCard (coerce $ mkRelation second wildcard)
                  
                  else pure ind'
                
                -- add to the component index
                arr <- HTB.lookup ind (coerce tyId) (`Arr.writeBack` ArchetypeRecord col 1 dstAty) $ Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col 1 dstAty) -- TODO Check if count=1 is a safe assumption?
                HTB.insert ind (coerce tyId) arr
              
              ) (pure componentIndex)

            writeIORef componentIndexRef compIndex

            pure dstAty
        -- now move the entity and its current data between the two
        (newRow, movedEid) <- Archetype.moveEntity aty row newColumn dstAty

        -- Important insert the moved first in case it is ourselves so that we overwrite it after
        writeIORef entityIndexRef $! IM.insert (coerce eid) (ArchetypeRecord newRow 1 dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row 1 aty) eIndex -- TODO Check if count=1 is a safe assumption?

        pure (dstAty, newRow, newColumn)
{-# INLINE syncAdd #-}

syncAddTag :: WorldImpl n -> EntityId -> ComponentId c -> IO ()
syncAddTag w eid compId = void $ syncAdd
  (\aty newTy _ -> Archetype.createArchetype newTy (getColumnSizes aty))
  (\aty -> Archetype.addComponentType (Proxy @Tag) aty compId)
  (\aty -> Archetype.lookupComponent (Proxy @Tag) aty compId)
  w eid compId

syncSetComponent :: forall c n . Component c => WorldImpl n -> EntityId -> ComponentId c -> c -> IO ()
syncSetComponent w eid compId comp = do
  (newAty, newRow, newCol) <- syncAdd
    (\aty newTy newColumn -> backing (Proxy @c)
      (Archetype.createArchetype newTy (getColumnSizes aty))
      (IO $ \s0 -> case Archetype.addColumnSize newColumn (sizeOf (undefined @_ @(Value c))) (alignment (undefined @_ @(Value c))) (Archetype.getColumnSizes aty) s0 of
        (# s1, newSzs #) -> case Archetype.createArchetype newTy newSzs of
          IO f -> f s1))
    (\atyTy -> Archetype.addComponentType (Proxy @(ComponentKind c)) atyTy compId)
    (\aty -> Archetype.lookupComponent (Proxy @(ComponentKind c)) aty compId)
    w eid compId
  Archetype.writeComponent (Proxy @c) newAty newRow newCol comp
{-# INLINABLE syncSetComponent #-}

syncRemove :: forall ty c n . KnownComponentType ty => Proxy ty -> WorldImpl n -> EntityId -> ComponentId c -> IO ()
syncRemove ty WorldImpl{..} eid compId = do
  eIndex <- readIORef entityIndexRef
  let ArchetypeRecord row _ aty = IM.findWithDefault (error "Hecs.World.Internal:syncRemove entity id not in entity index!") (coerce eid) eIndex
  Archetype.lookupComponent ty aty compId (\removedColumn -> do
    -- run handlers
    hdlTable <- readIORef componentHandlersRem
    HTB.lookup hdlTable (coerce compId) (\hdlArr -> Arr.iterate_ hdlArr $ \f -> f eid) $ pure ()

    dstAty <- Archetype.getEdge aty compId >>= \case
      ArchetypeEdge _ (Just dstAty) -> pure dstAty
      ArchetypeEdge _ Nothing -> do
        newTy <- Archetype.removeComponentType ty (Archetype.getTy aty) removedColumn

        archetypeIndex <- readIORef archetypeIndexRef
        HTB.lookup archetypeIndex newTy (\dstAty -> do
            -- putStrLn "Cheap move (no edge)" 
            Archetype.setEdge aty compId (ArchetypeEdge Nothing (Just dstAty))
            pure dstAty
          ) $ do
            -- putStrLn "Expensive move" 
            dstAty <- branchCompType ty
              (Archetype.createArchetype newTy (getColumnSizes aty))
              (IO $ \s0 -> case Archetype.removeColumnSize removedColumn (Archetype.getColumnSizes aty) s0 of
                (# s1, newSzs #) -> case Archetype.createArchetype newTy newSzs of
                  IO f -> f s1)
              (Archetype.createArchetype newTy (getColumnSizes aty))

            Archetype.setEdge aty compId (ArchetypeEdge Nothing (Just dstAty))
            !newArchetypeIndex <- HTB.insert archetypeIndex newTy dstAty
            writeIORef archetypeIndexRef newArchetypeIndex

            componentIndex <- readIORef componentIndexRef
            !compIndex <- Archetype.iterateComponentIds newTy (\tyId col ind' -> do

              -- Relation components are treated slightly different
              ind <- if (coerce @_ @(Bitfield Int EntityId.Entity) tyId).tag.isRelation
                then do
                  let (first, second) = unwrapRelation $ coerce compId
                      writeWildCard rel ind = do
                        arr <- HTB.lookup ind rel
                          (\arr -> do
                            let i = Arr.size arr - 1
                            ArchetypeRecord _ count aty' <- Arr.read arr i
                            if aty' == dstAty
                              then Arr.write arr i (ArchetypeRecord col (count + 1) dstAty) >> pure arr
                              else Arr.writeBack arr $ ArchetypeRecord col 1 dstAty
                          )
                          $ Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col 1 dstAty)
                        HTB.insert ind rel arr

                  -- add the wildcard parts to the index: Rel Type x and Rel x Type for both first and second
                  writeWildCard (coerce $ mkRelation wildcard first) ind'
                    >>= writeWildCard (coerce $ mkRelation first wildcard)
                    >>= writeWildCard (coerce $ mkRelation wildcard second)
                    >>= writeWildCard (coerce $ mkRelation second wildcard)

                else pure ind'

              arr <- HTB.lookup ind (coerce tyId) (`Arr.writeBack` ArchetypeRecord col 1 dstAty) $ Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col 1 dstAty) -- TODO Check if count=1 is a safe assumption?
              HTB.insert ind (coerce tyId) arr) (pure componentIndex)
            writeIORef componentIndexRef compIndex

            pure dstAty

    -- print (row, removedColumn, Archetype.getTy aty, Archetype.getTy dstAty)
    -- now move the entity and its current data between the two
    (newRow, movedEid) <- Archetype.moveEntity aty row removedColumn dstAty

    -- Important insert the moved first in case it is ourselves so that we overwrite it after
    writeIORef entityIndexRef $! IM.insert (coerce eid) (ArchetypeRecord newRow 1 dstAty) $ IM.insert (coerce movedEid) (ArchetypeRecord row 1 aty) eIndex
    ) $ pure ()
{-# SPECIALISE syncRemove :: forall c n . Proxy Boxed -> WorldImpl n -> EntityId -> ComponentId c -> IO () #-}
{-# SPECIALISE syncRemove :: forall c n . Proxy Flat  -> WorldImpl n -> EntityId -> ComponentId c -> IO () #-}
{-# SPECIALISE syncRemove :: forall c n . Proxy Tag   -> WorldImpl n -> EntityId -> ComponentId c -> IO () #-}

syncDestroyEntity :: WorldImpl n -> EntityId -> IO ()
syncDestroyEntity WorldImpl{..} eid = do
  modifyMVar_ freshEIdRef $ flip EntityId.deAllocateEntityId eid -- TODO Strictness
  eIndex <- readIORef entityIndexRef
  let ArchetypeRecord row _ aty = IM.findWithDefault (error "Hecs.World.Internal:syncAdd entity id not in entity index!") (coerce eid) eIndex

  -- TODO Get all components and run the remove handlers
  Archetype.iterateComponentIds (Archetype.getTy aty) (\cid _ _ -> do
    hdlTable <- readIORef componentHandlersRem
    HTB.lookup hdlTable (coerce cid) (\hdlArr -> Arr.iterate_ hdlArr $ \f -> f eid) $ pure ()
    ) (pure ())
  
  movedEid <- Archetype.removeEntity aty row

  writeIORef entityIndexRef $! IM.delete (coerce eid) $ IM.insert (coerce movedEid) (ArchetypeRecord row 1 aty) eIndex -- TODO Check if count=1 is a safe assumption?

syncRegister :: WorldImpl n -> ActionType -> ComponentId Any -> (EntityId -> IO ()) -> IO ()
syncRegister WorldImpl{..} OnAdd cid hdl = do
  -- add it to the global registry of handlers
  table <- readIORef componentHandlersAdd
  table' <- HTB.lookup table (coerce cid)
    (\arr -> Arr.writeBack arr hdl >>= HTB.insert table (coerce cid)) 
    $ Arr.new 2 >>= \arr -> Arr.writeBack arr hdl >>= HTB.insert table (coerce cid)
  writeIORef componentHandlersAdd table'
syncRegister WorldImpl{..} OnRemove cid hdl = do
  -- add it to the global registry of handlers
  table <- readIORef componentHandlersRem
  table' <- HTB.lookup table (coerce cid)
    (\arr -> Arr.writeBack arr hdl >>= HTB.insert table (coerce cid)) 
    $ Arr.new 2 >>= \arr -> Arr.writeBack arr hdl >>= HTB.insert table (coerce cid)
  writeIORef componentHandlersRem table'

-- | All behavior a World has to support. makeWorld creates a newtype around WorldImpl and derives this
class WorldClass w where
  new :: IO w
  allocateEntity :: w -> IO EntityId
  deAllocateEntity :: w -> EntityId -> IO ()
  addTagI :: w -> EntityId -> ComponentId c -> IO ()
  setI :: Component c => w -> EntityId -> ComponentId c -> c -> IO ()
  getI :: Component c => w -> EntityId -> ComponentId c -> (c -> IO r) -> IO r -> IO r
  hasTagI :: w -> EntityId -> ComponentId c -> IO Bool
  removeTagI :: w -> EntityId -> ComponentId c -> IO ()
  removeComponentI :: Component c => w -> EntityId -> ComponentId c -> IO ()
  filterI :: w -> Filter ty Filter.HasMainId -> (Filter.TypedArchetype ty -> b -> IO b) -> IO b -> IO b
  defer :: w -> (w -> IO a) -> IO a
  sync :: w -> IO ()
  registerI :: w -> ActionType -> ComponentId c -> (EntityId -> IO ()) -> IO ()

-- | Actions which one can trigger callbacks for
--
-- 'OnAdd' triggers when a component is added to an entity
-- 'OnRemove' triggers when a component is removed from an entity or an entity with this component is destroyed
data ActionType = OnAdd | OnRemove

-- TODO I am probably (most likely) a little excessive on the inline/inlineable pragmas
-- I probably need them on functions that take continuation arguments. And inlineable on functions with typeclasses so that I can specialize them on import.
-- But other than that I should probably get rid of some 
