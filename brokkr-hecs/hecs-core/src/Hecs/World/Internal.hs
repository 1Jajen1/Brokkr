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
import Hecs.Filter.Internal (Filter)
import qualified Hecs.Filter.Internal as Filter
import Brokkr.HashTable qualified as HT

import GHC.TypeLits
import Data.Proxy
import Control.Monad
import Data.Coerce
import Data.Foldable (traverse_)
import GHC.IO
import Foreign.Storable (sizeOf, alignment)
import GHC.Exts (Any)
import Control.Concurrent.MVar
import Data.Bits
import Data.Bitfield
import Hecs.Component.Relation
import Hecs.Component.Properties (wildcard)
import Foreign.Storable (Storable)

import GHC.Exts (RealWorld)

-- This is going to be wrapped by 'makeWorld "World" [''Comp1, ''Comp2, ...]' which enables
-- making some component ids static. The componentMap is then only used for unknown/dynamic components

-- | ECS World
--
-- This is the internal definition of a world. Most users will generate a newtype of this with 'makeWorld'.
data WorldImpl (preAllocatedEIds :: Nat) = WorldImpl {
  entitySparseSet      :: {-# UNPACK #-} !(EntityId.EntitySparseSet ArchetypeRecord)
, componentIndexRef    :: {-# UNPACK #-} !(HT.HashTable' HT.Storable HT.Boxed RealWorld IntIdHash (Arr.Array ArchetypeRecord))
, archetypeIndexRef    :: {-# UNPACK #-} !(HT.HashTable' HT.Boxed    HT.Boxed RealWorld ArchetypeTy Archetype)
, emptyArchetype       :: !Archetype -- root node for the archetype graph
, deferredOpsRef       :: {-# UNPACK #-} !(MVar (Arr.Array Command))
, isDeferred           :: !Bool -- TODO Experiment with this on the type level?
, componentHandlersAdd :: {-# UNPACK #-} !(HT.HashTable' HT.Storable HT.Boxed RealWorld IntIdHash (Arr.Array (EntityId -> IO ())))
, componentHandlersRem :: {-# UNPACK #-} !(HT.HashTable' HT.Storable HT.Boxed RealWorld IntIdHash (Arr.Array (EntityId -> IO ())))
}

-- Used for component or entity ids. Statics are sequential, rest is dynamic and also relatively sequential
newtype IntIdHash = IntIdHash Int
  deriving newtype (Eq, Storable)

instance HT.Hash IntIdHash where
  hash (IntIdHash x) = HT.HashFn (const x)
  {-# INLINE hash #-}

-- TODO This is temporary and not very efficient yet
data Command =
    CreateEntity !EntityId
  | forall c .                AddTag          !EntityId !(ComponentId c)
  | forall c . Component c => SetComponent    !EntityId !(ComponentId c) c
  | forall c .                RemoveTag       !EntityId !(ComponentId c)
  | forall c . Component c => RemoveComponent !EntityId !(ComponentId c)
  | DestroyEntity !EntityId
  | Register !ActionType !(ComponentId Any) (EntityId -> IO ())

-- TODO Revisit derring inside proceEntityIding set/get etc
data ArchetypeRecord = ArchetypeRecord !Int !Int !Archetype

instance KnownNat n => WorldClass (WorldImpl n) where
  new = do
    entitySparseSet <- EntityId.new

    componentIndexRef <- HT.new preAllocatedEIds 0 0.75
    archetypeIndexRef <- HT.new 32 0 0.75
    emptyArchetype <- Archetype.empty

    HT.insert archetypeIndexRef (Archetype.getTy emptyArchetype) emptyArchetype

    -- Preallocate a number of ids. This is a setup cost, but enables statically known ids for a list of components
    -- If this is ever too slow, this can be made much more efficient
    replicateM_ (preAllocatedEIds + 1) $ do
      eid <- EntityId.allocateEntityId entitySparseSet
      row <- Archetype.addEntity emptyArchetype eid
      EntityId.insert entitySparseSet eid (ArchetypeRecord row 1 emptyArchetype)

    deferred <- Arr.new 8
    deferredOpsRef <- newMVar deferred

    componentHandlersAdd <- HT.new 4 0 0.75
    componentHandlersRem <- HT.new 4 0 0.75

    let isDeferred = False

    pure WorldImpl{..}
    where
      preAllocatedEIds = fromIntegral @_ @Int $ natVal (Proxy @n)
  allocateEntity w@WorldImpl{..} = do
    !eid <- EntityId.allocateEntityId entitySparseSet

    if isDeferred
      then enqueue w $ CreateEntity eid
      else syncAllocateEntity w eid

    pure eid
  {-# INLINE allocateEntity #-}
  deAllocateEntity w@WorldImpl{..} !eid = if isDeferred
    then enqueue w $ DestroyEntity eid
    else syncDestroyEntity w eid
  {-# INLINE deAllocateEntity #-}
  addTagI w@WorldImpl{..} !eid !compId = if isDeferred
    then enqueue w $ AddTag eid compId
    else syncAddTag w eid compId
  {-# INLINE addTagI #-}
  setI w@WorldImpl{..} !eid !compId comp = if isDeferred
    then enqueue w (SetComponent eid compId comp)
    else syncSetComponent w eid compId comp
  {-# INLINE setI #-}
  getI :: forall c r . Component c => WorldImpl n -> EntityId -> ComponentId c -> (c -> IO r) -> IO r -> IO r
  getI WorldImpl{entitySparseSet} !eid !compId s f = do
    EntityId.lookup entitySparseSet eid >>= \case
      Just (ArchetypeRecord row _ aty) -> Archetype.lookupComponent (Proxy @(ComponentKind c)) aty compId (Archetype.readComponent (Proxy @c) aty row >=> s) f
      Nothing -> f
  {-# INLINE getI #-}
  hasTagI :: forall c . WorldImpl n -> EntityId -> ComponentId c -> IO Bool
  hasTagI WorldImpl{entitySparseSet} !eid !compId = do
    EntityId.lookup entitySparseSet eid >>= \case
      Just (ArchetypeRecord _ _ aty) -> Archetype.lookupComponent (Proxy @Tag) aty compId (const $ pure True) (pure False)
      Nothing -> pure False
  {-# INLINE hasTagI #-}
  removeTagI w@WorldImpl{..} !eid !compId = if isDeferred
    then enqueue w $ RemoveTag eid compId
    else syncRemove (Proxy @Tag) w eid compId
  {-# INLINE removeTagI #-}
  removeComponentI :: forall c . Component c => WorldImpl n -> EntityId -> ComponentId c -> IO ()
  removeComponentI w@WorldImpl{..} !eid !compId = if isDeferred
    then enqueue w $ RemoveComponent eid compId
    else syncRemove (Proxy @(ComponentKind c)) w eid compId
  {-# INLINE removeComponentI #-}
  registerI w@(WorldImpl{..}) !actionType !cid hdl = if isDeferred
    then enqueue w $ Register actionType (coerce cid) hdl
    else syncRegister w actionType (coerce cid) hdl
  {-# INLINE registerI #-}
  -- TODO Check if ghc removes the filter entirely
  filterI WorldImpl{componentIndexRef} !fi f z = HT.lookup componentIndexRef (coerce $ Filter.extractMainId fi) >>= \case
    Just arr ->
      let sz = Arr.size arr
          go !n !b
            | n >= sz   = pure b
            | otherwise = do
              ArchetypeRecord _ _ aty <- Arr.read arr n
              -- TODO Handle wildcard and isA stuff
              if Filter.evaluate fi aty
                then f (coerce aty) b >>= go (n + 1)
                else go (n + 1) b
      in z >>= go 0
    Nothing -> z
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

enqueue :: WorldImpl n -> Command -> IO ()
enqueue WorldImpl{..} !e = modifyMVar_ deferredOpsRef (`Arr.writeBack` e)

syncAllocateEntity :: WorldImpl n -> EntityId -> IO ()
syncAllocateEntity WorldImpl{..} !eid = do
  row <- Archetype.addEntity emptyArchetype eid
  EntityId.insert entitySparseSet eid (ArchetypeRecord row 1 emptyArchetype)

-- Note: Adding or manipulating a component which is no longer alive
--
-- It is poEntityIdible to free components as they are just entities under the hood.
-- There are valid reasons to do so for dynamic components with a fixed lifetime.
--
-- Freeing a component removes all listeners and it removes the component from the index.
-- It also removes all columns and moves all affected entities
--
-- Using the component id after it has been freed is simply undefined and will not be checked against!

syncAdd :: forall c n .
     (Archetype -> ArchetypeTy -> Int -> IO Archetype)
  -> (ArchetypeTy -> IO (ArchetypeTy, Int))
  -> (forall a . Archetype -> (Int -> IO a) -> IO a -> IO a)
  -> WorldImpl n -> EntityId -> ComponentId c -> IO (Archetype, Int, Int)
syncAdd newArchetype addToType lookupCol w@WorldImpl{..} !eid !compId = do
  ArchetypeRecord row _ aty <- EntityId.lookup entitySparseSet eid >>= \case
    Just x -> pure x
    Nothing -> error "Hecs.World.Internal:syncAdd entity id not in entity index!"
  
  -- lookupCol is usually the linear search over column types. This is fast, as shown by simply setting many times and hitting this fast path
  lookupCol aty (\c -> pure (aty, row, c)) $ do
     -- This is the slow path, edge moves are decently fast compared to non-edge moves, but still very slow
     -- Non-edge moves are about 2x slower compared to edge moves
     -- Moves to new archetypes are likely even worse but harder to benchmark

    -- run handlers
    HT.lookup componentHandlersAdd (coerce compId) >>= traverse_ (\hdlArr -> Arr.iterate_ hdlArr $ \f -> f eid) -- 10% even on an empty handlers table!

    Archetype.getEdge aty compId >>= \case
      ArchetypeEdge (Just dstAty) _-> lookupCol dstAty (\c -> do
        (newRow, movedEid) <- Archetype.moveEntity aty row c dstAty -- 30% and worse the more stuff we need to copy

        unless (movedEid == eid) $ EntityId.insert entitySparseSet movedEid (ArchetypeRecord row 1 aty)
        EntityId.insert entitySparseSet eid (ArchetypeRecord newRow 1 dstAty)
        pure (dstAty, newRow, c)
        )
        (error $ "Hecs.World.Internal:syncAdd edge destination did not have component: " <> show compId <> ". Searched in " <> show (getTy dstAty))
      ArchetypeEdge Nothing _ -> syncAddSlow newArchetype addToType w eid compId aty row
{-# INLINE syncAdd #-}

-- Split into two functions to be able to inline the fast-ish path
syncAddSlow :: forall c n .
     (Archetype -> ArchetypeTy -> Int -> IO Archetype)
  -> (ArchetypeTy -> IO (ArchetypeTy, Int))
  -> WorldImpl n -> EntityId -> ComponentId c -> Archetype -> Int -> IO (Archetype, Int, Int)
syncAddSlow newArchetype addToType WorldImpl{..} !eid !compId !aty !row = do
  (newTy, newColumn) <- addToType (Archetype.getTy aty)

  dstAty <- HT.lookup archetypeIndexRef newTy >>= \case
    Just dstAty -> do
      -- putStrLn "Cheap move (no edge)" 
      Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
      pure dstAty
    Nothing -> do
      -- putStrLn "Expensive move" 
      dstAty <- newArchetype aty newTy newColumn

      Archetype.setEdge aty compId (ArchetypeEdge (Just dstAty) Nothing)
      HT.insert archetypeIndexRef newTy dstAty

      Archetype.iterateComponentIds newTy (\tyId col ind' -> do 
          -- Relation components are treated slightly different
          if (coerce @_ @(Bitfield Int EntityId.Entity) tyId).tag.isRelation
            then do
              let (first, second) = unwrapRelation $ coerce compId
                  writeWildCard rel = do
                    arr <- HT.lookup componentIndexRef rel >>= \case
                      Just arr -> do
                        let i = Arr.size arr - 1
                        ArchetypeRecord _ count aty' <- Arr.read arr i
                        if aty' == dstAty
                          then Arr.write arr i (ArchetypeRecord col (count + 1) dstAty) >> pure arr
                          else Arr.writeBack arr $ ArchetypeRecord col 1 dstAty
                      Nothing -> Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col 1 dstAty)
                    HT.insert componentIndexRef rel arr
              
              -- add the wildcard parts to the index: Rel Type x and Rel x Type for both first and second
              writeWildCard (coerce $ mkRelation wildcard first)
              writeWildCard (coerce $ mkRelation first wildcard)
              writeWildCard (coerce $ mkRelation wildcard second)
              writeWildCard (coerce $ mkRelation second wildcard)
            
            else pure ind'
          
          -- add to the component index
          arr <- HT.lookup componentIndexRef (coerce tyId) >>= \case
            Just arr -> Arr.writeBack arr $ ArchetypeRecord col 1 dstAty
            Nothing -> Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col 1 dstAty) -- TODO Check if count=1 is a safe aEntityIdumption?
          HT.insert componentIndexRef (coerce tyId) arr
        ) (pure ())

      pure dstAty
  -- now move the entity and its current data between the two
  (newRow, movedEid) <- Archetype.moveEntity aty row newColumn dstAty

  unless (movedEid == eid) $ EntityId.insert entitySparseSet movedEid(ArchetypeRecord row 1 aty)
  EntityId.insert entitySparseSet eid (ArchetypeRecord newRow 1 dstAty)

  pure (dstAty, newRow, newColumn)

syncAddTag :: WorldImpl n -> EntityId -> ComponentId c -> IO ()
syncAddTag w !eid !compId = void $ syncAdd
  (\aty newTy _ -> Archetype.createArchetype newTy (getColumnSizes aty))
  (\aty -> Archetype.addComponentType (Proxy @Tag) aty compId)
  (\aty -> Archetype.lookupComponent (Proxy @Tag) aty compId)
  w eid compId
{-# INLINE syncAddTag #-}

-- TODO GHC is happy to unbox WorldImpl but equally happy to box EntityId and ComponentId ... Maybe just worker wrapper everything manually
syncSetComponent :: forall c n . Component c => WorldImpl n -> EntityId -> ComponentId c -> c -> IO ()
syncSetComponent w !eid !compId comp = do
  -- This is a fun little hack to get ghc to worker wrapper fields *if* they are stored
  -- using the storable instance
  () <- backing (Proxy @c) (pure ()) $ seq comp $ pure ()
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
{-# INLINE syncSetComponent #-}

-- TODO Define syncRemove in terms of syncAdd, or even generalize them?
-- That would enable me to inline syncRemove and keep the long worker out of line

syncRemove :: forall ty c n . KnownComponentType ty => Proxy ty -> WorldImpl n -> EntityId -> ComponentId c -> IO ()
syncRemove ty WorldImpl{..} !eid !compId = do
  ArchetypeRecord row _ aty <- EntityId.lookup entitySparseSet eid >>= \case
    Just x -> pure x
    Nothing -> error "Hecs.World.Internal:syncRemove entity id not in entity index!"

  Archetype.lookupComponent ty aty compId (\removedColumn -> do
    -- run handlers
    HT.lookup componentHandlersRem (coerce compId) >>= traverse_ (\hdlArr -> Arr.iterate_ hdlArr $ \f -> f eid)

    dstAty <- Archetype.getEdge aty compId >>= \case
      ArchetypeEdge _ (Just dstAty) -> pure dstAty
      ArchetypeEdge _ Nothing -> do
        newTy <- Archetype.removeComponentType ty (Archetype.getTy aty) removedColumn

        HT.lookup archetypeIndexRef newTy >>= \case
          Just dstAty -> do
            -- putStrLn "Cheap move (no edge)" 
            Archetype.setEdge aty compId (ArchetypeEdge Nothing (Just dstAty))
            pure dstAty
          Nothing -> do
            -- putStrLn "Expensive move" 
            dstAty <- branchCompType ty
              (Archetype.createArchetype newTy (getColumnSizes aty))
              (IO $ \s0 -> case Archetype.removeColumnSize removedColumn (Archetype.getColumnSizes aty) s0 of
                (# s1, newSzs #) -> case Archetype.createArchetype newTy newSzs of
                  IO f -> f s1)
              (Archetype.createArchetype newTy (getColumnSizes aty))

            Archetype.setEdge aty compId (ArchetypeEdge Nothing (Just dstAty))
            HT.insert archetypeIndexRef newTy dstAty

            Archetype.iterateComponentIds newTy (\tyId col ind' -> do

              -- Relation components are treated slightly different
              if (coerce @_ @(Bitfield Int EntityId.Entity) tyId).tag.isRelation
                then do
                  let (first, second) = unwrapRelation $ coerce compId
                      writeWildCard rel = do
                        arr <- HT.lookup componentIndexRef rel >>= \case
                          Just arr -> do
                            let i = Arr.size arr - 1
                            ArchetypeRecord _ count aty' <- Arr.read arr i
                            if aty' == dstAty
                              then Arr.write arr i (ArchetypeRecord col (count + 1) dstAty) >> pure arr
                              else Arr.writeBack arr $ ArchetypeRecord col 1 dstAty
                          Nothing -> Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col 1 dstAty)
                        HT.insert componentIndexRef rel arr

                  -- add the wildcard parts to the index: Rel Type x and Rel x Type for both first and second
                  -- TODO Actually do I want Rel l r to register Rel x l and Rel r x?
                  -- TODO This whole count thing is bad isn't it? 
                  writeWildCard (coerce $ mkRelation wildcard first)
                  writeWildCard (coerce $ mkRelation first wildcard)
                  writeWildCard (coerce $ mkRelation wildcard second)
                  writeWildCard (coerce $ mkRelation second wildcard)

                else pure ind'

              arr <- HT.lookup componentIndexRef (coerce tyId) >>= \case
                Just arr -> Arr.writeBack arr $ ArchetypeRecord col 1 dstAty
                Nothing -> Arr.new 4 >>= (`Arr.writeBack` ArchetypeRecord col 1 dstAty) -- TODO Check if count=1 is a safe aEntityIdumption?
              HT.insert componentIndexRef (coerce tyId) arr) (pure ())

            pure dstAty

    -- print (row, removedColumn, Archetype.getTy aty, Archetype.getTy dstAty)
    -- now move the entity and its current data between the two
    (newRow, movedEid) <- Archetype.moveEntity aty row removedColumn dstAty

    unless (movedEid == eid) $ EntityId.insert entitySparseSet movedEid (ArchetypeRecord row 1 aty)
    EntityId.insert entitySparseSet eid (ArchetypeRecord newRow 1 dstAty)
    ) $ pure ()

syncDestroyEntity :: WorldImpl n -> EntityId -> IO ()
syncDestroyEntity WorldImpl{..} !eid = do
  EntityId.deAllocateEntityId entitySparseSet eid >>= \case
    -- we could just ignore this, but this is almost always a user bug!
    Nothing -> error "Tried to free an already freed entityId"
    Just (ArchetypeRecord row _ aty) -> do
      -- TODO Get all components and run the remove handlers
      Archetype.iterateComponentIds (Archetype.getTy aty) (\cid _ _ -> do
        HT.lookup componentHandlersRem (coerce cid) >>= traverse_ (\hdlArr -> Arr.iterate_ hdlArr $ \f -> f eid)
        ) (pure ())

      -- This eid might be a component, so remove them from the relevant structures
      HT.delete componentIndexRef (coerce $ ComponentId eid) >>= \case
        Nothing -> pure ()
        Just arr -> do
          -- TODO This is a little annoying. Basically needs an archetype move for every member of every archetype in this list.
          --      This is reasonably fast as we can copy entire columns at once, its just really annoying to do...
          Arr.iterate_ arr $ \(ArchetypeRecord _col _ _aty) -> pure ()
          void $ HT.delete componentHandlersAdd (coerce $ ComponentId eid)
          void $ HT.delete componentHandlersRem (coerce $ ComponentId eid)
      
      movedEid <- Archetype.removeEntity aty row

      unless (movedEid == eid) $ EntityId.insert entitySparseSet movedEid (ArchetypeRecord row 1 aty)

syncRegister :: WorldImpl n -> ActionType -> ComponentId Any -> (EntityId -> IO ()) -> IO ()
syncRegister WorldImpl{..} OnAdd !cid hdl = do
  -- add it to the global registry of handlers
  HT.lookup componentHandlersAdd (coerce cid) >>= \case
    Just arr -> Arr.writeBack arr hdl >>= HT.insert componentHandlersAdd (coerce cid)
    Nothing -> Arr.new 2 >>= \arr -> Arr.writeBack arr hdl >>= HT.insert componentHandlersAdd (coerce cid)
syncRegister WorldImpl{..} OnRemove !cid hdl = do
  -- add it to the global registry of handlers
  HT.lookup componentHandlersAdd (coerce cid) >>= \case
    Just arr -> Arr.writeBack arr hdl >>= HT.insert componentHandlersAdd (coerce cid)
    Nothing -> Arr.new 2 >>= \arr -> Arr.writeBack arr hdl >>= HT.insert componentHandlersAdd (coerce cid)


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

-- TODO Revisit inline/inlineable pragmas.
--      - AggreEntityIdively optimize the fast path, always inline it, always keep the slow path out of line!