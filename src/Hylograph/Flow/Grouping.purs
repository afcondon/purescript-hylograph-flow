-- | Grouping beside a flow, never in it: a nested partition of step ids (a
-- | rose tree whose leaves are steps), and `collapse`, which draws a cut
-- | through it. Each closed group becomes one step; the items only its
-- | members write and read are hidden; every other wire is re-attached to the
-- | group, by a port that is the member and its port (`MemberPort`), and
-- | wires left parallel are merged into one.
-- |
-- | A closed group's step is `GroupStep` of the group's id and its ports are
-- | `MemberPort`s, constructors no step or port of a source has. So whatever
-- | the source's steps and ports are called, none is taken for a group's,
-- | and a flow already collapsed can be collapsed again: a `Member` may name
-- | a `GroupStep`. (The one thing left to the caller: a second cut must not
-- | reuse the id of a group the first cut closed, unless it holds that
-- | group's step.)
-- |
-- | One mechanism for a radio's fused runs, for semantic zoom, and for the
-- | rig chart's machines opened into their voices.
module Hylograph.Flow.Grouping
  ( module Exports
  , Grouping(..)
  , Collapsed(..)
  , members
  , collapse
  , groupStep
  ) where

import Prelude

import Data.Array as Array
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Hylograph.Flow (Flow(..), GroupId, ItemId, PortName(..), StepId(..), WireId(..))
import Hylograph.Flow (GroupId(..)) as Exports

-- | A step, or a named group of groupings.
data Grouping
  = Member StepId
  | Group GroupId String (Array Grouping)

-- | A step of the coarser flow: one kept as it was, or a closed group with
-- | its members' payloads in step order.
data Collapsed step
  = Kept step
  | Closed GroupId (Array step)

derive instance Eq step => Eq (Collapsed step)

-- | Every step under a grouping.
members :: Grouping -> Array StepId
members = case _ of
  Member s -> [ s ]
  Group _ _ inner -> Array.concatMap members inner

-- | The step a closed group becomes.
groupStep :: GroupId -> StepId
groupStep = GroupStep

-- | The coarser flow for a cut: the groups in `closed` are drawn shut; a
-- | closed group shuts everything under it; an open one is transparent.
-- |
-- | Parallel wires are merged: where several members of a closed group read
-- | one item that stays visible, the group reads it by one wire, at the port
-- | of the first of them, carrying what they carried, combined in wire order.
-- | (Likewise for writes, and for any two wires left with one identity.) The
-- | group keeps no port for a wire merged away. So between a closed group
-- | and an item there is at most one wire each way, and no two wires of the
-- | coarser flow share an identity.
-- |
-- | `Semigroup`, not a function argument: the merge has to be associative
-- | for the result not to depend on how the wires happen to be bracketed,
-- | which is the one law the class states, and `Unit` already has it.
collapse :: forall i s w. Semigroup w => Array Grouping -> Set GroupId -> Flow i s w -> Flow i (Collapsed s) w
collapse forest closed (Flow f) = Flow { items, steps, wires }
  where
  shut = Array.concatMap outermost forest
  outermost = case _ of
    Member _ -> []
    g@(Group gid name inner)
      | Set.member gid closed -> [ { gid, name, steps: members g } ]
      | otherwise -> Array.concatMap outermost inner

  owner = Map.fromFoldable (shut # Array.concatMap \g -> g.steps <#> \s -> Tuple s g)
  ownerOf s = Map.lookup s owner
  sameGroup a b = case ownerOf a, ownerOf b of
    Just x, Just y -> x.gid == y.gid
    _, _ -> false

  wireIds = map _.id f.wires

  writers i = Array.mapMaybe
    ( case _ of
        Writes s _ i' | i' == i -> Just s
        _ -> Nothing
    )
    wireIds
  readers i = Array.mapMaybe
    ( case _ of
        Reads i' s _ | i' == i -> Just s
        _ -> Nothing
    )
    wireIds

  -- written in a closed group, read, and read only there
  internal :: ItemId -> Boolean
  internal i = case writers i of
    [ w ] | isJust (ownerOf w) ->
      let rs = readers i in not (Array.null rs) && Array.all (sameGroup w) rs
    _ -> false

  items = Array.filter (not <<< internal <<< _.id) f.items

  attach s p = case ownerOf s of
    Just g -> { step: groupStep g.gid, port: MemberPort s p }
    Nothing -> { step: s, port: p }

  -- every wire that stays, re-attached, each still at its own member's port
  attached = f.wires # Array.mapMaybe \w -> case w.id of
    Reads i s p
      | internal i -> Nothing
      | otherwise -> let a = attach s p in Just { id: Reads i a.step a.port, grouped: isJust (ownerOf s), payload: w.payload }
    Writes s p i
      | internal i -> Nothing
      | otherwise -> let a = attach s p in Just { id: Writes a.step a.port i, grouped: isJust (ownerOf s), payload: w.payload }

  -- the same step and item, the same way round
  parallel a b = case a, b of
    Reads i s _, Reads i' s' _ -> i == i' && s == s'
    Writes s _ i, Writes s' _ i' -> i == i' && s == s'
    _, _ -> false

  -- a closed group's wire moves to the port of the first wire parallel to it
  led = attached <#> \w ->
    if w.grouped then
      case Array.find (\first -> parallel first.id w.id) attached of
        Just first -> w { id = first.id }
        Nothing -> w
    else w

  combined = Array.foldl (\acc w -> Map.insertWith (<>) w.id w.payload acc) Map.empty led

  wires = Array.nub (map _.id led) # Array.mapMaybe \id -> Map.lookup id combined <#> \payload -> { id, payload }

  -- a member's ports that are not hidden, and whose wires were not all
  -- merged into another port's
  openPorts pick isTouching isMergedAway g = g.steps # Array.concatMap \s ->
    case Array.find (\st -> st.id == s) f.steps of
      Just st -> Array.filter (not <<< isMergedAway (groupStep g.gid)) (Array.filter (not <<< isTouching s) (pick st) <#> MemberPort s)
      Nothing -> []
  -- a port is hidden when it has wires and every one of them meets a hidden
  -- item; a port that also meets a visible item stays, with that wire
  readsHidden s p = allHidden $ Array.mapMaybe
    ( case _ of
        Reads i s' p' | s' == s && p' == p -> Just i
        _ -> Nothing
    )
    wireIds
  writesHidden s p = allHidden $ Array.mapMaybe
    ( case _ of
        Writes s' p' i | s' == s && p' == p -> Just i
        _ -> Nothing
    )
    wireIds
  allHidden met = not (Array.null met) && Array.all internal met
  inputAt s p = case _ of
    Reads _ s' p' -> s' == s && p' == p
    Writes _ _ _ -> false
  outputAt s p = case _ of
    Writes s' p' _ -> s' == s && p' == p
    Reads _ _ _ -> false
  mergedAway at s p = Array.any (at s p <<< _.id) attached && not (Array.any (at s p <<< _.id) wires)

  steps = Array.nubBy (comparing _.id) $ f.steps <#> \st -> case ownerOf st.id of
    Nothing -> st { payload = Kept st.payload }
    Just g ->
      { id: groupStep g.gid
      , name: Just g.name
      , inputs: openPorts _.inputs readsHidden (mergedAway inputAt) g
      , outputs: openPorts _.outputs writesHidden (mergedAway outputAt) g
      , payload: Closed g.gid (Array.mapMaybe (\s -> _.payload <$> Array.find (\x -> x.id == s) f.steps) (Array.filter (\s -> Array.elem s g.steps) (map _.id f.steps)))
      }
