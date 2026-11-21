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
import Data.Foldable (foldr)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Hylograph.Flow (Flow(..), GroupId, ItemId, PortName(..), StepId(..), WireId(..))
import Hylograph.Flow (GroupId(..)) as Exports
import Hylograph.Flow.Internal.Index (gathered, under)

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
  -- the outermost closed groups, each with its place among them
  shut = Array.mapWithIndex (\place g -> { place, gid: g.gid, name: g.name, steps: g.steps }) (Array.concatMap outermost forest)
  outermost = case _ of
    Member _ -> []
    g@(Group gid name inner)
      | Set.member gid closed -> [ { gid, name, steps: members g } ]
      | otherwise -> Array.concatMap outermost inner

  -- where a forest puts a step under two closed groups, the later one's
  owner = Map.fromFoldable (shut # Array.concatMap \g -> g.steps <#> \s -> Tuple s g)
  ownerOf s = Map.lookup s owner
  sameGroup a b = case ownerOf a, ownerOf b of
    Just x, Just y -> x.gid == y.gid
    _, _ -> false

  -- each item's writers and readers, a step once for each of its wires, in
  -- wire order
  writers = gathered $ f.wires # Array.mapMaybe \w -> case w.id of
    Writes s _ i -> Just (Tuple i s)
    Reads _ _ _ -> Nothing
  readers = gathered $ f.wires # Array.mapMaybe \w -> case w.id of
    Reads i s _ -> Just (Tuple i s)
    Writes _ _ _ -> Nothing

  -- written in a closed group, read, and read only there
  hidden :: Set ItemId
  hidden = Map.keys writers # Set.filter \i -> case under i writers of
    [ w ] | isJust (ownerOf w) ->
      let rs = under i readers in not (Array.null rs) && Array.all (sameGroup w) rs
    _ -> false
  internal i = Set.member i hidden

  items = Array.filter (not <<< internal <<< _.id) f.items

  attach s p = case ownerOf s of
    Just g -> { step: groupStep g.gid, port: MemberPort s p }
    Nothing -> { step: s, port: p }

  -- every wire that stays, re-attached, each still at its own member's
  -- port, with its lane: the step and the item it joins, and which way
  attached = f.wires # Array.mapMaybe \w -> case w.id of
    Reads i s p
      | internal i -> Nothing
      | otherwise -> let a = attach s p in Just { id: Reads i a.step a.port, lane: { way: Reading, item: i, step: a.step }, grouped: isJust (ownerOf s), payload: w.payload }
    Writes s p i
      | internal i -> Nothing
      | otherwise -> let a = attach s p in Just { id: Writes a.step a.port i, lane: { way: Writing, item: i, step: a.step }, grouped: isJust (ownerOf s), payload: w.payload }

  -- the first wire in each lane (`foldr`, so that the earliest is put last)
  first = foldr (\w -> Map.insert w.lane w.id) Map.empty attached

  -- a closed group's wire moves to the port of the first wire parallel to it
  led = attached <#> \w ->
    if w.grouped then
      case Map.lookup w.lane first of
        Just leader -> w { id = leader }
        Nothing -> w
    else w

  combined = Array.foldl (\acc w -> Map.insertWith (<>) w.id w.payload acc) Map.empty led

  wires = Array.nub (map _.id led) # Array.mapMaybe \id -> Map.lookup id combined <#> \payload -> { id, payload }

  -- the items each port of each step of the source meets, each way
  readsMet = gathered $ f.wires # Array.mapMaybe \w -> case w.id of
    Reads i s p -> Just (Tuple (Tuple s p) i)
    Writes _ _ _ -> Nothing
  writesMet = gathered $ f.wires # Array.mapMaybe \w -> case w.id of
    Writes s p i -> Just (Tuple (Tuple s p) i)
    Reads _ _ _ -> Nothing

  -- the ports that wires are at, each way: of those that stay, before the
  -- merge and after it
  inputsOf :: forall r. Array { id :: WireId | r } -> Set (Tuple StepId PortName)
  inputsOf = Set.fromFoldable <<< Array.mapMaybe \w -> case w.id of
    Reads _ s p -> Just (Tuple s p)
    Writes _ _ _ -> Nothing
  outputsOf :: forall r. Array { id :: WireId | r } -> Set (Tuple StepId PortName)
  outputsOf = Set.fromFoldable <<< Array.mapMaybe \w -> case w.id of
    Writes s p _ -> Just (Tuple s p)
    Reads _ _ _ -> Nothing
  attachedInputs = inputsOf attached
  attachedOutputs = outputsOf attached
  mergedInputs = inputsOf wires
  mergedOutputs = outputsOf wires

  -- the first step of each id (`foldr`, so that the earliest is put last)
  stepOf = foldr (\st -> Map.insert st.id st) Map.empty f.steps

  -- a member's ports that are not hidden, and whose wires were not all
  -- merged into another port's.
  -- A port is hidden when it has wires and every one of them meets a hidden
  -- item; a port that also meets a visible item stays, with that wire
  openPorts pick met before after g = g.steps # Array.concatMap \s ->
    case Map.lookup s stepOf of
      Just st ->
        let
          isHidden p = allHidden (under (Tuple s p) met)
          mergedAway p = let at = Tuple (groupStep g.gid) p in Set.member at before && not (Set.member at after)
        in
          Array.filter (not <<< mergedAway) (Array.filter (not <<< isHidden) (pick st) <#> MemberPort s)
      Nothing -> []
  allHidden met = not (Array.null met) && Array.all internal met

  -- per closed group, by its place: its members' payloads in step order (a
  -- step said twice is there twice, with the payload of the first)
  placesOf = gathered (shut # Array.concatMap \g -> Array.nub g.steps <#> \s -> Tuple s g.place)
  payloadsOf = gathered $ f.steps # Array.concatMap \st ->
    case Map.lookup st.id stepOf of
      Just firstOfId -> under st.id placesOf <#> \place -> Tuple place firstOfId.payload
      Nothing -> []

  -- each closed group's step, made once
  groupSteps = shut <#> \g ->
    { id: groupStep g.gid
    , name: Just g.name
    , inputs: openPorts _.inputs readsMet attachedInputs mergedInputs g
    , outputs: openPorts _.outputs writesMet attachedOutputs mergedOutputs g
    , payload: Closed g.gid (under g.place payloadsOf)
    }

  steps = Array.nubBy (comparing _.id) $ f.steps # Array.mapMaybe \st -> case ownerOf st.id of
    Nothing -> Just (st { payload = Kept st.payload })
    Just g -> Array.index groupSteps g.place

-- | Which way a wire runs, for telling a read of an item from a write.
data Way = Reading | Writing

derive instance Eq Way
derive instance Ord Way
