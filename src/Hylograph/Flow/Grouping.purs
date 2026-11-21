-- | Grouping beside a flow, never in it: a nested partition of the flow's
-- | steps and items (a rose tree whose leaves are steps and items), and
-- | `collapse`, which draws a cut through it and gives the coarser flow.
-- |
-- | ## What a group names
-- |
-- | `Member` names a step and `Holds` names an item; a `Group` holds either,
-- | and groups. A grouping is well formed when every step, every item and
-- | every group id is in it at most once (`groupingProblems` says what is
-- | not). Nothing ties an item's group to its writer's: a table of entered
-- | data has no writer and belongs to its sheet; a module's output belongs to
-- | its package; an item may be held by one group and written by a step of
-- | another. An item no group holds is treated as 0.1 treated every item (see
-- | the last rule below), so a grouping of steps alone means what it meant.
-- |
-- | ## What a closed group is, in the coarser flow
-- |
-- | The core has two kinds of node and wires only between the two kinds, and
-- | `collapse` keeps to that. A closed group becomes
-- |
-- | - **one step**, `GroupStep` of its id, carrying the payloads of every
-- |   step and every item shut inside it (`Closed`); and
-- | - **at most one item**, its *summary*, `GroupItem` of its id: what the
-- |   group provides. It is there exactly when something leaves the group
-- |   through an item, and it is written by the group's step and by nothing
-- |   else, so the one-writer rule holds of it by construction.
-- |
-- | Which side of the cut an element is on:
-- |
-- | - a step under a closed group is inside it (the outermost closed group
-- |   over it; an open group is transparent);
-- | - an item a closed group holds is inside it, unless an open step writes
-- |   it. An item written by a step that stays drawn stays drawn: it is the
-- |   one thing that can stand between that step and the group, since a wire
-- |   joins an item and a step, and it is not invented;
-- | - an item no closed group holds is inside a closed group only as in 0.1:
-- |   when that group writes it, and it is read, and read only there.
-- |
-- | And then every wire of the finer flow is exactly one of these:
-- |
-- | | the wire | in the coarser flow |
-- | |---|---|
-- | | both ends inside one closed group | gone: it is inside the group |
-- | | neither end inside a closed group | itself |
-- | | a step inside `G`, an item that stays | the same wire at `GroupStep G`, by the member's port (`MemberPort`); parallel ones merged, as in 0.1 |
-- | | an item inside `G` read by a step outside `G` | a read of `G`'s summary: by the reader's own port, or by `SummaryPort G` where the reader is itself a closed group's step |
-- | | an item inside `G` written by a step inside another closed group `H` | a read of `H`'s summary by `GroupStep G`, at `SummaryPort H` |
-- |
-- | Wires left with one identity are merged, their payloads combined with
-- | `<>` in wire order. A summary's one write wire carries the combination of
-- | every wire that became a read of it: what a group provides is what is
-- | taken from it. No wire of the coarser flow is invented: each stands for
-- | at least one wire of the finer flow, which is why `Semigroup` is enough.
-- |
-- | So what crosses a group's boundary is conserved. The wires of the finer
-- | flow with one end inside `G` and one outside are, payload for payload,
-- | the wires of the coarser flow that join `G`'s step or summary to anything
-- | else (the summary's own write wire is inside the group's two nodes and
-- | repeats what the reads of it carry).
-- |
-- | Between two closed groups `G` and `H`, everything that goes from `G` to
-- | `H` through items inside either is one wire, `Reads (GroupItem G)
-- | (GroupStep H) (SummaryPort G)`. Under `Steps` that is one edge from `G`
-- | to `H`, and one from `H` to `G` where each reads the other: a cycle
-- | between two closed groups is two edges, neither dropped. Under `Both` a
-- | providing group is its step, its summary, and the wire between; under
-- | `Holders` it is its summary alone, with an edge from every summary and
-- | item it reads, and a group that provides nothing is no node (as a step
-- | that writes nothing is no edge). A second edge between two closed groups
-- | can only run through an item that stayed drawn, which is an item no
-- | closed group holds.
-- |
-- | ## Identities
-- |
-- | `GroupStep`, `GroupItem`, `MemberPort` and `SummaryPort` are constructors
-- | no step, item or port of a source has. So whatever the source's elements
-- | are called, none is taken for a group's, and a flow already collapsed can
-- | be collapsed again: a `Member` may name a `GroupStep` and a `Holds` a
-- | `GroupItem`. (The one thing left to the caller: a second cut must not
-- | reuse the id of a group the first cut closed, unless it holds that
-- | group's step.)
-- |
-- | One mechanism for a radio's fused runs, for semantic zoom, for the rig
-- | chart's machines opened into their voices, for a workbook by sheet and a
-- | workspace by package.
module Hylograph.Flow.Grouping
  ( module Exports
  , Grouping(..)
  , Collapsed(..)
  , CollapsedItem(..)
  , GroupingProblem(..)
  , members
  , held
  , groupingProblems
  , collapse
  , groupStep
  , groupItem
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldr, sum)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Hylograph.Flow (Flow(..), GroupId, ItemId(..), PortName(..), StepId(..), WireId(..))
import Hylograph.Flow (GroupId(..)) as Exports
import Hylograph.Flow.Internal.Index (gathered, repeated, under)

-- | A step, an item, or a named group of groupings.
data Grouping
  = Member StepId
  | Holds ItemId
  | Group GroupId String (Array Grouping)

-- | A step of the coarser flow: one kept as it was, or a closed group with
-- | the payloads of everything shut inside it: its steps in step order, and
-- | its items in item order. Every step of the finer flow is kept or is in
-- | exactly one closed group's `steps`, and every item is kept or is in
-- | exactly one closed group's `items`.
data Collapsed item step
  = Kept step
  | Closed GroupId { steps :: Array step, items :: Array item }

derive instance (Eq item, Eq step) => Eq (Collapsed item step)

-- | An item of the coarser flow: one kept as it was, or a closed group's
-- | summary with the payloads of the items it stands for, in item order:
-- | those inside the group that something outside reads, and those inside
-- | another closed group that this group writes. There is always one, since
-- | a summary is there only when something leaves through it.
data CollapsedItem item
  = KeptItem item
  | Summary GroupId (NonEmptyArray item)

derive instance Eq item => Eq (CollapsedItem item)

-- | Every step under a grouping.
members :: Grouping -> Array StepId
members = case _ of
  Member s -> [ s ]
  Holds _ -> []
  Group _ _ inner -> Array.concatMap members inner

-- | Every item under a grouping.
held :: Grouping -> Array ItemId
held = case _ of
  Member _ -> []
  Holds i -> [ i ]
  Group _ _ inner -> Array.concatMap held inner

-- | What keeps a forest from being a partition.
data GroupingProblem
  = StepTwice StepId
  | ItemTwice ItemId
  | GroupTwice GroupId

derive instance Eq GroupingProblem

instance Show GroupingProblem where
  show = case _ of
    StepTwice s -> "StepTwice (" <> show s <> ")"
    ItemTwice i -> "ItemTwice (" <> show i <> ")"
    GroupTwice g -> "GroupTwice (" <> show g <> ")"

-- | A forest is well formed when no step, no item and no group id is in it
-- | twice. (`collapse` is total even so: where a forest puts an element
-- | under two closed groups, it is in the later one.)
groupingProblems :: Array Grouping -> Array GroupingProblem
groupingProblems forest =
  map StepTwice (repeated (Array.concatMap members forest))
    <> map ItemTwice (repeated (Array.concatMap held forest))
    <> map GroupTwice (repeated (Array.concatMap groupIds forest))
  where
  groupIds = case _ of
    Member _ -> []
    Holds _ -> []
    Group gid _ inner -> [ gid ] <> Array.concatMap groupIds inner

-- | The step a closed group becomes.
groupStep :: GroupId -> StepId
groupStep = GroupStep

-- | The summary item of a closed group: what it provides.
groupItem :: GroupId -> ItemId
groupItem = GroupItem

-- | The coarser flow for a cut: the groups in `closed` are drawn shut; a
-- | closed group shuts everything under it; an open one is transparent. The
-- | module's header says what each element and each wire becomes.
-- |
-- | Parallel wires are merged: where several members of a closed group read
-- | one item that stays visible, the group reads it by one wire, at the port
-- | of the first of them, carrying what they carried, combined in wire order.
-- | (Likewise for writes, and for any two wires left with one identity.) The
-- | group keeps no port for a wire merged away. So between a closed group
-- | and an item there is at most one wire each way, and no two wires of the
-- | coarser flow share an identity.
-- |
-- | Order: the items that stay, in item order, then the summaries in the
-- | order of their groups in the forest. A closed group's step stands where
-- | the first of its member steps stood; one with no step of the flow under
-- | it (a group of items only) comes after every other step, in forest
-- | order. A closed group with nothing of the flow inside it is no step.
-- | Wires in the order of the first wire each stands for, a summary's write
-- | just before the first read of it.
-- |
-- | `Semigroup`, not a function argument: the merge has to be associative
-- | for the result not to depend on how the wires happen to be bracketed,
-- | which is the one law the class states, and `Unit` already has it.
collapse :: forall i s w. Semigroup w => Array Grouping -> Set GroupId -> Flow i s w -> Flow (CollapsedItem i) (Collapsed i s) w
collapse forest closed (Flow f) = Flow { items, steps, wires }
  where
  -- the outermost closed groups, each with its place among them
  shut = Array.mapWithIndex (\place g -> { place, gid: g.gid, name: g.name, steps: g.steps, items: g.items }) (Array.concatMap outermost forest)
  outermost = case _ of
    Member _ -> []
    Holds _ -> []
    g@(Group gid name inner)
      | Set.member gid closed -> [ { gid, name, steps: members g, items: held g } ]
      | otherwise -> Array.concatMap outermost inner

  -- where a forest puts a step under two closed groups, the later one's
  owner = Map.fromFoldable (shut # Array.concatMap \g -> g.steps <#> \s -> Tuple s g)
  ownerOf s = Map.lookup s owner
  sameGroup a b = case ownerOf a, ownerOf b of
    Just x, Just y -> x.gid == y.gid
    _, _ -> false

  -- likewise an item
  holder = Map.fromFoldable (shut # Array.concatMap \g -> g.items <#> \i -> Tuple i g)

  -- each item's writers and readers, a step once for each of its wires, in
  -- wire order
  writers = gathered $ f.wires # Array.mapMaybe \w -> case w.id of
    Writes s _ i -> Just (Tuple i s)
    Reads _ _ _ -> Nothing
  readers = gathered $ f.wires # Array.mapMaybe \w -> case w.id of
    Reads i s _ -> Just (Tuple i s)
    Writes _ _ _ -> Nothing

  -- The closed group each item is inside, for the items that are inside one.
  -- Held by a closed group: inside it, unless an open step writes it.
  -- Held by none: written in a closed group, read, and read only there.
  inside = Map.union heldInside unheldInside
  heldInside = holder # Map.filterWithKey \i _ -> Array.all (isJust <<< ownerOf) (under i writers)
  unheldInside = Map.mapMaybeWithKey
    ( \i written -> case Map.member i holder, written of
        false, [ w ] -> ownerOf w >>= \g ->
          let rs = under i readers in if not (Array.null rs) && Array.all (sameGroup w) rs then Just g else Nothing
        _, _ -> Nothing
    )
    writers
  groupOf i = Map.lookup i inside
  stays i = not (Map.member i inside)

  attach s p = case ownerOf s of
    Just g -> { step: groupStep g.gid, port: MemberPort s p }
    Nothing -> { step: s, port: p }

  -- a wire through a group's summary: the summary's write, then the read
  through provider reader port item payload =
    [ { id: Writes (groupStep provider.gid) (SummaryPort provider.gid) (groupItem provider.gid)
      , lane: { way: Writing, item: groupItem provider.gid, step: groupStep provider.gid }
      , grouped: false
      , payload
      , provides: Just { place: provider.place, item }
      }
    , { id: Reads (groupItem provider.gid) reader port
      , lane: { way: Reading, item: groupItem provider.gid, step: reader }
      , grouped: false
      , payload
      , provides: Nothing
      }
    ]

  -- every wire that stays, re-attached, each still at its own member's
  -- port, with its lane: the step and the item it joins, and which way.
  -- `grouped`: a closed group's wire to an item that stays
  attached = f.wires # Array.concatMap \w -> case w.id of
    Reads i s p -> case groupOf i of
      Nothing ->
        let a = attach s p in [ { id: Reads i a.step a.port, lane: { way: Reading, item: i, step: a.step }, grouped: isJust (ownerOf s), payload: w.payload, provides: Nothing } ]
      Just g -> case ownerOf s of
        Nothing -> through g s p i w.payload
        Just reading
          | reading.gid == g.gid -> []
          | otherwise -> through g (groupStep reading.gid) (SummaryPort g.gid) i w.payload
    Writes s p i -> case ownerOf s of
      -- an open step's item stays
      Nothing -> [ { id: w.id, lane: { way: Writing, item: i, step: s }, grouped: false, payload: w.payload, provides: Nothing } ]
      Just writing -> case groupOf i of
        Nothing -> [ { id: Writes (groupStep writing.gid) (MemberPort s p) i, lane: { way: Writing, item: i, step: groupStep writing.gid }, grouped: true, payload: w.payload, provides: Nothing } ]
        Just g
          | writing.gid == g.gid -> []
          | otherwise -> through writing (groupStep g.gid) (SummaryPort writing.gid) i w.payload

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

  -- the first step of each id, and the first item with its place (`foldr`,
  -- so that the earliest is put last)
  stepOf = foldr (\st -> Map.insert st.id st) Map.empty f.steps
  itemOf = foldr (\standing -> Map.insert standing.item.id standing) Map.empty (Array.mapWithIndex (\place item -> { place, item }) f.items)

  -- a member's ports that are not hidden, and whose wires were not all
  -- merged into another port's.
  -- A port is hidden when it has wires and every one of them meets an item
  -- inside a closed group (the wire is gone, or is at a summary's port); a
  -- port that also meets an item that stays stays, with that wire
  openPorts pick met before after g = g.steps # Array.concatMap \s ->
    case Map.lookup s stepOf of
      Just st ->
        let
          isHidden p = allInside (under (Tuple s p) met)
          mergedAway p = let at = Tuple (groupStep g.gid) p in Set.member at before && not (Set.member at after)
        in
          Array.filter (not <<< mergedAway) (Array.filter (not <<< isHidden) (pick st) <#> MemberPort s)
      Nothing -> []
  allInside met = not (Array.null met) && not (Array.any stays met)

  -- the summary ports of each closed group's step, in wire order
  summaryInputs = gathered $ wires # Array.mapMaybe \w -> case w.id of
    Reads _ step port@(SummaryPort _) -> Just (Tuple step port)
    _ -> Nothing
  summaryOutputs = gathered $ wires # Array.mapMaybe \w -> case w.id of
    Writes step port@(SummaryPort _) _ -> Just (Tuple step port)
    _ -> Nothing

  -- per closed group, by its place: its members' payloads in step order (a
  -- step said twice is there twice, with the payload of the first), and the
  -- payloads of the items inside it in item order
  placesOf = gathered (shut # Array.concatMap \g -> Array.nub g.steps <#> \s -> Tuple s g.place)
  payloadsOf = gathered $ f.steps # Array.concatMap \st ->
    case Map.lookup st.id stepOf of
      Just firstOfId -> under st.id placesOf <#> \place -> Tuple place firstOfId.payload
      Nothing -> []
  itemsInside = gathered $ f.items # Array.mapMaybe \it -> groupOf it.id <#> \g -> Tuple g.place it.payload

  -- each closed group's step, made once
  groupSteps = shut <#> \g ->
    { id: groupStep g.gid
    , name: Just g.name
    , inputs: openPorts _.inputs readsMet attachedInputs mergedInputs g <> under (groupStep g.gid) summaryInputs
    , outputs: openPorts _.outputs writesMet attachedOutputs mergedOutputs g <> under (groupStep g.gid) summaryOutputs
    , payload: Closed g.gid { steps: under g.place payloadsOf, items: under g.place itemsInside }
    }

  -- a closed group that holds items of the flow, inside it, and has no step
  -- of the flow under it is a step all the same, after the others
  placed = Set.fromFoldable (Array.mapMaybe (\st -> ownerOf st.id <#> _.place) f.steps)
  holding = Set.fromFoldable (Array.mapMaybe (\it -> Map.lookup it.id heldInside <#> _.place) f.items)
  ofItemsOnly = shut # Array.filter \g -> Set.member g.place holding && not (Set.member g.place placed)

  steps =
    ( Array.nubBy (comparing _.id) $ f.steps # Array.mapMaybe \st -> case ownerOf st.id of
        Nothing -> Just (st { payload = Kept st.payload })
        Just g -> Array.index groupSteps g.place
    )
      <> Array.mapMaybe (\g -> Array.index groupSteps g.place) ofItemsOnly

  -- per closed group, by its place: the items its summary stands for
  -- (an item of a wire that is no item of the flow stands for nothing, and
  -- a summary that would stand for nothing but such items is not made: the
  -- flow had that problem already)
  provided = gathered $ attached # Array.mapMaybe \w -> w.provides <#> \p -> Tuple p.place p.item
  summaries = shut # Array.mapMaybe \g -> do
    standing <- NonEmptyArray.fromArray (Array.sortWith _.place (Array.mapMaybe (\i -> Map.lookup i itemOf) (Array.nub (under g.place provided))))
    pure
      { id: groupItem g.gid
      , name: Just g.name
      , count: sum (map _.item.count standing)
      , payload: Summary g.gid (map _.item.payload standing)
      }

  items = Array.mapMaybe (\it -> if stays it.id then Just (it { payload = KeptItem it.payload }) else Nothing) f.items <> summaries

-- | Which way a wire runs, for telling a read of an item from a write.
data Way = Reading | Writing

derive instance Eq Way
derive instance Ord Way
