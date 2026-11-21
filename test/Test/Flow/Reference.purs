-- | The library's functions as they were at 0.1.0, word for word (but for
-- | `collapse`'s result type, which is 0.1's, kept in `Test.Flow.Reference011`,
-- | and one line for a grouping's `Holds`, which is no step): the
-- | definition the faster ones in `src/` are held equal to
-- | (`Test.Flow.Identical`), and the "before" of `Test.Flow.Benchmark`. Slow
-- | by design (each scans every wire inside a loop); nothing here is to be
-- | made faster, and nothing but the tests is to use it.
module Test.Flow.Reference
  ( problems
  , project
  , layers
  , leafMass
  , isolated
  , paths
  , projectionFaults
  , collapse
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl, maximum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isJust, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Hylograph.Flow (Drawing, Edge, ElementKey(..), Flow(..), GroupId, ItemId, PortName(..), Problem(..), Projection, StepId, Wire, WireId(..), drawnBy, itemLabel, stepLabel, wireEnds)
import Hylograph.Flow.Grouping (Grouping(..), groupStep, members)
import Test.Flow.Reference011 (Collapsed(..))

-- | Every wire names an existing item, step and port; each item has at most
-- | one writer; ids are unique, a wire's too (two wires with one identity
-- | could not be told apart by an overlay).
problems :: forall i s w. Flow i s w -> Array Problem
problems (Flow f) = duplicateItems <> duplicateSteps <> duplicateWires <> Array.concatMap wireProblems wireIds <> twice
  where
  wireIds = map _.id f.wires
  duplicateWires = map DuplicateWire (repeated wireIds)
  itemIds = map _.id f.items
  stepIds = map _.id f.steps
  stepMap = Map.fromFoldable (f.steps <#> \st -> Tuple st.id st)
  repeated :: forall a. Ord a => Array a -> Array a
  repeated xs = Array.filter (\x -> Array.length (Array.filter (_ == x) xs) > 1) (Array.nub xs)
  duplicateItems = map DuplicateItem (repeated itemIds)
  duplicateSteps = map DuplicateStep (repeated stepIds)
  item i = if Array.elem i itemIds then [] else [ UnknownItem i ]
  port pick s p = case Map.lookup s stepMap of
    Nothing -> [ UnknownStep s ]
    Just st -> if Array.elem p (pick st) then [] else [ UnknownPort s p ]
  wireProblems = case _ of
    Reads i s p -> item i <> port _.inputs s p
    Writes s p i -> item i <> port _.outputs s p
  written = Array.mapMaybe
    ( case _ of
        Writes _ _ i -> Just i
        Reads _ _ _ -> Nothing
    )
    wireIds
  twice = map WrittenTwice (repeated written)


-- | Nodes in array order, items before steps. Edges in the order of the
-- | wire that enters their target.
project :: forall i s w. Drawing -> Flow i s w -> Projection w
project drawing (Flow f) = { nodes, edges }
  where
  drawn = drawnBy drawing

  nodes =
    Array.mapMaybe
      ( \it -> if drawn (ItemKey it.id) then Just { key: ItemKey it.id, label: fromMaybe (itemLabel it.id) it.name, inputs: [], outputs: [] } else Nothing
      )
      f.items
      <> Array.mapMaybe
        ( \st -> if drawn (StepKey st.id) then Just { key: StepKey st.id, label: fromMaybe (stepLabel st.id) st.name, inputs: st.inputs, outputs: st.outputs } else Nothing
        )
        f.steps

  -- the wires into a step, and into an item, whole
  readsOf s = f.wires # Array.mapMaybe \w -> case w.id of
    Reads i s' _ | s' == s -> Just { item: i, wire: w }
    _ -> Nothing
  writesOf i = f.wires # Array.mapMaybe \w -> case w.id of
    Writes s p i' | i' == i -> Just { step: s, port: p, wire: w }
    _ -> Nothing

  -- the drawn sources an element is reached from, walking wires backwards,
  -- with the wires walked, source first
  backItem :: Set ElementKey -> ItemId -> Array { from :: ElementKey, port :: Maybe PortName, via :: Array ElementKey, wires :: Array (Wire w) }
  backItem seen i
    | drawn (ItemKey i) = [ { from: ItemKey i, port: Nothing, via: [], wires: [] } ]
    | Set.member (ItemKey i) seen = []
    | otherwise =
        writesOf i # Array.concatMap \write ->
          backStep (Set.insert (ItemKey i) seen) write.step (Just write.port) <#> \r ->
            r { via = r.via <> [ ItemKey i ], wires = Array.snoc r.wires write.wire }

  backStep :: Set ElementKey -> StepId -> Maybe PortName -> Array { from :: ElementKey, port :: Maybe PortName, via :: Array ElementKey, wires :: Array (Wire w) }
  backStep seen s port
    | drawn (StepKey s) = [ { from: StepKey s, port, via: [], wires: [] } ]
    | Set.member (StepKey s) seen = []
    | otherwise =
        readsOf s # Array.concatMap \read ->
          backItem (Set.insert (StepKey s) seen) read.item <#> \r ->
            r { via = r.via <> [ StepKey s ], wires = Array.snoc r.wires read.wire }

  entering w = case w.id of
    Reads i s p | drawn (StepKey s) ->
      backItem Set.empty i <#> \r ->
        { from: r.from, fromPort: r.port, to: StepKey s, toPort: Just p, via: r.via, wires: NonEmptyArray.snoc' r.wires w }
    Writes s p i | drawn (ItemKey i) ->
      backStep Set.empty s (Just p) <#> \r ->
        { from: r.from, fromPort: r.port, to: ItemKey i, toPort: Nothing, via: r.via, wires: NonEmptyArray.snoc' r.wires w }
    _ -> []

  edges = Array.concatMap entering f.wires

-- | Longest path from a node with no incoming edge, over the projection.
-- | Self-loops are ignored; nodes on a cycle get the layer they had when
-- | the cycle was met.
layers :: forall w. Projection w -> Map ElementKey Int
layers p = foldl visit Map.empty (topological p)
  where
  visit acc key =
    let
      feeders = Array.mapMaybe (\e -> if e.to == key && e.from /= key then Map.lookup e.from acc else Nothing) p.edges
    in
      Map.insert key (fromMaybe (-1) (maximum feeders) + 1) acc

-- | Kahn's order, ties by node order; anything left (a cycle) at the end.
topological :: forall w. Projection w -> Array ElementKey
topological p = go [] (map _.key p.nodes)
  where
  go done remaining =
    case Array.find ready remaining of
      Just next -> go (Array.snoc done next) (Array.filter (_ /= next) remaining)
      Nothing -> done <> remaining
    where
    ready key = Array.all (\e -> e.to /= key || e.from == key || Array.elem e.from done) p.edges

-- | Per node, the set of nodes with no incoming edge that reach it (itself,
-- | for such a node).
leafMass :: forall w. Projection w -> Map ElementKey (Set ElementKey)
leafMass p = foldl visit Map.empty (topological p)
  where
  visit acc key =
    let
      feeders = Array.nub (Array.mapMaybe (\e -> if e.to == key then Just e.from else Nothing) p.edges)
    in
      if Array.null feeders then Map.insert key (Set.singleton key) acc
      else Map.insert key (Set.unions (feeders <#> \x -> fromMaybe Set.empty (Map.lookup x acc))) acc

-- | Nodes no edge touches, in node order.
isolated :: forall w. Projection w -> Array ElementKey
isolated p = Array.filter lonely (map _.key p.nodes)
  where
  lonely key = isNothing (Array.find (\e -> e.from == key || e.to == key) p.edges)

-- | Every path of wires from a drawn element to a drawn element through
-- | undrawn ones, no undrawn element twice on a path. Found walking forwards
-- | from each wire that leaves a drawn element, where `project` walks
-- | backwards from each wire that enters one.
paths :: forall i s w. Drawing -> Flow i s w -> Array (NonEmptyArray WireId)
paths drawing (Flow f) = Array.concatMap starting wireIds
  where
  drawn = drawnBy drawing
  wireIds = map _.id f.wires
  leaving element = Array.filter (\w -> (wireEnds w).from == element) wireIds

  starting :: WireId -> Array (NonEmptyArray WireId)
  starting w = if drawn (wireEnds w).from then onward Set.empty (NonEmptyArray.singleton w) else []

  onward :: Set ElementKey -> NonEmptyArray WireId -> Array (NonEmptyArray WireId)
  onward seen walked =
    let
      reached = (wireEnds (NonEmptyArray.last walked)).to
    in
      if drawn reached then [ walked ]
      else if Set.member reached seen then []
      else leaving reached # Array.concatMap \next -> onward (Set.insert reached seen) (NonEmptyArray.snoc walked next)

-- | Every fault in the projection of a flow under a drawing, in words; none
-- | is the law.
-- |
-- | - every wire of every edge is a wire of the flow;
-- | - an edge's wires are a path: the first leaves `from`, the last enters
-- |   `to`, each one between meets the next at the element of `via` in that
-- |   place, both ends are drawn and nothing in `via` is; `fromPort` and
-- |   `toPort` are the ports of the first and last wire where those ends
-- |   are steps;
-- | - the edges are exactly the paths of wires from a drawn element to a
-- |   drawn element through undrawn ones (`paths`), each path one edge.
-- |
-- | So a wire between two drawn elements is one edge, alone in it; a wire
-- | with an undrawn end is in one edge for each such path through it, which
-- | may be several (a step with two inputs, contracted, puts its output
-- | wire in two edges) or none (nothing drawn is reached that way). Those
-- | two are restated as checks of their own, though they follow.
projectionFaults :: forall i s w. Drawing -> Flow i s w -> Array String
projectionFaults drawing flow@(Flow f) = unknown <> broken <> ports <> missing <> invented <> repeated <> direct <> counted
  where
  drawn = drawnBy drawing
  projection = project drawing flow
  flowWires = map _.id f.wires
  expected = paths drawing flow

  idsOf :: Edge w -> Array WireId
  idsOf edge = NonEmptyArray.toArray (map _.id edge.wires)
  edgePaths = map idsOf projection.edges
  describe :: Edge w -> String
  describe edge = "the edge " <> show edge.from <> " -> " <> show edge.to <> " via " <> show edge.via

  unknown = projection.edges # Array.concatMap \edge ->
    Array.filter (\w -> not (Array.elem w flowWires)) (idsOf edge) <#> \w -> describe edge <> " holds " <> show w <> ", which is no wire of the flow"

  -- the elements along an edge, as its wires say
  isPath edge =
    let
      ends = map wireEnds (idsOf edge)
    in
      map _.from ends == [ edge.from ] <> edge.via && map _.to ends == edge.via <> [ edge.to ]
  broken = projection.edges # Array.concatMap \edge ->
    (if isPath edge then [] else [ describe edge <> " has wires that are not a path along it" ])
      <> (if drawn edge.from && drawn edge.to then [] else [ describe edge <> " has an end that is not drawn" ])
      <> (if Array.any drawn edge.via then [ describe edge <> " passes through a drawn element" ] else [])

  portOut = case _ of
    Writes _ port _ -> Just port
    Reads _ _ _ -> Nothing
  portIn = case _ of
    Reads _ _ port -> Just port
    Writes _ _ _ -> Nothing
  ports = projection.edges # Array.concatMap \edge ->
    if edge.fromPort == portOut (NonEmptyArray.head edge.wires).id && edge.toPort == portIn (NonEmptyArray.last edge.wires).id then []
    else [ describe edge <> " has a port that is not its first or last wire's" ]

  missing = Array.filter (\path -> not (Array.elem (NonEmptyArray.toArray path) edgePaths)) expected <#> \path ->
    "no edge stands for the path " <> show (NonEmptyArray.toArray path)
  invented = Array.filter (\path -> not (Array.elem path (map NonEmptyArray.toArray expected))) edgePaths <#> \path ->
    "an edge stands for " <> show path <> ", which is no path between drawn elements"
  repeated = Array.filter (\path -> Array.length (Array.filter (_ == path) edgePaths) > 1) (Array.nub edgePaths) <#> \path ->
    "more than one edge stands for the path " <> show path

  holding w = Array.filter (Array.elem w) edgePaths
  bothDrawn w = drawn (wireEnds w).from && drawn (wireEnds w).to
  direct = Array.filter bothDrawn flowWires # Array.concatMap \w ->
    if holding w == [ [ w ] ] then [] else [ show w <> " joins two drawn elements and is not exactly one edge, alone" ]
  counted = flowWires # Array.concatMap \w ->
    let
      through = Array.length (Array.filter (Array.elem w <<< NonEmptyArray.toArray) expected)
    in
      if Array.length (holding w) == through then []
      else [ show w <> " is in " <> show (Array.length (holding w)) <> " edges and on " <> show through <> " paths" ]


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
    Holds _ -> []
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
