-- | One shared "flow" value: things that hold a value (items) and things that
-- | compute (steps), wired together, as in Build Systems a la Carte's store
-- | and tasks. Domain detail rides in the `item`, `step` and `wire`
-- | parameters and is never inspected here. A flow with nothing to say about
-- | its wires has `Unit` there; one whose wires carry a quantity (a Sankey's
-- | widths) has the quantity.
-- |
-- | What is drawn is not in the value: `project` takes a `Drawing`. Run state
-- | is not in the value: see `Hylograph.Flow.Overlay`. Grouping is not in the
-- | value: see `Hylograph.Flow.Grouping`. Shapes of steps over collections
-- | live one layer up, in `Hylograph.Flow.Collection`.
module Hylograph.Flow
  ( ItemId(..)
  , StepId(..)
  , PortName(..)
  , GroupId(..)
  , stepLabel
  , portLabel
  , Flow(..)
  , Item
  , Step
  , WireId(..)
  , Wire
  , plain
  , wireEnds
  , Imported
  , unFlow
  , mapPayloads
  , erase
  , ElementKey(..)
  , Drawing(..)
  , drawnBy
  , Node
  , Edge
  , Projection
  , project
  , Problem(..)
  , problems
  , writerOf
  , readersOf
  , layers
  , leafMass
  , isolated
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl, maximum)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))

newtype ItemId = ItemId String

derive instance Eq ItemId
derive instance Ord ItemId
instance Show ItemId where
  show (ItemId s) = "ItemId " <> show s

-- | Which group, of a grouping beside a flow (`Hylograph.Flow.Grouping`).
-- | Here, and not there, because a closed group's step is named by it.
newtype GroupId = GroupId String

derive instance Eq GroupId
derive instance Ord GroupId

instance Show GroupId where
  show (GroupId g) = "GroupId " <> show g

-- | Which step. `StepId`: a step of the source, by the name the source gave
-- | it. `GroupStep`: the one step a closed group became under `collapse`.
-- | They are different constructors, so no name a source can give a step is
-- | the identity of a group, whatever the name's letters.
data StepId
  = StepId String
  | GroupStep GroupId

derive instance Eq StepId
derive instance Ord StepId

instance Show StepId where
  show = case _ of
    StepId s -> "StepId " <> show s
    GroupStep g -> "GroupStep (" <> show g <> ")"

-- | Which port of a step. `PortName`: a port of the source's own step.
-- | `MemberPort`: a port of a closed group, which is a port of one of its
-- | members seen from outside: the member and the member's port. The member
-- | may itself be a closed group and its port a `MemberPort`, where a flow
-- | already collapsed is collapsed again.
data PortName
  = PortName String
  | MemberPort StepId PortName

derive instance Eq PortName
derive instance Ord PortName

instance Show PortName where
  show = case _ of
    PortName s -> "PortName " <> show s
    MemberPort step port -> "MemberPort (" <> show step <> ") (" <> show port <> ")"

-- | A step's id as words for a reader: the source's name, or `group:` and
-- | the group's. For display only: two different ids can read the same (a
-- | step the source named `group:G` and the closed group `G`), so nothing
-- | is to be keyed or compared by it.
stepLabel :: StepId -> String
stepLabel = case _ of
  StepId name -> name
  GroupStep (GroupId group) -> "group:" <> group

-- | A port as words for a reader: the source's name, or the member's label,
-- | a dot, and the member's port. For display only, as `stepLabel` is.
portLabel :: PortName -> String
portLabel = case _ of
  PortName name -> name
  MemberPort member port -> stepLabel member <> "." <> portLabel port

-- | What holds a value: a column, a file, a stream, a store. `name` is
-- | `Nothing` only where the source gives the thing no name (a radio
-- | stream, a chart's link). `count`: how many values it holds side by side.
type Item item = { id :: ItemId, name :: Maybe String, count :: Int, payload :: item }

-- | What computes.
type Step step =
  { id :: StepId
  , name :: Maybe String
  , inputs :: Array PortName
  , outputs :: Array PortName
  , payload :: step
  }

-- | Which wire: its two ends and the port at the step end. This is a wire's
-- | whole identity; what it carries is no part of it.
data WireId
  = Reads ItemId StepId PortName
  | Writes StepId PortName ItemId

derive instance Eq WireId
derive instance Ord WireId

instance Show WireId where
  show = case _ of
    Reads i s p -> "(Reads " <> show i <> " " <> show s <> " " <> show p <> ")"
    Writes s p i -> "(Writes " <> show s <> " " <> show p <> " " <> show i <> ")"

-- | A wire and what it carries, shaped as an item and a step are: an id and
-- | a payload. The payload is `Unit` where there is nothing to say.
type Wire wire = { id :: WireId, payload :: wire }

-- | A wire that carries nothing.
plain :: WireId -> Wire Unit
plain id = { id, payload: unit }

newtype Flow item step wire = Flow
  { items :: Array (Item item)
  , steps :: Array (Step step)
  , wires :: Array (Wire wire)
  }

derive instance (Eq item, Eq step, Eq wire) => Eq (Flow item step wire)

unFlow
  :: forall item step wire
   . Flow item step wire
  -> { items :: Array (Item item), steps :: Array (Step step), wires :: Array (Wire wire) }
unFlow (Flow f) = f

-- | A freshly imported flow and what the importer found that does not fit.
type Imported misfit item step wire = { flow :: Flow item step wire, misfits :: Array misfit }

mapPayloads :: forall i i' s s' w w'. (i -> i') -> (s -> s') -> (w -> w') -> Flow i s w -> Flow i' s' w'
mapPayloads onItem onStep onWire (Flow f) = Flow
  { items: f.items <#> \it -> it { payload = onItem it.payload }
  , steps: f.steps <#> \st -> st { payload = onStep st.payload }
  , wires: f.wires <#> \w -> w { payload = onWire w.payload }
  }

erase :: forall i s w. Flow i s w -> Flow Unit Unit Unit
erase = mapPayloads (const unit) (const unit) (const unit)

-- | Any element of a flow, for drawing and for overlays. A wire is keyed by
-- | its identity, never by what it carries.
data ElementKey = ItemKey ItemId | StepKey StepId | WireKey WireId

derive instance Eq ElementKey
derive instance Ord ElementKey

instance Show ElementKey where
  show = case _ of
    ItemKey i -> "(ItemKey " <> show i <> ")"
    StepKey s -> "(StepKey " <> show s <> ")"
    WireKey w -> "(WireKey " <> show w <> ")"

-- ---------------------------------------------------------------------------
-- Well-formedness
-- ---------------------------------------------------------------------------

data Problem
  = UnknownItem ItemId
  | UnknownStep StepId
  | UnknownPort StepId PortName
  | WrittenTwice ItemId
  | DuplicateItem ItemId
  | DuplicateStep StepId
  | DuplicateWire WireId

derive instance Eq Problem

-- | The two elements a wire joins, in the direction it runs.
wireEnds :: WireId -> { from :: ElementKey, to :: ElementKey }
wireEnds = case _ of
  Reads item step _ -> { from: ItemKey item, to: StepKey step }
  Writes step _ item -> { from: StepKey step, to: ItemKey item }

instance Show Problem where
  show = case _ of
    UnknownItem i -> "UnknownItem " <> show i
    UnknownStep s -> "UnknownStep " <> show s
    UnknownPort s p -> "UnknownPort " <> show s <> " " <> show p
    WrittenTwice i -> "WrittenTwice " <> show i
    DuplicateItem i -> "DuplicateItem " <> show i
    DuplicateStep s -> "DuplicateStep " <> show s
    DuplicateWire w -> "DuplicateWire " <> show w

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

writerOf :: forall i s w. Flow i s w -> ItemId -> Array { step :: StepId, port :: PortName, payload :: w }
writerOf (Flow f) item = f.wires # Array.mapMaybe \w -> case w.id of
  Writes s p i | i == item -> Just { step: s, port: p, payload: w.payload }
  _ -> Nothing

readersOf :: forall i s w. Flow i s w -> ItemId -> Array { step :: StepId, port :: PortName, payload :: w }
readersOf (Flow f) item = f.wires # Array.mapMaybe \w -> case w.id of
  Reads i s p | i == item -> Just { step: s, port: p, payload: w.payload }
  _ -> Nothing

-- ---------------------------------------------------------------------------
-- Drawing: which elements are nodes
-- ---------------------------------------------------------------------------

-- | Which elements a picture draws as nodes. Every element not drawn is
-- | contracted: an edge runs between two drawn elements for each path of
-- | wires between them through undrawn ones.
-- |
-- | - `Holders`: items are nodes, each step becomes edges (a spreadsheet's
-- |   columns, a Makefile's files).
-- | - `Steps`: steps are nodes, each item becomes edges (a radio's blocks).
-- | - `Both`: the bipartite graph, every wire an edge.
-- | - `Chosen`: the caller says, element by element. Added beyond the three
-- |   agreed cases, because two subjects (Makefile phony targets drawn
-- |   among files; the rig chart's stores drawn among its processes) draw
-- |   some of each kind, and none of the three reproduces them.
data Drawing
  = Holders
  | Steps
  | Both
  | Chosen (ElementKey -> Boolean)

-- | Whether a drawing draws an element as a node.
drawnBy :: Drawing -> ElementKey -> Boolean
drawnBy = case _ of
  Holders -> case _ of
    ItemKey _ -> true
    _ -> false
  Steps -> case _ of
    StepKey _ -> true
    _ -> false
  Both -> case _ of
    WireKey _ -> false
    _ -> true
  Chosen chosen -> chosen

-- | A drawn element. `label`: its name, or its id where it has none.
-- | `inputs`, `outputs`: a step's ports (empty for an item).
type Node =
  { key :: ElementKey
  , label :: String
  , inputs :: Array PortName
  , outputs :: Array PortName
  }

-- | `via`: the undrawn elements the edge passes through, from source to
-- | target. `fromPort`: the port it leaves a drawn step by; `toPort`: the
-- | port it enters a drawn step by. `wires`: the wires the edge stands for,
-- | each with its identity and what it carries, from source to target. They
-- | are a path: the first leaves `from`, the last enters `to`, and each one
-- | between meets the next at an element of `via`. An edge is at least one
-- | wire: exactly one where the edge is a wire (`Both`), two where it
-- | crosses one undrawn element. A wire on several paths is in several
-- | edges. (`Hylograph.Flow.Laws` states all of this as a function.)
type Edge wire =
  { from :: ElementKey
  , fromPort :: Maybe PortName
  , to :: ElementKey
  , toPort :: Maybe PortName
  , via :: Array ElementKey
  , wires :: NonEmptyArray (Wire wire)
  }

type Projection wire = { nodes :: Array Node, edges :: Array (Edge wire) }

-- | Nodes in array order, items before steps. Edges in the order of the
-- | wire that enters their target.
project :: forall i s w. Drawing -> Flow i s w -> Projection w
project drawing (Flow f) = { nodes, edges }
  where
  drawn = drawnBy drawing
  itemLabel (ItemId s) = s

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
