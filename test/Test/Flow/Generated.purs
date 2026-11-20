-- | What must hold of every flow, held on a few hundred small well-formed
-- | flows made from a fixed seed, so that a run is the same run every time.
module Test.Flow.Generated (suite) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (all)
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), Edge, ElementKey(..), Flow(..), ItemId(..), PortName(..), Problem(..), Projection, StepId(..), Wire, WireId(..), erase, mapPayloads, problems, project, unFlow)
import Hylograph.Flow.Grouping (Collapsed(..), GroupId(..), Grouping(..), collapse, groupStep)
import Hylograph.Flow.Laws (projectionFaults)
import Test.Flow.Check (expect, section)
import Test.QuickCheck (Result(..), mkSeed, quickCheckPure)
import Test.QuickCheck.Gen (Gen, chooseInt, shuffle)

-- | Items carry a number, steps a word, wires a list, which has a
-- | `Semigroup` whose order of combination shows.
type Made = Flow Int String (Array Int)

-- | Whether one port may meet several wires.
data Ports = OneWireEach | Shared

upTo :: Int -> Array Int
upTo count = if count <= 0 then [] else Array.range 0 (count - 1)

chance :: Gen Boolean
chance = map (_ == 1) (chooseInt 0 1)

-- | Some of an array, in a random order.
someOf :: forall a. Int -> Array a -> Gen (Array a)
someOf most xs = ado
  shuffled <- shuffle xs
  count <- chooseInt 0 most
  in Array.take count shuffled

-- | A flow with no problems, by construction: up to four items and three
-- | steps, each step with up to two ports each way, each read by a port its
-- | step has, each item written at most once. Arrays in a random order;
-- | some names missing.
flowOf :: Ports -> Gen Made
flowOf ports = do
  itemCount <- chooseInt 0 4
  stepCount <- chooseInt 0 3
  items <- for (upTo itemCount) \index -> chance <#> \named ->
    { id: ItemId ("i" <> show index), name: if named then Just ("item " <> show index) else Nothing, count: 1, payload: index }
  steps <- for (upTo stepCount) \index -> ado
    inputCount <- chooseInt 0 2
    outputCount <- chooseInt 0 2
    in
      { id: StepId ("s" <> show index)
      , name: Just ("step " <> show index)
      , inputs: upTo inputCount <#> \port -> PortName ("in" <> show port)
      , outputs: upTo outputCount <#> \port -> PortName ("out" <> show port)
      , payload: "step " <> show index
      }
  let itemIds = map _.id items
  let inputs = steps # Array.concatMap \st -> st.inputs <#> \port -> { step: st.id, port }
  let outputs = steps # Array.concatMap \st -> st.outputs <#> \port -> { step: st.id, port }
  let
    most = case ports of
      OneWireEach -> 1
      Shared -> 2
  readsOf <- for inputs \input -> someOf most itemIds <#> map \read -> Reads read input.step input.port
  writesOf <- case ports of
    -- each output port writes at most one item, each item is written at most once
    OneWireEach -> ado
      shuffledOutputs <- shuffle outputs
      shuffledItems <- shuffle itemIds
      count <- chooseInt 0 (min (Array.length outputs) (Array.length items))
      in Array.take count (Array.zipWith (\output written -> Writes output.step output.port written) shuffledOutputs shuffledItems)
    -- each item is written at most once, by any port
    Shared -> map Array.catMaybes $ for itemIds \written -> ado
      isWritten <- chance
      pick <- chooseInt 0 (max 0 (Array.length outputs - 1))
      in if isWritten then Array.index outputs pick <#> \output -> Writes output.step output.port written else Nothing
  wireIds <- shuffle (Array.concat readsOf <> writesOf)
  shuffledItems <- shuffle items
  shuffledSteps <- shuffle steps
  pure (Flow { items: shuffledItems, steps: shuffledSteps, wires: Array.mapWithIndex (\index id -> { id, payload: [ index ] }) wireIds })

elementsOf :: forall i s w. Flow i s w -> Array ElementKey
elementsOf (Flow f) = map (ItemKey <<< _.id) f.items <> map (StepKey <<< _.id) f.steps

-- | Some of a flow's items and steps.
chosenOf :: forall i s w. Flow i s w -> Gen (Set ElementKey)
chosenOf flow = map (Set.fromFoldable <<< Array.catMaybes) $ for (elementsOf flow) \key -> chance <#> \isDrawn -> if isDrawn then Just key else Nothing

-- | The three fixed drawings and the drawing of a given set.
drawingsWith :: Set ElementKey -> Array { name :: String, drawing :: Drawing }
drawingsWith chosen =
  [ { name: "Holders", drawing: Holders }
  , { name: "Steps", drawing: Steps }
  , { name: "Both", drawing: Both }
  , { name: "Chosen " <> show (Array.fromFoldable chosen), drawing: Chosen \key -> Set.member key chosen }
  ]

describe :: Made -> String
describe (Flow f) =
  "items " <> show (map _.id f.items)
    <> "; steps "
    <> show (f.steps <#> \st -> { id: st.id, inputs: st.inputs, outputs: st.outputs })
    <> "; wires "
    <> show (map _.id f.wires)

-- | A flow with its three arrays each in another order.
type Reordered = { flow :: Made, reordered :: Made, chosen :: Set ElementKey }

reorderedOf :: Ports -> Gen Reordered
reorderedOf ports = do
  flow <- flowOf ports
  chosen <- chosenOf flow
  let f = unFlow flow
  items <- shuffle f.items
  steps <- shuffle f.steps
  wires <- shuffle f.wires
  pure { flow, reordered: Flow { items, steps, wires }, chosen }

unitWires :: forall wire. Edge wire -> Edge Unit
unitWires edge = edge { wires = map (_ { payload = unit }) edge.wires }

-- | A flow, a forest over its steps (two top groups, one with a group
-- | inside it, and steps in none) and some of the groups shut.
type Grouped = { flow :: Made, forest :: Array Grouping, closed :: Set GroupId }

groupedOf :: Ports -> Gen Grouped
groupedOf ports = do
  flow <- flowOf ports
  placed <- for (unFlow flow).steps \st -> chooseInt 0 3 <#> \place -> { step: st.id, place }
  closed <- someOf 3 [ GroupId "first", GroupId "nested", GroupId "second" ]
  let membersAt place = Array.filter (\p -> p.place == place) placed <#> \p -> Member p.step
  pure
    { flow
    , forest:
        [ Group (GroupId "first") "First" (membersAt 1 <> [ Group (GroupId "nested") "Nested" (membersAt 2) ])
        , Group (GroupId "second") "Second" (membersAt 3)
        ]
          <> membersAt 0
    , closed: Set.fromFoldable closed
    }

describeGrouped :: Grouped -> String
describeGrouped g = describe g.flow <> "; closed " <> show (Array.fromFoldable g.closed) <> "; forest " <> show (map showGrouping g.forest)
  where
  showGrouping = case _ of
    Member s -> show s
    Group gid _ within -> show gid <> " " <> show (map showGrouping within)

-- | Each fault as a failure that says which flow it was.
verdict :: String -> Array String -> Result
verdict subject faults = case Array.head faults of
  Nothing -> Success
  Just fault -> Failed (fault <> "\n      in: " <> subject)

-- | Run a property on `count` made subjects from a fixed seed; hold when
-- | none fails, and say the first that does.
holds :: String -> Int -> Gen Result -> Effect Unit
holds label count property = do
  let results = quickCheckPure (mkSeed 20261120) count property :: List Result
  let
    failures = Array.fromFoldable results # Array.mapMaybe case _ of
      Success -> Nothing
      Failed message -> Just message
  case Array.head failures of
    Just message -> log ("    counterexample (" <> show (Array.length failures) <> " of " <> show count <> " fail): " <> message)
    Nothing -> pure unit
  expect (label <> " (" <> show count <> " made flows)") (Array.null failures)

-- | The node keys in order; the last wire of each edge.
nodeKeys :: forall wire. Projection wire -> Array ElementKey
nodeKeys projection = map _.key projection.nodes

lastWire :: forall wire. Edge wire -> Wire wire
lastWire edge = NonEmptyArray.last edge.wires

nonDecreasing :: Array Int -> Boolean
nonDecreasing xs = all identity (Array.zipWith (<=) xs (Array.drop 1 xs))

-- | What `collapse` promises of any flow without problems, apart from the
-- | flow's being well formed (see `Test.Flow.Grouping` for where that fails).
collapseFaults :: Grouped -> Flow Int (Collapsed String) (Array Int) -> Array String
collapseFaults g coarser =
  (if Array.length (Array.nub ids) == Array.length ids then [] else [ "two wires of the coarser flow share an identity" ])
    <> (if Array.length (Array.nub stepIds) == Array.length stepIds then [] else [ "two steps of the coarser flow share an id" ])
    <> (if Array.sort carried == Array.sort (Array.concatMap _.payload surviving) then [] else [ "the coarser flow's wires do not carry exactly what the wires that stayed carried" ])
    <> (if all (\w -> Array.length (Array.filter (sameWay w) ids) == 1) (Array.filter atGroup ids) then [] else [ "a closed group and an item are joined by two wires the same way" ])
  where
  f = unFlow coarser
  ids = map _.id f.wires
  stepIds = map _.id f.steps
  carried = Array.concatMap _.payload f.wires
  keptItems = map _.id f.items
  -- a wire stays exactly when its item does
  surviving = Array.filter (\w -> Array.elem (itemOf w.id) keptItems) (unFlow g.flow).wires
  itemOf = case _ of
    Reads i _ _ -> i
    Writes _ _ i -> i
  groupSteps = map groupStep [ GroupId "first", GroupId "nested", GroupId "second" ]
  atGroup = case _ of
    Reads _ s _ -> Array.elem s groupSteps
    Writes s _ _ -> Array.elem s groupSteps
  sameWay a b = case a, b of
    Reads i s _, Reads i' s' _ -> i == i' && s == s'
    Writes s _ i, Writes s' _ i' -> i == i' && s == s'
    _, _ -> false

suite :: Effect Unit
suite = do
  section "made flows: the generator makes what it says"
  holds "a made flow has no problems, one wire to a port" 300 $ flowOf OneWireEach <#> \flow ->
    verdict (describe flow) (map show (problems flow))
  holds "a made flow has no problems, ports shared" 300 $ flowOf Shared <#> \flow ->
    verdict (describe flow) (map show (problems flow))

  section "made flows: project"
  holds "the projection law holds under Holders, Steps, Both and a chosen drawing" 300 $ reorderedOf Shared <#> \made ->
    verdict (describe made.flow) (drawingsWith made.chosen # Array.concatMap \d -> map (\fault -> d.name <> ": " <> fault) (projectionFaults d.drawing made.flow))
  holds "every edge's two ends are drawn nodes, and nothing an edge passes through is" 300 $ reorderedOf Shared <#> \made ->
    verdict (describe made.flow) $ drawingsWith made.chosen # Array.concatMap \d ->
      let
        projection = project d.drawing made.flow
        keys = nodeKeys projection
      in
        projection.edges # Array.concatMap \edge ->
          (if Array.elem edge.from keys && Array.elem edge.to keys then [] else [ d.name <> ": an edge " <> show edge.from <> " -> " <> show edge.to <> " has an end that is no node" ])
            <> (if Array.any (\through -> Array.elem through keys) edge.via then [ d.name <> ": an edge passes through a node" ] else [])
  holds "erasing payloads commutes with project: the same nodes, the same edges with nothing carried" 300 $ reorderedOf Shared <#> \made ->
    verdict (describe made.flow) $ drawingsWith made.chosen # Array.concatMap \d ->
      let
        before = project d.drawing made.flow
        after = project d.drawing (erase made.flow)
      in
        if after.nodes == before.nodes && after.edges == map unitWires before.edges then [] else [ d.name <> ": the projection of the erased flow is not the erased projection" ]
  holds "the arrays' order decides only the order: reordered, the same nodes and the same edges" 300 $ reorderedOf Shared <#> \made ->
    verdict (describe made.flow <> "\n      reordered: " <> describe made.reordered) $ drawingsWith made.chosen # Array.concatMap \d ->
      let
        one = project d.drawing made.flow
        other = project d.drawing made.reordered
      in
        (if Array.sort one.nodes == Array.sort other.nodes then [] else [ d.name <> ": the nodes differ" ])
          <> (if Array.sort one.edges == Array.sort other.edges then [] else [ d.name <> ": the edges differ" ])
  holds "nodes are in array order, items before steps; edges in the order of the wire that enters their target" 300 $ reorderedOf Shared <#> \made ->
    verdict (describe made.flow) $ drawingsWith made.chosen # Array.concatMap \d ->
      let
        projection = project d.drawing made.flow
        isDrawn key = case d.drawing of
          Holders -> isItem key
          Steps -> not (isItem key)
          Both -> true
          Chosen _ -> Set.member key made.chosen
        wires = (unFlow made.flow).wires
        places = Array.mapMaybe (\edge -> Array.findIndex (_ == lastWire edge) wires) projection.edges
      in
        (if nodeKeys projection == Array.filter isDrawn (elementsOf made.flow) then [] else [ d.name <> ": the nodes are not the drawn elements in array order" ])
          <> (if Array.length places == Array.length projection.edges && nonDecreasing places then [] else [ d.name <> ": the edges are not in the order of their last wires" ])
  holds "project is a function: the same flow drawn twice is the same projection" 100 $ reorderedOf Shared <#> \made ->
    verdict (describe made.flow) $ drawingsWith made.chosen # Array.concatMap \d ->
      if project d.drawing made.flow == project d.drawing made.flow then [] else [ d.name <> ": two projections of one flow differ" ]

  section "made flows: collapse"
  holds "with nothing shut, collapse is mapPayloads identity Kept identity, whatever the forest" 300 $ groupedOf Shared <#> \g ->
    verdict (describeGrouped g) (if collapse g.forest Set.empty g.flow == mapPayloads identity Kept identity g.flow then [] else [ "collapsing nothing changed the flow" ])
  holds "one wire to a port: the coarser flow has no problems" 500 $ groupedOf OneWireEach <#> \g ->
    verdict (describeGrouped g) (map show (problems (collapse g.forest g.closed g.flow)))
  holds "one wire to a port: the coarser flow's projections are lawful" 300 $ groupedOf OneWireEach <#> \g ->
    verdict (describeGrouped g) ([ Holders, Steps, Both ] # Array.concatMap \drawing -> projectionFaults drawing (collapse g.forest g.closed g.flow))
  holds "ports shared or not: no two wires share an identity, a group and an item have one wire each way, and nothing carried is lost or made" 500 $ groupedOf Shared <#> \g ->
    verdict (describeGrouped g) (collapseFaults g (collapse g.forest g.closed g.flow))
  holds "ports shared: the coarser flow has no problems (a port that still has a visible wire stays)" 500 $ groupedOf Shared <#> \g ->
    verdict (describeGrouped g) (map show (problems (collapse g.forest g.closed g.flow)))
  holds "ports shared: the coarser flow's projections are lawful" 300 $ groupedOf Shared <#> \g ->
    verdict (describeGrouped g) ([ Holders, Steps, Both ] # Array.concatMap \drawing -> projectionFaults drawing (collapse g.forest g.closed g.flow))
  where
  isItem = case _ of
    ItemKey _ -> true
    _ -> false
