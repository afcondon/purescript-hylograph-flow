-- | The library's functions give what the 0.1.0 ones gave
-- | (`Test.Flow.Reference`), array for array and in the same order: on the
-- | fixtures, on small made flows with no problems, on small made flows with
-- | every kind of problem (ids said twice, wires to nothing, a step under
-- | two groups, two groups of one id), on projections no flow would give,
-- | and on the benchmark's shapes at a size the old functions can afford.
module Test.Flow.Identical (suite) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (for_)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Effect (Effect)
import Hylograph.Flow (Drawing(..), ElementKey(..), Flow(..), ItemId(..), PortName(..), Projection, StepId(..), WireId(..), isolated, layers, leafMass, plain, problems, project, unFlow)
import Hylograph.Flow.Grouping (GroupId(..), Grouping(..), collapse)
import Hylograph.Flow.Laws (paths, projectionFaults)
import Test.Flow.Check (expect, expectNone, section)
import Test.Flow.Fixtures (drawings, fixtures)
import Test.Flow.Generated (Made, Ports(..), chance, chosenOf, describe, describeGrouped, drawingsWith, flowOf, groupedOf, holds, someOf, upTo, verdict)
import Test.Flow.Reference as Reference
import Test.Flow.Shapes (halfTheSteps, shapes)
import Test.QuickCheck.Gen (Gen, chooseInt)

-- | Where the two differ on a flow under a drawing: the projection, what is
-- | read off it, the paths and the law's faults.
drawnDifferently :: forall item step wire. Eq wire => String -> Drawing -> Flow item step wire -> Array String
drawnDifferently name drawing flow =
  differs "project" (now == before)
    <> readDifferently name now
    <> differs "paths" (paths drawing flow == Reference.paths drawing flow)
    <> differs "projectionFaults" (projectionFaults drawing flow == Reference.projectionFaults drawing flow)
  where
  now = project drawing flow
  before = Reference.project drawing flow
  differs what same = if same then [] else [ name <> ": " <> what <> " differs" ]

-- | Where the two differ in what they read off a projection.
readDifferently :: forall wire. String -> Projection wire -> Array String
readDifferently name projection =
  differs "layers" (layers projection == Reference.layers projection)
    <> differs "leafMass" (leafMass projection == Reference.leafMass projection)
    <> differs "isolated" (isolated projection == Reference.isolated projection)
  where
  differs what same = if same then [] else [ name <> ": " <> what <> " differs" ]

-- | Where the two differ on a flow whatever is drawn or shut.
differently :: forall item step wire. Eq item => Eq step => Eq wire => Semigroup wire => Array { name :: String, drawing :: Drawing } -> Array Grouping -> Array (Set GroupId) -> Flow item step wire -> Array String
differently drawn forest cuts flow =
  (if problems flow == Reference.problems flow then [] else [ "problems differs" ])
    <> Array.concatMap (\d -> drawnDifferently d.name d.drawing flow) drawn
    <> Array.concatMap (\cut -> if collapse forest cut flow == Reference.collapse forest cut flow then [] else [ "collapse differs, shut " <> show (Array.fromFoldable cut) ]) cuts

-- | One of an array, or the given where there is none.
oneOf :: forall a. a -> Array a -> Gen a
oneOf otherwise xs = chooseInt 0 (max 0 (Array.length xs - 1)) <#> \place -> fromMaybe otherwise (Array.index xs place)

-- | Some of an array, any of it any number of times.
anyOf :: forall a. Int -> a -> Array a -> Gen (Array a)
anyOf most otherwise xs = chooseInt 0 most >>= \count -> for (upTo count) \_ -> oneOf otherwise xs

groupIds :: Array GroupId
groupIds = [ GroupId "first", GroupId "nested", GroupId "second" ]

stepPool :: Array StepId
stepPool = [ StepId "s0", StepId "s1", StepId "s2", GroupStep (GroupId "first"), StepId "nobody" ]

itemPool :: Array ItemId
itemPool = [ ItemId "i0", ItemId "i1", ItemId "i2", ItemId "ghost" ]

portPool :: Array PortName
portPool = [ PortName "in0", PortName "out0", PortName "both", MemberPort (StepId "s0") (PortName "in0") ]

-- | A flow with any problem there is: items and steps drawn from a few ids
-- | with repeats, wires between any of them and to nothing, by any port. A
-- | forest to match: steps under several groups, a group's id used twice,
-- | members that are no steps.
type Loose = { flow :: Made, forest :: Array Grouping, closed :: Set GroupId, chosen :: Set ElementKey }

looseOf :: Gen Loose
looseOf = do
  itemIds <- anyOf 5 (ItemId "i0") (Array.take 3 itemPool)
  stepIds <- anyOf 4 (StepId "s0") (Array.take 4 stepPool)
  steps <- for (Array.mapWithIndex (\place id -> { place, id }) stepIds) \st -> ado
    inputs <- anyOf 2 (PortName "in0") portPool
    outputs <- anyOf 2 (PortName "out0") portPool
    in { id: st.id, name: Just ("step " <> show st.place), inputs, outputs, payload: "step " <> show st.place }
  wireCount <- chooseInt 0 9
  wireIds <- for (upTo wireCount) \_ -> ado
    isRead <- chance
    item <- oneOf (ItemId "i0") itemPool
    step <- oneOf (StepId "s0") stepPool
    port <- oneOf (PortName "in0") portPool
    in if isRead then Reads item step port else Writes step port item
  firstMembers <- anyOf 3 (StepId "s0") stepPool
  nestedMembers <- anyOf 2 (StepId "s0") stepPool
  secondMembers <- anyOf 3 (StepId "s0") stepPool
  againMembers <- anyOf 2 (StepId "s0") stepPool
  closed <- someOf 3 groupIds
  let
    flow = Flow
      { items: Array.mapWithIndex (\place id -> { id, name: Nothing, count: 1, payload: place }) itemIds
      , steps
      , wires: Array.mapWithIndex (\place id -> { id, payload: [ place ] }) wireIds
      }
  chosen <- chosenOf flow
  pure
    { flow
    , forest:
        [ Group (GroupId "first") "First" (map Member firstMembers <> [ Group (GroupId "nested") "Nested" (map Member nestedMembers) ])
        , Group (GroupId "second") "Second" (map Member secondMembers)
        , Group (GroupId "first") "First again" (map Member againMembers)
        ]
    , closed: Set.fromFoldable closed
    , chosen
    }

-- | A projection as no flow gives one: nodes said twice, edges from and to
-- | what is no node, loops, rings.
projectionOf :: Gen (Projection Unit)
projectionOf = do
  nodes <- anyOf 6 (ItemKey (ItemId "i0")) (Array.take 5 keys)
  edgeCount <- chooseInt 0 9
  edges <- for (upTo edgeCount) \_ -> ado
    from <- oneOf (ItemKey (ItemId "i0")) keys
    to <- oneOf (ItemKey (ItemId "i0")) keys
    in { from, fromPort: Nothing, to, toPort: Nothing, via: [], wires: NonEmptyArray.singleton (plain (Reads (ItemId "i0") (StepId "s0") (PortName "in0"))) }
  pure { nodes: nodes <#> \key -> { key, label: show key, inputs: [], outputs: [] }, edges }
  where
  keys = [ ItemKey (ItemId "i0"), ItemKey (ItemId "i1"), StepKey (StepId "s0"), StepKey (StepId "s1"), StepKey (StepId "s2"), StepKey (StepId "outside") ]

-- | Every way of shutting the three groups.
everyCut :: Array (Set GroupId)
everyCut = subsets groupIds <#> Set.fromFoldable
  where
  subsets :: forall a. Array a -> Array (Array a)
  subsets xs = case Array.uncons xs of
    Nothing -> [ [] ]
    Just { head, tail } -> let rest = subsets tail in map (Array.cons head) rest <> rest

suite :: Effect Unit
suite = do
  section "identical to 0.1.0: the fixtures"
  for_ fixtures \fixture -> do
    let
      steps = map _.id (unFlow fixture.flow).steps
      forest = [ Group (GroupId "first") "First" (map Member (Array.take 1 steps)), Group (GroupId "second") "Second" (map Member (Array.drop 1 steps)) ]
    expectNone ("the fixture " <> fixture.name <> ": problems, every drawing, every cut of two groups")
      (differently drawings forest [ Set.empty, Set.singleton (GroupId "first"), Set.singleton (GroupId "second"), Set.fromFoldable [ GroupId "first", GroupId "second" ] ] fixture.flow)

  section "identical to 0.1.0: made flows"
  holds "flows with no problems, ports shared: problems, the four drawings, every cut of the forest" 400 $ groupedOf Shared >>= \g -> chosenOf g.flow <#> \chosen ->
    verdict (describeGrouped g) (differently (drawingsWith chosen) g.forest everyCut g.flow)
  holds "flows with no problems, one wire to a port: likewise" 300 $ groupedOf OneWireEach >>= \g -> chosenOf g.flow <#> \chosen ->
    verdict (describeGrouped g) (differently (drawingsWith chosen) g.forest everyCut g.flow)
  holds "flows with problems of every kind, under forests with steps in several groups and a group's id twice: likewise" 1500 $ looseOf <#> \loose ->
    verdict (describeGrouped { flow: loose.flow, forest: loose.forest, closed: loose.closed }) (differently (drawingsWith loose.chosen) loose.forest everyCut loose.flow)
  holds "the same, collapsed and collapsed again" 500 $ looseOf <#> \loose ->
    let
      once = collapse loose.forest loose.closed loose.flow
      onceBefore = Reference.collapse loose.forest loose.closed loose.flow
      again = [ Group (GroupId "again") "Again" (map (Member <<< _.id) (Array.take 2 (unFlow once).steps)) ]
    in
      verdict (describeGrouped { flow: loose.flow, forest: loose.forest, closed: loose.closed })
        ( (if once == onceBefore then [] else [ "collapse differs" ])
            <> differently (drawingsWith loose.chosen) again [ Set.singleton (GroupId "again") ] once
        )
  holds "projections no flow gives (nodes twice, edges to nothing, rings): layers, leafMass, isolated" 1500 $ projectionOf <#> \projection ->
    verdict (show (map _.key projection.nodes) <> " " <> show (projection.edges <#> \edge -> [ edge.from, edge.to ])) (readDifferently "made" projection)
  holds "a made flow twice over, side by side (every id twice): likewise" 200 $ flowOf Shared <#> \flow ->
    let
      f = unFlow flow
      doubled = Flow { items: f.items <> f.items, steps: f.steps <> Array.reverse f.steps, wires: f.wires <> f.wires }
    in
      verdict (describe flow) (differently (drawingsWith Set.empty) [ Group (GroupId "first") "First" (map (Member <<< _.id) f.steps) ] [ Set.singleton (GroupId "first") ] doubled)

  section "identical to 0.1.0: the benchmark's shapes, small"
  for_ shapes \shape -> for_ [ 60, 240 ] \wireCount -> do
    let shaped = shape.make wireCount
    let f = unFlow shaped.flow
    let
      someGroups = Set.fromFoldable (Array.mapWithIndex (\place group -> if place `mod` 2 == 0 then Just group else Nothing) (Array.fromFoldable shaped.groups) # Array.catMaybes)
      drawn =
        [ { name: "Holders", drawing: Holders }
        , { name: "Steps", drawing: Steps }
        , { name: "Both", drawing: Both }
        , { name: "Chosen half the steps", drawing: halfTheSteps shaped.flow }
        ]
    expectNone (shape.name <> ", " <> show (Array.length f.wires) <> " wires, " <> show (Array.length f.steps) <> " steps: problems, four drawings, every group shut, every other group, none")
      (differently drawn shaped.forest [ shaped.groups, someGroups, Set.empty ] shaped.flow)
  expect "and the shapes have no problems" (Array.all (\shape -> problems (shape.make 240).flow == []) shapes)
