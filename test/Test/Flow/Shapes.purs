-- | Flows of a wanted size, made the same way every time: a chain, fan-outs,
-- | and a grouped flow shaped like a workspace's module graph. For the
-- | benchmark (`Test.Flow.Benchmark`) at 1k, 10k and 50k wires, and for the
-- | comparison with the 0.1.0 functions (`Test.Flow.Identical`) at sizes
-- | those can afford.
module Test.Flow.Shapes
  ( Shaped
  , Sized
  , chain
  , fanOuts
  , moduleGraph
  , shapes
  , halfTheSteps
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Hylograph.Flow (Drawing(..), ElementKey(..), Flow(..), Item, ItemId(..), PortName(..), Step, StepId(..), Wire, WireId(..))
import Hylograph.Flow.Grouping (GroupId(..), Grouping(..))

-- | Items carry their place, steps their name, wires their place among the
-- | wires, in a list, whose `Semigroup` shows the order of a merge.
type Sized = Flow Int String (Array Int)

-- | A flow, a grouping of its steps, and the groups there are to shut.
type Shaped = { flow :: Sized, forest :: Array Grouping, groups :: Set GroupId }

upTo :: Int -> Array Int
upTo count = if count <= 0 then [] else Array.range 0 (count - 1)

itemAt :: String -> Int -> Item Int
itemAt prefix place = { id: ItemId (prefix <> show place), name: Just (prefix <> " " <> show place), count: 1, payload: place }

stepAt :: String -> Int -> Array PortName -> Array PortName -> Step String
stepAt prefix place inputs outputs = { id: StepId (prefix <> show place), name: Just (prefix <> " " <> show place), inputs, outputs, payload: prefix <> show place }

numbered :: Array WireId -> Array (Wire (Array Int))
numbered = Array.mapWithIndex \place id -> { id, payload: [ place ] }

-- | Groups of `size` steps, in step order; every one of them to shut.
inGroupsOf :: Int -> Array StepId -> { forest :: Array Grouping, groups :: Set GroupId }
inGroupsOf size steps = { forest, groups: Set.fromFoldable (map _.group runs) }
  where
  count = (Array.length steps + size - 1) / size
  runs = upTo count <#> \place -> { group: GroupId ("group" <> show place), name: "group " <> show place, steps: Array.slice (place * size) ((place + 1) * size) steps }
  forest = runs <#> \run -> Group run.group run.name (map Member run.steps)

-- | item0 -step0-> item1 -step1-> …: two wires a step. Its grouping is the
-- | case `collapse` was first found slow on: every step in one group.
chain :: Int -> Shaped
chain wireCount = { flow: Flow { items, steps, wires }, forest: [ Group whole "whole" (map (Member <<< _.id) steps) ], groups: Set.singleton whole }
  where
  whole = GroupId "whole"
  stepCount = max 1 (wireCount / 2)
  items = map (itemAt "item") (upTo (stepCount + 1))
  steps = upTo stepCount <#> \place -> stepAt "step" place [ PortName "in" ] [ PortName "out" ]
  wires = numbered $ upTo stepCount # Array.concatMap \place ->
    [ Reads (ItemId ("item" <> show place)) (StepId ("step" <> show place)) (PortName "in")
    , Writes (StepId ("step" <> show place)) (PortName "out") (ItemId ("item" <> show (place + 1)))
    ]

-- | Hubs: an item read by fifty steps, each of which writes an item of its
-- | own; the next hub is the item the first of them writes. About two
-- | wires a step. Grouped in tens.
fanOuts :: Int -> Shaped
fanOuts wireCount = { flow: Flow { items, steps, wires }, forest: grouped.forest, groups: grouped.groups }
  where
  fan = 50
  stepCount = max 1 (wireCount / 2)
  -- step `place` reads the hub of its fifty and writes item `place + 1`
  hubOf place = let hub = place / fan in if hub == 0 then 0 else (hub - 1) * fan + 1
  items = map (itemAt "item") (upTo (stepCount + 1))
  steps = upTo stepCount <#> \place -> stepAt "step" place [ PortName "in" ] [ PortName "out" ]
  wires = numbered $ upTo stepCount # Array.concatMap \place ->
    [ Reads (ItemId ("item" <> show (hubOf place))) (StepId ("step" <> show place)) (PortName "in")
    , Writes (StepId ("step" <> show place)) (PortName "out") (ItemId ("item" <> show (place + 1)))
    ]
  grouped = inGroupsOf 10 (map _.id steps)

-- | A workspace's modules: each step writes one item (its compiled output)
-- | and reads the outputs of up to twelve earlier steps, some of them from
-- | its own group of ten (a package) where there are that many, by a port
-- | for each. About 8.7 wires a step: 1,300 steps in 130 groups is 11,400.
moduleGraph :: Int -> Shaped
moduleGraph wireCount = { flow: Flow { items, steps, wires }, forest: grouped.forest, groups: grouped.groups }
  where
  packageSize = 10
  stepCount = max 1 ((wireCount * 10) / 92)
  items = map (itemAt "output") (upTo stepCount)
  -- a fixed scramble, for which earlier steps a step imports
  scramble seed = (seed * 75 + 74) `mod` 65537
  importsOf place =
    let
      first = (place / packageSize) * packageSize
      near = upTo 5 <#> \turn -> first + (scramble (place * 7 + turn) `mod` packageSize)
      far = upTo 7 <#> \turn -> scramble (place * 13 + turn * 31) `mod` (max 1 place)
    in
      Array.nub (Array.filter (_ < place) (near <> far))
  imported = upTo stepCount <#> \place -> { place, imports: importsOf place }
  steps = imported <#> \m -> stepAt "module" m.place (Array.mapWithIndex (\port _ -> PortName ("import" <> show port)) m.imports) [ PortName "output" ]
  wires = numbered $ imported # Array.concatMap \m ->
    Array.mapWithIndex (\port from -> Reads (ItemId ("output" <> show from)) (StepId ("module" <> show m.place)) (PortName ("import" <> show port))) m.imports
      <> [ Writes (StepId ("module" <> show m.place)) (PortName "output") (ItemId ("output" <> show m.place)) ]
  grouped = inGroupsOf packageSize (map _.id steps)

shapes :: Array { name :: String, make :: Int -> Shaped }
shapes =
  [ { name: "chain", make: chain }
  , { name: "fan-outs", make: fanOuts }
  , { name: "modules", make: moduleGraph }
  ]

-- | Every item and every other step: a `Chosen` drawing under which an edge
-- | is one wire or crosses one undrawn step.
halfTheSteps :: forall item step wire. Flow item step wire -> Drawing
halfTheSteps (Flow f) = Chosen case _ of
  ItemKey _ -> true
  StepKey step -> Set.member step drawnSteps
  WireKey _ -> false
  where
  drawnSteps = Set.fromFoldable (Array.mapWithIndex (\place st -> if place `mod` 2 == 0 then Just st.id else Nothing) f.steps # Array.catMaybes)
