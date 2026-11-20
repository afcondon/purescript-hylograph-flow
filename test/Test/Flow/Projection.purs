-- | `project`, drawing by drawing, on the fixtures: what is drawn stated
-- | outright, and the projection law on every fixture under every drawing.
module Test.Flow.Projection (suite) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Hylograph.Flow (Drawing(..), ElementKey(..), Flow(..), ItemId(..), PortName(..), StepId(..), WireId, drawnBy, erase, isolated, layers, leafMass, mapPayloads, plain, project, readersOf, unFlow, wireEnds, writerOf)
import Hylograph.Flow.Laws (paths, projectionFaults)
import Test.Flow.Check (expect, expectNone, section)
import Test.Flow.Fixtures (Bare, Summary, chain, direct, fanIn, fanOut, fixtures, drawings, itemKey, lonely, only, reads, ring, scan, stepKey, summary, unnamed, writes)

keysOf :: Drawing -> Bare -> Array ElementKey
keysOf drawing flow = map _.key (project drawing flow).nodes

edgesOf :: Drawing -> Bare -> Array Summary
edgesOf drawing flow = map summary (project drawing flow).edges

-- | An edge across undrawn elements.
across :: ElementKey -> Maybe String -> Array ElementKey -> ElementKey -> Maybe String -> Array WireId -> Summary
across from fromPort via to toPort wires = { from, fromPort: map PortName fromPort, to, toPort: map PortName toPort, via, wires }

wireIds :: Bare -> Array WireId
wireIds flow = map _.id (unFlow flow).wires

suite :: Effect Unit
suite = do
  section "project: Holders (items are nodes, a hidden step is contracted)"
  expect "chain: the items, in order" (keysOf Holders chain == [ itemKey "a", itemKey "b", itemKey "c" ])
  expect "chain: a -> b through f and b -> c through g, each its read and its write"
    ( edgesOf Holders chain ==
        [ across (itemKey "a") Nothing [ stepKey "f" ] (itemKey "b") Nothing [ reads "a" "f" "in", writes "f" "out" "b" ]
        , across (itemKey "b") Nothing [ stepKey "g" ] (itemKey "c") Nothing [ reads "b" "g" "in", writes "g" "out" "c" ]
        ]
    )
  expect "fan-out: x -> y and x -> z; the write into x by a step that reads nothing is in no edge"
    ( edgesOf Holders fanOut ==
        [ across (itemKey "x") Nothing [ stepKey "f" ] (itemKey "y") Nothing [ reads "x" "f" "in", writes "f" "out" "y" ]
        , across (itemKey "x") Nothing [ stepKey "g" ] (itemKey "z") Nothing [ reads "x" "g" "in", writes "g" "out" "z" ]
        ]
    )
  expect "fan-in: p -> r and q -> r through the one step, whose write is in both edges"
    ( edgesOf Holders fanIn ==
        [ across (itemKey "p") Nothing [ stepKey "h" ] (itemKey "r") Nothing [ reads "p" "h" "left", writes "h" "out" "r" ]
        , across (itemKey "q") Nothing [ stepKey "h" ] (itemKey "r") Nothing [ reads "q" "h" "right", writes "h" "out" "r" ]
        ]
    )
  expect "scan: amount -> total, and total -> total, an edge from a node to itself"
    ( edgesOf Holders scan ==
        [ across (itemKey "amount") Nothing [ stepKey "add" ] (itemKey "total") Nothing [ reads "amount" "add" "amount", writes "add" "total" "total" ]
        , across (itemKey "total") Nothing [ stepKey "add" ] (itemKey "total") Nothing [ reads "total" "add" "total", writes "add" "total" "total" ]
        ]
    )
  expect "ring: a -> b and b -> a"
    ( edgesOf Holders ring ==
        [ across (itemKey "a") Nothing [ stepKey "f" ] (itemKey "b") Nothing [ reads "a" "f" "in", writes "f" "out" "b" ]
        , across (itemKey "b") Nothing [ stepKey "g" ] (itemKey "a") Nothing [ reads "b" "g" "in", writes "g" "out" "a" ]
        ]
    )
  expect "lonely: the item no wire touches is a node, and the only isolated one; the idle step is not drawn"
    ( keysOf Holders lonely == [ itemKey "a", itemKey "alone", itemKey "b", itemKey "c" ]
        && isolated (project Holders lonely) == [ itemKey "alone" ]
    )

  section "project: Steps (steps are nodes, a hidden item is contracted)"
  expect "chain: the steps, in order" (keysOf Steps chain == [ stepKey "f", stepKey "g" ])
  expect "chain: f -> g through b, leaving by f's port and entering by g's"
    (edgesOf Steps chain == [ across (stepKey "f") (Just "out") [ itemKey "b" ] (stepKey "g") (Just "in") [ writes "f" "out" "b", reads "b" "g" "in" ] ])
  expect "fan-out: s -> f and s -> g through the one item, whose write is in both edges"
    ( edgesOf Steps fanOut ==
        [ across (stepKey "s") (Just "out") [ itemKey "x" ] (stepKey "f") (Just "in") [ writes "s" "out" "x", reads "x" "f" "in" ]
        , across (stepKey "s") (Just "out") [ itemKey "x" ] (stepKey "g") (Just "in") [ writes "s" "out" "x", reads "x" "g" "in" ]
        ]
    )
  expect "fan-in: f -> h and g -> h, each by its own port of h"
    ( edgesOf Steps fanIn ==
        [ across (stepKey "f") (Just "out") [ itemKey "p" ] (stepKey "h") (Just "left") [ writes "f" "out" "p", reads "p" "h" "left" ]
        , across (stepKey "g") (Just "out") [ itemKey "q" ] (stepKey "h") (Just "right") [ writes "g" "out" "q", reads "q" "h" "right" ]
        ]
    )
  expect "scan: add -> add through total; the read of an item nothing writes is in no edge"
    (edgesOf Steps scan == [ across (stepKey "add") (Just "total") [ itemKey "total" ] (stepKey "add") (Just "total") [ writes "add" "total" "total", reads "total" "add" "total" ] ])
  expect "ring: f -> g and g -> f"
    ( edgesOf Steps ring ==
        [ across (stepKey "g") (Just "out") [ itemKey "a" ] (stepKey "f") (Just "in") [ writes "g" "out" "a", reads "a" "f" "in" ]
        , across (stepKey "f") (Just "out") [ itemKey "b" ] (stepKey "g") (Just "in") [ writes "f" "out" "b", reads "b" "g" "in" ]
        ]
    )
  expect "lonely: the step no wire touches is a node, and the only isolated one"
    ( keysOf Steps lonely == [ stepKey "f", stepKey "idle", stepKey "g" ]
        && isolated (project Steps lonely) == [ stepKey "idle" ]
    )

  section "project: Both (every wire an edge)"
  for_ fixtures \fixture ->
    expect (fixture.name <> ": the nodes are the items then the steps, the edges are the wires, one each, in order")
      ( keysOf Both fixture.flow == map (ItemKey <<< _.id) (unFlow fixture.flow).items <> map (StepKey <<< _.id) (unFlow fixture.flow).steps
          && edgesOf Both fixture.flow == map direct (wireIds fixture.flow)
      )
  expect "lonely: both the lone item and the idle step are isolated, in node order"
    (isolated (project Both lonely) == [ itemKey "alone", stepKey "idle" ])
  expect "a step node has its ports and its name; an item node has no ports"
    ( (project Both chain).nodes ==
        [ { key: itemKey "a", label: "a", inputs: [], outputs: [] }
        , { key: itemKey "b", label: "b", inputs: [], outputs: [] }
        , { key: itemKey "c", label: "c", inputs: [], outputs: [] }
        , { key: stepKey "f", label: "f", inputs: [ PortName "in" ], outputs: [ PortName "out" ] }
        , { key: stepKey "g", label: "g", inputs: [ PortName "in" ], outputs: [ PortName "out" ] }
        ]
    )
  expect "an element with no name is labelled by its id"
    (map _.label (project Both unnamed).nodes == [ "stream-1", "block-1" ])

  section "project: Chosen (the caller says, element by element)"
  let endsOfChain = only [ itemKey "a", stepKey "f", itemKey "c" ]
  expect "chain, drawing a, f and c: items before steps"
    (keysOf endsOfChain chain == [ itemKey "a", itemKey "c", stepKey "f" ])
  expect "chain, drawing a, f and c: a -> f is its wire; f -> c crosses a hidden item and a hidden step, three wires"
    ( edgesOf endsOfChain chain ==
        [ direct (reads "a" "f" "in")
        , across (stepKey "f") (Just "out") [ itemKey "b", stepKey "g" ] (itemKey "c") Nothing [ writes "f" "out" "b", reads "b" "g" "in", writes "g" "out" "c" ]
        ]
    )
  expect "ring, drawing f alone: one edge from f to itself, round the whole ring"
    ( edgesOf (only [ stepKey "f" ]) ring ==
        [ across (stepKey "f") (Just "out") [ itemKey "b", stepKey "g", itemKey "a" ] (stepKey "f") (Just "in")
            [ writes "f" "out" "b", reads "b" "g" "in", writes "g" "out" "a", reads "a" "f" "in" ]
        ]
    )
  expect "ring, drawing nothing: no node and no edge (the walk round a cycle of hidden elements ends)"
    ((project (Chosen (const false)) ring).nodes == [] && edgesOf (Chosen (const false)) ring == [])
  expect "scan, drawing amount and total and add: the same as Both"
    (edgesOf (only [ itemKey "amount", itemKey "total", stepKey "add" ]) scan == edgesOf Both scan)
  expect "a chosen drawing that says yes to wires draws no more than one that does not"
    (edgesOf (Chosen (const true)) chain == edgesOf Both chain && keysOf (Chosen (const true)) chain == keysOf Both chain)
  expect "drawnBy: Holders draws items only, Steps steps only, Both neither wire"
    ( map (drawnBy Holders) [ itemKey "a", stepKey "f", wireKeyOfChain ] == [ true, false, false ]
        && map (drawnBy Steps) [ itemKey "a", stepKey "f", wireKeyOfChain ] == [ false, true, false ]
        && map (drawnBy Both) [ itemKey "a", stepKey "f", wireKeyOfChain ] == [ true, true, false ]
    )

  section "project: the projection law, on every fixture under every drawing"
  for_ fixtures \fixture -> for_ drawings \d -> do
    let projection = project d.drawing fixture.flow
    expectNone (fixture.name <> ", " <> d.name <> ": " <> show (Array.length projection.nodes) <> " nodes, " <> show (Array.length projection.edges) <> " edges, lawful")
      (projectionFaults d.drawing fixture.flow)
  expect "the law's own paths, walked forwards: chain under Holders has two, each a read then a write"
    ( map Array.fromFoldable (paths Holders chain) ==
        [ [ reads "a" "f" "in", writes "f" "out" "b" ], [ reads "b" "g" "in", writes "g" "out" "c" ] ]
    )

  section "the flow's other questions"
  expect "wireEnds: a read runs from its item to its step, a write from its step to its item"
    ( wireEnds (reads "a" "f" "in") == { from: itemKey "a", to: stepKey "f" }
        && wireEnds (writes "f" "out" "b") == { from: stepKey "f", to: itemKey "b" }
    )
  let labelled = mapPayloads identity identity (const "carried") chain
  expect "writerOf and readersOf: who writes b and who reads it, with what the wire carries"
    ( writerOf labelled (ItemId "b") == [ { step: StepId "f", port: PortName "out", payload: "carried" } ]
        && readersOf labelled (ItemId "b") == [ { step: StepId "g", port: PortName "in", payload: "carried" } ]
        && writerOf labelled (ItemId "a") == []
        && readersOf labelled (ItemId "c") == []
    )
  expect "mapPayloads changes payloads and nothing else; erase is mapPayloads to unit"
    ( erase labelled == chain
        && map _.id (unFlow labelled).wires == wireIds chain
        && mapPayloads identity identity identity chain == chain
        && map _.payload (unFlow (mapPayloads (const 1) (const 2) (const 3) chain)).wires == [ 3, 3, 3, 3 ]
    )
  expect "plain: a wire that carries nothing" (plain (reads "a" "f" "in") == { id: reads "a" "f" "in", payload: unit })
  expect "layers: the chain under Both is five layers deep, each element one further on"
    ( layers (project Both chain) == Map.fromFoldable
        [ Tuple (itemKey "a") 0, Tuple (stepKey "f") 1, Tuple (itemKey "b") 2, Tuple (stepKey "g") 3, Tuple (itemKey "c") 4 ]
    )
  expect "layers: an edge from a node to itself is ignored (scan under Holders: amount 0, total 1)"
    (layers (project Holders scan) == Map.fromFoldable [ Tuple (itemKey "amount") 0, Tuple (itemKey "total") 1 ])
  expect "layers: a node no edge touches is in layer 0"
    (Map.lookup (itemKey "alone") (layers (project Holders lonely)) == Just 0)
  expect "leafMass: fan-in under Holders, r is reached from p and from q, each of which is its own"
    ( leafMass (project Holders fanIn) == Map.fromFoldable
        [ Tuple (itemKey "p") (Set.singleton (itemKey "p"))
        , Tuple (itemKey "q") (Set.singleton (itemKey "q"))
        , Tuple (itemKey "r") (Set.fromFoldable [ itemKey "p", itemKey "q" ])
        ]
    )
  expect "the flow is its three arrays (unFlow of Flow)"
    (Flow (unFlow chain) == chain)
  where
  wireKeyOfChain = WireKey (reads "a" "f" "in")
