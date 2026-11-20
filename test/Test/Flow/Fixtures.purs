-- | Small flows made for their shapes, and the words to make them with.
-- | Nothing here comes from any adapter's subject.
module Test.Flow.Fixtures
  ( Bare
  , item
  , step
  , reads
  , writes
  , bare
  , itemKey
  , stepKey
  , Summary
  , summary
  , direct
  , chain
  , fanOut
  , fanIn
  , scan
  , ring
  , lonely
  , unnamed
  , nothing
  , fixtures
  , drawings
  , only
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Hylograph.Flow (Drawing(..), Edge, ElementKey(..), Flow(..), Item, ItemId(..), PortName(..), Step, StepId(..), WireId(..), plain, stepLabel)

-- | A flow with nothing in its payloads.
type Bare = Flow Unit Unit Unit

item :: String -> Item Unit
item name = { id: ItemId name, name: Just name, count: 1, payload: unit }

-- | A step, its input ports and its output ports.
step :: String -> Array String -> Array String -> Step Unit
step name inputs outputs = { id: StepId name, name: Just name, inputs: map PortName inputs, outputs: map PortName outputs, payload: unit }

-- | The item, the step that reads it, the port it is read by.
reads :: String -> String -> String -> WireId
reads itemName stepName port = Reads (ItemId itemName) (StepId stepName) (PortName port)

-- | The step, the port it writes by, the item written.
writes :: String -> String -> String -> WireId
writes stepName port itemName = Writes (StepId stepName) (PortName port) (ItemId itemName)

bare :: Array (Item Unit) -> Array (Step Unit) -> Array WireId -> Bare
bare items steps wires = Flow { items, steps, wires: map plain wires }

itemKey :: String -> ElementKey
itemKey = ItemKey <<< ItemId

stepKey :: String -> ElementKey
stepKey = StepKey <<< StepId

-- | An edge without what its wires carry, so that one can be written down
-- | and compared.
type Summary =
  { from :: ElementKey
  , fromPort :: Maybe PortName
  , to :: ElementKey
  , toPort :: Maybe PortName
  , via :: Array ElementKey
  , wires :: Array WireId
  }

summary :: forall wire. Edge wire -> Summary
summary edge =
  { from: edge.from
  , fromPort: edge.fromPort
  , to: edge.to
  , toPort: edge.toPort
  , via: edge.via
  , wires: NonEmptyArray.toArray (map _.id edge.wires)
  }

-- | The edge that is one wire and nothing else.
direct :: WireId -> Summary
direct wire = case wire of
  Reads i s p -> { from: ItemKey i, fromPort: Nothing, to: StepKey s, toPort: Just p, via: [], wires: [ wire ] }
  Writes s p i -> { from: StepKey s, fromPort: Just p, to: ItemKey i, toPort: Nothing, via: [], wires: [ wire ] }

-- | a -f-> b -g-> c
chain :: Bare
chain = bare
  [ item "a", item "b", item "c" ]
  [ step "f" [ "in" ] [ "out" ], step "g" [ "in" ] [ "out" ] ]
  [ reads "a" "f" "in", writes "f" "out" "b", reads "b" "g" "in", writes "g" "out" "c" ]

-- | `s` writes `x`, which two steps read.
fanOut :: Bare
fanOut = bare
  [ item "x", item "y", item "z" ]
  [ step "s" [] [ "out" ], step "f" [ "in" ] [ "out" ], step "g" [ "in" ] [ "out" ] ]
  [ writes "s" "out" "x", reads "x" "f" "in", writes "f" "out" "y", reads "x" "g" "in", writes "g" "out" "z" ]

-- | `h` reads two items, each written by a step of its own.
fanIn :: Bare
fanIn = bare
  [ item "p", item "q", item "r" ]
  [ step "f" [] [ "out" ], step "g" [] [ "out" ], step "h" [ "left", "right" ] [ "out" ] ]
  [ writes "f" "out" "p", writes "g" "out" "q", reads "p" "h" "left", reads "q" "h" "right", writes "h" "out" "r" ]

-- | A cycle of one step: `add` reads the `total` it writes.
scan :: Bare
scan = bare
  [ item "amount", item "total" ]
  [ step "add" [ "amount", "total" ] [ "total" ] ]
  [ reads "amount" "add" "amount", reads "total" "add" "total", writes "add" "total" "total" ]

-- | A cycle of two steps and two items, and nothing else.
ring :: Bare
ring = bare
  [ item "a", item "b" ]
  [ step "f" [ "in" ] [ "out" ], step "g" [ "in" ] [ "out" ] ]
  [ reads "a" "f" "in", writes "f" "out" "b", reads "b" "g" "in", writes "g" "out" "a" ]

-- | The chain, an item no wire touches and a step no wire touches.
lonely :: Bare
lonely = bare
  [ item "a", item "alone", item "b", item "c" ]
  [ step "f" [ "in" ] [ "out" ], step "idle" [ "in" ] [ "out" ], step "g" [ "in" ] [ "out" ] ]
  [ reads "a" "f" "in", writes "f" "out" "b", reads "b" "g" "in", writes "g" "out" "c" ]

-- | An item and a step the source gave no names.
unnamed :: Bare
unnamed = Flow
  { items: [ { id: ItemId "stream-1", name: Nothing, count: 2, payload: unit } ]
  , steps: [ { id: StepId "block-1", name: Nothing, inputs: [], outputs: [ PortName "out" ], payload: unit } ]
  , wires: [ plain (writes "block-1" "out" "stream-1") ]
  }

nothing :: Bare
nothing = bare [] [] []

fixtures :: Array { name :: String, flow :: Bare }
fixtures =
  [ { name: "chain", flow: chain }
  , { name: "fan-out", flow: fanOut }
  , { name: "fan-in", flow: fanIn }
  , { name: "scan", flow: scan }
  , { name: "ring", flow: ring }
  , { name: "lonely", flow: lonely }
  , { name: "unnamed", flow: unnamed }
  , { name: "nothing", flow: nothing }
  ]

-- | The drawing of exactly these elements.
only :: Array ElementKey -> Drawing
only chosen = Chosen \key -> Array.elem key chosen

-- | The three fixed drawings, and three chosen ones: nothing drawn, every
-- | item and step whose id sorts no later than "f", and every item and step
-- | but those.
drawings :: Array { name :: String, drawing :: Drawing }
drawings =
  [ { name: "Holders", drawing: Holders }
  , { name: "Steps", drawing: Steps }
  , { name: "Both", drawing: Both }
  , { name: "Chosen nothing", drawing: Chosen (const false) }
  , { name: "Chosen a to f", drawing: Chosen early }
  , { name: "Chosen the rest", drawing: Chosen (not <<< early) }
  ]
  where
  early = case _ of
    ItemKey (ItemId name) -> name <= "f"
    StepKey stepId -> stepLabel stepId <= "f"
    WireKey _ -> false
