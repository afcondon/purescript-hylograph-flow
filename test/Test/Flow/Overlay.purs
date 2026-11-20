-- | `Overlay`: facts beside a flow, keyed by item, by step and by wire, and
-- | found again from a projection whatever became of the payloads.
module Test.Flow.Overlay (suite) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Hylograph.Flow (Drawing(..), ElementKey(..), Flow, Projection, erase, mapPayloads, project)
import Hylograph.Flow.Overlay (Overlay, keys, lookup, overlay)
import Test.Flow.Check (expect, section)
import Test.Flow.Fixtures (chain, itemKey, reads, stepKey, writes)

data Fact = Stale | Running | Broken

derive instance Eq Fact

-- | The chain with something in every payload.
loaded :: Flow String Int Number
loaded = mapPayloads (const "a file") (const 7) (const 2.5) chain

facts :: Overlay Fact
facts = overlay
  [ Tuple (itemKey "b") Stale
  , Tuple (stepKey "g") Running
  , Tuple (WireKey (writes "f" "out" "b")) Broken
  ]

-- | What an overlay knows of each node, and of each edge that is one wire.
known :: forall wire. Overlay Fact -> Projection wire -> { nodes :: Array (Maybe Fact), edges :: Array (Maybe Fact) }
known state projection =
  { nodes: projection.nodes <#> \node -> lookup node.key state
  , edges: projection.edges <#> \edge -> case NonEmptyArray.toArray edge.wires of
      [ wire ] -> lookup (WireKey wire.id) state
      _ -> Nothing
  }

suite :: Effect Unit
suite = do
  section "Overlay"
  expect "a fact is found by its item, by its step and by its wire"
    ( lookup (itemKey "b") facts == Just Stale
        && lookup (stepKey "g") facts == Just Running
        && lookup (WireKey (writes "f" "out" "b")) facts == Just Broken
    )
  expect "nothing is known of an element nothing was said of, nor of another wire between the same two elements"
    ( lookup (itemKey "a") facts == Nothing
        && lookup (stepKey "f") facts == Nothing
        && lookup (WireKey (reads "b" "g" "in")) facts == Nothing
        && lookup (WireKey (writes "f" "other" "b")) facts == Nothing
    )
  expect "an item and a step of one name are two elements"
    ( lookup (itemKey "same") (overlay [ Tuple (itemKey "same") Stale, Tuple (stepKey "same") Running ]) == Just Stale
        && lookup (stepKey "same") (overlay [ Tuple (itemKey "same") Stale, Tuple (stepKey "same") Running ]) == Just Running
    )
  expect "keys: every element something is known of, items then steps then wires"
    (keys facts == [ itemKey "b", stepKey "g", WireKey (writes "f" "out" "b") ])
  expect "said twice of one element, the later holds"
    (lookup (itemKey "b") (overlay [ Tuple (itemKey "b") Stale, Tuple (itemKey "b") Broken ]) == Just Broken)
  expect "an overlay of nothing knows nothing" (keys (overlay [] :: Overlay Fact) == [] && lookup (itemKey "b") (overlay [] :: Overlay Fact) == Nothing)
  expect "mapping an overlay changes what is known and not of what"
    ( let named = map (\fact -> if fact == Stale then "stale" else "other") facts
      in keys named == keys facts && lookup (itemKey "b") named == Just "stale" && lookup (stepKey "g") named == Just "other"
    )
  expect "overlays are equal when they know the same of the same"
    (overlay [ Tuple (itemKey "b") Stale, Tuple (stepKey "g") Running ] == overlay [ Tuple (stepKey "g") Running, Tuple (itemKey "b") Stale ])

  -- the same overlay, beside the flow with its payloads and beside the
  -- flow erased
  let before = known facts (project Both loaded)
  let after = known facts (project Both (erase loaded))
  expect "under Both, the overlay marks one item, one step and one wire of the loaded flow"
    ( before.nodes == [ Nothing, Just Stale, Nothing, Nothing, Just Running ]
        && before.edges == [ Nothing, Just Broken, Nothing, Nothing ]
    )
  expect "erasing the payloads changes no lookup: the same nodes and the same edges are marked"
    (after == before)
  expect "nor under Steps and Holders, where the nodes are found and no edge is a single wire"
    ( known facts (project Steps (erase loaded)) == known facts (project Steps loaded)
        && known facts (project Holders (erase loaded)) == known facts (project Holders loaded)
        && (known facts (project Steps loaded)).nodes == [ Nothing, Just Running ]
        && Array.all (_ == Nothing) (known facts (project Holders loaded)).edges
    )
