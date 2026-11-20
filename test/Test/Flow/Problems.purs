-- | `problems`: one flow for each `Problem`, which has that problem and no
-- | other, and well-formed flows, which have none.
module Test.Flow.Problems (suite) where

import Prelude

import Data.Foldable (for_)
import Effect (Effect)
import Hylograph.Flow (ItemId(..), PortName(..), Problem(..), StepId(..), problems)
import Test.Flow.Check (expect, section)
import Test.Flow.Fixtures (Bare, bare, fixtures, item, reads, step, writes)

-- | a -f-> b, and what each case below spoils.
wellFormed :: Bare
wellFormed = bare
  [ item "a", item "b" ]
  [ step "f" [ "in" ] [ "out" ] ]
  [ reads "a" "f" "in", writes "f" "out" "b" ]

cases :: Array { name :: String, flow :: Bare, expected :: Array Problem }
cases =
  [ { name: "a wire reads an item that is not there"
    , flow: bare [ item "a", item "b" ] [ step "f" [ "in" ] [ "out" ] ] [ reads "ghost" "f" "in", writes "f" "out" "b" ]
    , expected: [ UnknownItem (ItemId "ghost") ]
    }
  , { name: "a wire writes an item that is not there"
    , flow: bare [ item "a" ] [ step "f" [ "in" ] [ "out" ] ] [ reads "a" "f" "in", writes "f" "out" "ghost" ]
    , expected: [ UnknownItem (ItemId "ghost") ]
    }
  , { name: "a wire names a step that is not there"
    , flow: bare [ item "a", item "b" ] [ step "f" [ "in" ] [ "out" ] ] [ reads "a" "nobody" "in", writes "f" "out" "b" ]
    , expected: [ UnknownStep (StepId "nobody") ]
    }
  , { name: "a wire reads by a port its step has not"
    , flow: bare [ item "a", item "b" ] [ step "f" [ "in" ] [ "out" ] ] [ reads "a" "f" "other", writes "f" "out" "b" ]
    , expected: [ UnknownPort (StepId "f") (PortName "other") ]
    }
  , { name: "a wire reads by a port that is an output"
    , flow: bare [ item "a", item "b" ] [ step "f" [ "in" ] [ "out" ] ] [ reads "a" "f" "out", writes "f" "out" "b" ]
    , expected: [ UnknownPort (StepId "f") (PortName "out") ]
    }
  , { name: "a wire writes by a port that is an input"
    , flow: bare [ item "a", item "b" ] [ step "f" [ "in" ] [ "out" ] ] [ reads "a" "f" "in", writes "f" "in" "b" ]
    , expected: [ UnknownPort (StepId "f") (PortName "in") ]
    }
  , { name: "two steps write one item"
    , flow: bare [ item "a", item "b" ] [ step "f" [ "in" ] [ "out" ], step "g" [] [ "out" ] ] [ reads "a" "f" "in", writes "f" "out" "b", writes "g" "out" "b" ]
    , expected: [ WrittenTwice (ItemId "b") ]
    }
  , { name: "one step writes one item by two ports"
    , flow: bare [ item "b" ] [ step "f" [] [ "out", "also" ] ] [ writes "f" "out" "b", writes "f" "also" "b" ]
    , expected: [ WrittenTwice (ItemId "b") ]
    }
  , { name: "two items share an id"
    , flow: bare [ item "a", item "b", item "a" ] [ step "f" [ "in" ] [ "out" ] ] [ reads "a" "f" "in", writes "f" "out" "b" ]
    , expected: [ DuplicateItem (ItemId "a") ]
    }
  , { name: "two steps share an id"
    , flow: bare [ item "a", item "b" ] [ step "f" [ "in" ] [ "out" ], step "f" [ "in" ] [ "out" ] ] [ reads "a" "f" "in", writes "f" "out" "b" ]
    , expected: [ DuplicateStep (StepId "f") ]
    }
  , { name: "two wires share an identity"
    , flow: bare [ item "a", item "b" ] [ step "f" [ "in" ] [ "out" ] ] [ reads "a" "f" "in", reads "a" "f" "in", writes "f" "out" "b" ]
    , expected: [ DuplicateWire (reads "a" "f" "in") ]
    }
  ]

suite :: Effect Unit
suite = do
  section "problems"
  expect "a -f-> b has none" (problems wellFormed == [])
  for_ fixtures \fixture ->
    expect ("the fixture " <> fixture.name <> " has none") (problems fixture.flow == [])
  for_ cases \c ->
    expect (c.name <> ": exactly " <> show c.expected <> " (found " <> show (problems c.flow) <> ")") (problems c.flow == c.expected)
  -- every problem is reported once however often it occurs, and all at once
  let
    several = bare [ item "a", item "a", item "a" ] [ step "f" [ "in" ] [] ]
      [ reads "ghost" "nobody" "in", reads "a" "f" "in", reads "a" "f" "in", reads "a" "f" "in" ]
  expect "problems of several kinds are all reported, a repeated id once"
    ( problems several ==
        [ DuplicateItem (ItemId "a")
        , DuplicateWire (reads "a" "f" "in")
        , UnknownItem (ItemId "ghost")
        , UnknownStep (StepId "nobody")
        ]
    )
  -- a write said twice is a wire said twice and, as things stand, also an
  -- item written twice: the one fixture for `DuplicateWire` is a read
  expect "the same write said twice is reported as a duplicate wire and as an item written twice"
    ( problems (bare [ item "b" ] [ step "f" [] [ "out" ] ] [ writes "f" "out" "b", writes "f" "out" "b" ])
        == [ DuplicateWire (writes "f" "out" "b"), WrittenTwice (ItemId "b") ]
    )
