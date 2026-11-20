-- | `Collection`: the five operations and the one that is none of them, as
-- | a step's payload.
module Test.Flow.Collection (suite) where

import Prelude

import Data.Array as Array
import Effect (Effect)
import Hylograph.Flow (mapPayloads, project, Drawing(..), unFlow)
import Hylograph.Flow.Collection (CollectionStep, Operation(..), operationName)
import Test.Flow.Check (expect, section)
import Test.Flow.Fixtures (chain)

operations :: Array (Operation String)
operations = [ Map, Fold, Scan, Join, Filter, Unclassified "a pivot" ]

suite :: Effect Unit
suite = do
  section "Collection"
  expect "each operation has its own name, in lower case"
    (map operationName operations == [ "map", "fold", "scan", "join", "filter", "unclassified" ])
  expect "the names are all different" (Array.length (Array.nub (map operationName operations)) == Array.length operations)
  expect "an unclassified step is named the same whatever it is said to be"
    (operationName (Unclassified "a pivot") == operationName (Unclassified 3))
  expect "operations are equal to themselves and to no other; two unclassified ones by what they say"
    ( (operations <#> \a -> Array.length (Array.filter (_ == a) operations)) == [ 1, 1, 1, 1, 1, 1 ]
        && Unclassified "a pivot" /= Unclassified "a sort"
    )
  expect "an operation shows as it is written"
    (map show operations == [ "Map", "Fold", "Scan", "Join", "Filter", "Unclassified \"a pivot\"" ])
  -- a collection step is a step's payload like any other: it rides in the
  -- flow, is never looked at by `project`, and comes back out
  let
    describe :: Unit -> CollectionStep String String
    describe _ = { operation: Scan, detail: "=B1+A2" }
  let classified = mapPayloads identity describe identity chain
  expect "a collection step rides as a step's payload and comes back out"
    (map (operationName <<< _.operation <<< _.payload) (unFlow classified).steps == [ "scan", "scan" ])
  expect "and the projection is the same with it as without"
    (project Holders classified == project Holders chain)
