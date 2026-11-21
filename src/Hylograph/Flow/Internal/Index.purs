-- | INTERNAL: no part of the library's interface, and free to change in any
-- | release. The three ways the library's own functions look a flow's
-- | arrays up without scanning them again for every element: values gathered
-- | under a key, how often each value occurs, and which occur more than once.
module Hylograph.Flow.Internal.Index
  ( gathered
  , under
  , tally
  , repeated
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (foldl)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))

-- | Every value under its key, a key's values in the order they came.
-- | One sort by key and then by place, so that nothing rests on the sort's
-- | being stable and no array is appended to once for each value.
gathered :: forall key value. Ord key => Array (Tuple key value) -> Map key (Array value)
gathered pairs = Map.fromFoldable (map together runs)
  where
  placed = Array.mapWithIndex (\place (Tuple key value) -> { place, key, value }) pairs
  sorted = Array.sortBy (\a b -> compare a.key b.key <> compare a.place b.place) placed
  runs = Array.groupBy (\a b -> a.key == b.key) sorted
  together run = Tuple (NonEmptyArray.head run).key (NonEmptyArray.toArray (map _.value run))

-- | What was gathered under a key; nothing, where the key was never met.
under :: forall key value. Ord key => key -> Map key (Array value) -> Array value
under key = fromMaybe [] <<< Map.lookup key

-- | How many times each value occurs.
tally :: forall a. Ord a => Array a -> Map a Int
tally = foldl (\counted x -> Map.insertWith (+) x 1 counted) Map.empty

-- | The values that occur more than once, each once, in the order of their
-- | first occurrence.
repeated :: forall a. Ord a => Array a -> Array a
repeated xs = Array.filter (\x -> fromMaybe 0 (Map.lookup x counted) > 1) (Array.nub xs)
  where
  counted = tally xs
