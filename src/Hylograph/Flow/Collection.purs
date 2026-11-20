-- | One layer up from the flow: a step payload for flows over collections.
-- | A step is one of the five familiar operations, or explicitly none of
-- | them, carrying the domain's own description of what it is instead.
module Hylograph.Flow.Collection
  ( Operation(..)
  , CollectionStep
  , operationName
  ) where

import Prelude

data Operation description
  = Map
  | Fold
  | Scan
  | Join
  | Filter
  | Unclassified description

derive instance Eq description => Eq (Operation description)

instance Show description => Show (Operation description) where
  show = case _ of
    Map -> "Map"
    Fold -> "Fold"
    Scan -> "Scan"
    Join -> "Join"
    Filter -> "Filter"
    Unclassified d -> "Unclassified " <> show d

-- | `detail`: the domain's own data for the step (a formula, a recipe).
type CollectionStep description detail = { operation :: Operation description, detail :: detail }

operationName :: forall description. Operation description -> String
operationName = case _ of
  Map -> "map"
  Fold -> "fold"
  Scan -> "scan"
  Join -> "join"
  Filter -> "filter"
  Unclassified _ -> "unclassified"
