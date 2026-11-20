-- | Run state beside a flow, never in it: anything known about any element
-- | (a file stale, a link broken, a step running), keyed by the element.
module Hylograph.Flow.Overlay
  ( Overlay(..)
  , overlay
  , lookup
  , keys
  ) where

import Prelude

import Data.Array as Array
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Hylograph.Flow (ElementKey)

newtype Overlay a = Overlay (Map ElementKey a)

derive newtype instance Eq a => Eq (Overlay a)
derive newtype instance Functor Overlay

overlay :: forall a. Array (Tuple ElementKey a) -> Overlay a
overlay = Overlay <<< Map.fromFoldable

lookup :: forall a. ElementKey -> Overlay a -> Maybe a
lookup key (Overlay m) = Map.lookup key m

keys :: forall a. Overlay a -> Array ElementKey
keys (Overlay m) = Map.keys m # Array.fromFoldable
