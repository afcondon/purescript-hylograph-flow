-- | The library's own tests: flows made here, registry packages only.
module Test.Flow (main) where

import Prelude

import Effect (Effect)
import Effect.Console (log)
import Test.Flow.Collection as Collection
import Test.Flow.Example as Example
import Test.Flow.Generated as Generated
import Test.Flow.Grouping as Grouping
import Test.Flow.Overlay as Overlay
import Test.Flow.Problems as Problems
import Test.Flow.Projection as Projection

main :: Effect Unit
main = do
  Problems.suite
  Projection.suite
  Overlay.suite
  Grouping.suite
  Collection.suite
  Generated.suite
  Example.suite
  log "flow: all green"
