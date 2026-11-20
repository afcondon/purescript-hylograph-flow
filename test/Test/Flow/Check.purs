-- | The one assertion every suite here uses, as the adapters' tests do: a
-- | line for what held, an exception for what did not.
module Test.Flow.Check
  ( expect
  , expectNone
  , section
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Effect (Effect)
import Effect.Console (log)
import Effect.Exception (throw)

section :: String -> Effect Unit
section = log

expect :: String -> Boolean -> Effect Unit
expect label ok = if ok then log ("  ok: " <> label) else throw ("FAILED: " <> label)

-- | Holds where there is nothing to report; says what there was otherwise.
expectNone :: String -> Array String -> Effect Unit
expectNone label found = do
  for_ found \fault -> log ("    fault: " <> fault)
  expect label (Array.null found)
