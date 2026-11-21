-- | How long the library's functions take on flows of a given size, and how
-- | long the 0.1.0 ones took (`Test.Flow.Reference`). No part of `spago
-- | test`: one measurement to a process, run by `bench.mjs` at the root,
-- | which gives each a time limit:
-- |
-- |     node bench.mjs                 (after `spago build`)
-- |
-- | or one at a time, the words being: which implementation (`library` or
-- | `reference`), which function, which shape, how many wires:
-- |
-- |     node -e "import('./output/Test.Flow.Benchmark/index.js').then((m) => m.main())" library collapse modules 12000
-- |
-- | The flow is made before the clock starts.
module Test.Flow.Benchmark (main) where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Console (log)
import Hylograph.Flow (Drawing(..), unFlow)
import Hylograph.Flow as Library
import Hylograph.Flow.Grouping as LibraryGrouping
import Hylograph.Flow.Laws as LibraryLaws
import Test.Flow.Reference as Reference
import Test.Flow.Reference011 as Reference011
import Test.Flow.Shapes (Shaped, halfTheSteps, shapes)

foreign import milliseconds :: Effect Number
foreign import commandLine :: Effect (Array String)

data Implementation = OfLibrary | OfReference

-- | What is timed, each giving a number to say what it made (so that the two
-- | implementations can be seen to have made as much).
measures :: Array { name :: String, run :: Implementation -> Shaped -> Unit -> Int }
measures =
  [ { name: "problems"
    , run: \implementation shaped _ -> Array.length case implementation of
        OfLibrary -> Library.problems shaped.flow
        OfReference -> Reference.problems shaped.flow
    }
  , projecting "project-Holders" (const Holders)
  , projecting "project-Steps" (const Steps)
  , projecting "project-Both" (const Both)
  , projecting "project-Chosen" halfTheSteps
  , { name: "layers-Steps"
    , run: \implementation shaped ->
        let
          projection = Library.project Steps shaped.flow
        in
          \_ -> Map.size case implementation of
            OfLibrary -> Library.layers projection
            OfReference -> Reference.layers projection
    }
  , faults "faults-Holders" Holders
  , faults "faults-Steps" Steps
  , faults "faults-Both" Both
  , { name: "collapse"
    , run: \implementation shaped _ -> case implementation of
        OfLibrary -> wireCount (LibraryGrouping.collapse shaped.forest shaped.groups shaped.flow)
        OfReference -> wireCount (Reference.collapse shaped.forest shaped.groups shaped.flow)
    }
  -- 0.1.1's collapse (`Test.Flow.Reference011`), whichever implementation is asked for
  , { name: "collapse-0.1.1"
    , run: \_ shaped _ -> Array.length (unFlow (Reference011.collapse shaped.forest shaped.groups shaped.flow)).wires
    }
  -- the library's collapse with every item held by a group, whichever is asked for
  , { name: "collapse-holding"
    , run: \_ shaped _ -> Array.length (unFlow (LibraryGrouping.collapse shaped.holding shaped.groups shaped.flow)).wires
    }
  ]
  where
  wireCount :: forall item step wire. Library.Flow item step wire -> Int
  wireCount flow = Array.length (unFlow flow).wires
  projecting name drawingOf =
    { name
    , run: \implementation shaped ->
        let
          drawing = drawingOf shaped.flow
        in
          \_ -> Array.length $ _.edges case implementation of
            OfLibrary -> Library.project drawing shaped.flow
            OfReference -> Reference.project drawing shaped.flow
    }
  faults name drawing =
    { name
    , run: \implementation shaped _ -> Array.length case implementation of
        OfLibrary -> LibraryLaws.projectionFaults drawing shaped.flow
        OfReference -> Reference.projectionFaults drawing shaped.flow
    }

main :: Effect Unit
main = do
  words <- commandLine
  case words of
    [ implementationName, measureName, shapeName, wireCount ]
      | Just implementation <- implementationOf implementationName
      , Just measure <- Array.find (\m -> m.name == measureName) measures
      , Just shape <- Array.find (\s -> s.name == shapeName) shapes
      , Just wanted <- Int.fromString wireCount -> do
          let shaped = shape.make wanted
          let f = unFlow shaped.flow
          let ready = measure.run implementation shaped
          before <- milliseconds
          let made = ready unit
          after <- milliseconds
          log
            ( "{\"implementation\":" <> show implementationName <> ",\"measure\":" <> show measureName <> ",\"shape\":" <> show shapeName
                <> ",\"wires\":" <> show (Array.length f.wires)
                <> ",\"steps\":" <> show (Array.length f.steps)
                <> ",\"items\":" <> show (Array.length f.items)
                <> ",\"made\":" <> show made
                <> ",\"milliseconds\":" <> show (after - before)
                <> "}"
            )
    _ -> log ("usage: library|reference " <> show (map _.name measures) <> " " <> show (map _.name shapes) <> " <wires>")
  where
  implementationOf = case _ of
    "library" -> Just OfLibrary
    "reference" -> Just OfReference
    _ -> Nothing
