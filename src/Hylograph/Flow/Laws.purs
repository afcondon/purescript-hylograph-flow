-- | What must hold of any projection of any flow, as a function that returns
-- | what does not. Here, and not in a test directory, so that every adapter's
-- | tests can hold the projections of its own real flows to it.
module Hylograph.Flow.Laws
  ( projectionFaults
  , paths
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Hylograph.Flow (Drawing, Edge, ElementKey, Flow(..), WireId(..), drawnBy, project, wireEnds)

-- | Every path of wires from a drawn element to a drawn element through
-- | undrawn ones, no undrawn element twice on a path. Found walking forwards
-- | from each wire that leaves a drawn element, where `project` walks
-- | backwards from each wire that enters one.
paths :: forall i s w. Drawing -> Flow i s w -> Array (NonEmptyArray WireId)
paths drawing (Flow f) = Array.concatMap starting wireIds
  where
  drawn = drawnBy drawing
  wireIds = map _.id f.wires
  leaving element = Array.filter (\w -> (wireEnds w).from == element) wireIds

  starting :: WireId -> Array (NonEmptyArray WireId)
  starting w = if drawn (wireEnds w).from then onward Set.empty (NonEmptyArray.singleton w) else []

  onward :: Set ElementKey -> NonEmptyArray WireId -> Array (NonEmptyArray WireId)
  onward seen walked =
    let
      reached = (wireEnds (NonEmptyArray.last walked)).to
    in
      if drawn reached then [ walked ]
      else if Set.member reached seen then []
      else leaving reached # Array.concatMap \next -> onward (Set.insert reached seen) (NonEmptyArray.snoc walked next)

-- | Every fault in the projection of a flow under a drawing, in words; none
-- | is the law.
-- |
-- | - every wire of every edge is a wire of the flow;
-- | - an edge's wires are a path: the first leaves `from`, the last enters
-- |   `to`, each one between meets the next at the element of `via` in that
-- |   place, both ends are drawn and nothing in `via` is; `fromPort` and
-- |   `toPort` are the ports of the first and last wire where those ends
-- |   are steps;
-- | - the edges are exactly the paths of wires from a drawn element to a
-- |   drawn element through undrawn ones (`paths`), each path one edge.
-- |
-- | So a wire between two drawn elements is one edge, alone in it; a wire
-- | with an undrawn end is in one edge for each such path through it, which
-- | may be several (a step with two inputs, contracted, puts its output
-- | wire in two edges) or none (nothing drawn is reached that way). Those
-- | two are restated as checks of their own, though they follow.
projectionFaults :: forall i s w. Drawing -> Flow i s w -> Array String
projectionFaults drawing flow@(Flow f) = unknown <> broken <> ports <> missing <> invented <> repeated <> direct <> counted
  where
  drawn = drawnBy drawing
  projection = project drawing flow
  flowWires = map _.id f.wires
  expected = paths drawing flow

  idsOf :: Edge w -> Array WireId
  idsOf edge = NonEmptyArray.toArray (map _.id edge.wires)
  edgePaths = map idsOf projection.edges
  describe :: Edge w -> String
  describe edge = "the edge " <> show edge.from <> " -> " <> show edge.to <> " via " <> show edge.via

  unknown = projection.edges # Array.concatMap \edge ->
    Array.filter (\w -> not (Array.elem w flowWires)) (idsOf edge) <#> \w -> describe edge <> " holds " <> show w <> ", which is no wire of the flow"

  -- the elements along an edge, as its wires say
  isPath edge =
    let
      ends = map wireEnds (idsOf edge)
    in
      map _.from ends == [ edge.from ] <> edge.via && map _.to ends == edge.via <> [ edge.to ]
  broken = projection.edges # Array.concatMap \edge ->
    (if isPath edge then [] else [ describe edge <> " has wires that are not a path along it" ])
      <> (if drawn edge.from && drawn edge.to then [] else [ describe edge <> " has an end that is not drawn" ])
      <> (if Array.any drawn edge.via then [ describe edge <> " passes through a drawn element" ] else [])

  portOut = case _ of
    Writes _ port _ -> Just port
    Reads _ _ _ -> Nothing
  portIn = case _ of
    Reads _ _ port -> Just port
    Writes _ _ _ -> Nothing
  ports = projection.edges # Array.concatMap \edge ->
    if edge.fromPort == portOut (NonEmptyArray.head edge.wires).id && edge.toPort == portIn (NonEmptyArray.last edge.wires).id then []
    else [ describe edge <> " has a port that is not its first or last wire's" ]

  missing = Array.filter (\path -> not (Array.elem (NonEmptyArray.toArray path) edgePaths)) expected <#> \path ->
    "no edge stands for the path " <> show (NonEmptyArray.toArray path)
  invented = Array.filter (\path -> not (Array.elem path (map NonEmptyArray.toArray expected))) edgePaths <#> \path ->
    "an edge stands for " <> show path <> ", which is no path between drawn elements"
  repeated = Array.filter (\path -> Array.length (Array.filter (_ == path) edgePaths) > 1) (Array.nub edgePaths) <#> \path ->
    "more than one edge stands for the path " <> show path

  holding w = Array.filter (Array.elem w) edgePaths
  bothDrawn w = drawn (wireEnds w).from && drawn (wireEnds w).to
  direct = Array.filter bothDrawn flowWires # Array.concatMap \w ->
    if holding w == [ [ w ] ] then [] else [ show w <> " joins two drawn elements and is not exactly one edge, alone" ]
  counted = flowWires # Array.concatMap \w ->
    let
      through = Array.length (Array.filter (Array.elem w <<< NonEmptyArray.toArray) expected)
    in
      if Array.length (holding w) == through then []
      else [ show w <> " is in " <> show (Array.length (holding w)) <> " edges and on " <> show through <> " paths" ]
