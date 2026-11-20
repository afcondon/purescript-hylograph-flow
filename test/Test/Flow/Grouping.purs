-- | `Grouping` and `collapse`: nested groups shut one at a time, what
-- | collapsing nothing is, and the merging of wires left parallel.
module Test.Flow.Grouping (suite) where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Hylograph.Flow (Drawing(..), Flow(..), ItemId(..), PortName(..), StepId(..), WireId(..), mapPayloads, portLabel, problems, project, stepLabel, unFlow)
import Hylograph.Flow.Grouping (Collapsed(..), GroupId(..), Grouping(..), collapse, groupStep, members)
import Hylograph.Flow.Laws (projectionFaults)
import Test.Flow.Check (expect, expectNone, section)
import Test.Flow.Fixtures (Bare, bare, fixtures, item, reads, step, writes)

-- | A flow whose steps carry their names and whose wires say what they are,
-- | so that a merge can be read off.
type Told = Flow Unit String (Array String)

told :: Bare -> Told
told (Flow f) = Flow
  { items: f.items
  , steps: f.steps <#> \st -> st { payload = nameOf st.id }
  , wires: f.wires <#> \w -> w { payload = [ show w.id ] }
  }
  where
  nameOf = stepLabel

-- | a -f-> b -g-> c -h-> d -k-> e, and `g` reads `a` as well.
pipeline :: Array WireId -> Told
pipeline = told <<< bare
  [ item "a", item "b", item "c", item "d", item "e" ]
  [ step "f" [ "in" ] [ "out" ], step "g" [ "in", "extra" ] [ "out" ], step "h" [ "in" ] [ "out" ], step "k" [ "in", "spare" ] [ "out" ] ]

inOrder :: Array WireId
inOrder =
  [ reads "a" "f" "in"
  , writes "f" "out" "b"
  , reads "b" "g" "in"
  , reads "a" "g" "extra"
  , writes "g" "out" "c"
  , reads "c" "h" "in"
  , writes "h" "out" "d"
  , reads "d" "k" "in"
  , writes "k" "out" "e"
  ]

-- | The same wires with `g`'s read of `a` before `f`'s.
gFirst :: Array WireId
gFirst = [ reads "a" "g" "extra" ] <> Array.filter (_ /= reads "a" "g" "extra") inOrder

inner :: GroupId
inner = GroupId "inner"

outer :: GroupId
outer = GroupId "outer"

-- | outer [ inner [ f, g ], h ], and k in no group.
forest :: Array Grouping
forest =
  [ Group outer "Outer" [ Group inner "Inner" [ Member (StepId "f"), Member (StepId "g") ], Member (StepId "h") ]
  , Member (StepId "k")
  ]

-- | What collapsing nothing is: the same flow, each step's payload wrapped
-- | in `Kept`.
kept :: forall item step wire. Flow item step wire -> Flow item (Collapsed step) wire
kept = mapPayloads identity Kept identity

wire :: forall wire. WireId -> wire -> { id :: WireId, payload :: wire }
wire id payload = { id, payload }

-- | A port of a closed group: the member's, seen from outside.
member :: String -> String -> PortName
member stepName port = MemberPort (StepId stepName) (PortName port)

-- | The item, the closed group that reads it, the member and its port.
groupReads :: String -> GroupId -> String -> String -> WireId
groupReads itemName group stepName port = Reads (ItemId itemName) (GroupStep group) (member stepName port)

-- | The closed group, the member and its port, the item written.
groupWrites :: GroupId -> String -> String -> String -> WireId
groupWrites group stepName port itemName = Writes (GroupStep group) (member stepName port) (ItemId itemName)

-- | A wire the collapse left as it was.
same :: WireId -> { id :: WireId, payload :: Array String }
same id = wire id [ show id ]

suite :: Effect Unit
suite = do
  section "Grouping"
  expect "members: every step under a grouping, nested or not, in order"
    ( Array.concatMap members forest == [ StepId "f", StepId "g", StepId "h", StepId "k" ]
        && members (Member (StepId "f")) == [ StepId "f" ]
        && members (Group inner "Inner" []) == []
    )
  expect "groupStep: the step a closed group becomes is the group's own, and no step a source can name"
    ( groupStep inner == GroupStep inner
        && groupStep inner /= StepId "group:inner"
        && stepLabel (groupStep inner) == "group:inner"
        && stepLabel (StepId "group:inner") == "group:inner"
    )
  expect "a member's port seen from outside is the member and its port, and no port a source can name"
    ( member "f" "out" /= PortName "f.out"
        && portLabel (member "f" "out") == "f.out"
        && portLabel (PortName "f.out") == "f.out"
        && portLabel (MemberPort (GroupStep inner) (member "f" "out")) == "group:inner.f.out"
    )
  expect "ids of the two kinds compare and show apart"
    ( compare (StepId "group:inner") (GroupStep inner) == LT
        && compare (PortName "f.out") (member "f" "out") == LT
        && show (GroupStep inner) == "GroupStep (GroupId \"inner\")"
        && show (member "f" "out") == "MemberPort (StepId \"f\") (PortName \"out\")"
    )

  section "collapse: nothing closed"
  -- "Identity" cannot be equality with the flow itself, since the result's
  -- steps carry `Collapsed step`. It is: `collapse forest Set.empty flow`
  -- equals `mapPayloads identity Kept identity flow`, the same items, steps
  -- and wires in the same order with every step's payload in `Kept`, for
  -- any forest, provided no two wires and no two steps of the flow share
  -- an identity (two that do are merged, which is `collapse`'s other job).
  expect "with nothing closed, the pipeline comes back with each step's payload in Kept"
    (collapse forest Set.empty (pipeline inOrder) == kept (pipeline inOrder))
  expect "likewise with no groups at all, and whatever is said to be closed"
    (collapse [] (Set.fromFoldable [ inner, outer ]) (pipeline inOrder) == kept (pipeline inOrder))
  expect "likewise when what is closed is no group of the forest"
    (collapse forest (Set.singleton (GroupId "elsewhere")) (pipeline inOrder) == kept (pipeline inOrder))
  for_ fixtures \fixture ->
    expect ("likewise the fixture " <> fixture.name <> ", every step of it in one open group")
      ( let whole = [ Group (GroupId "all") "All" (map (Member <<< _.id) (unFlow fixture.flow).steps) ]
        in collapse whole Set.empty fixture.flow == kept fixture.flow
      )
  expect "two wires of one identity are merged even so, their payloads combined in order: this is where identity stops"
    ( let doubled = Flow { items: [ item "a" ], steps: [ step "f" [ "in" ] [] ], wires: [ wire (reads "a" "f" "in") [ "first" ], wire (reads "a" "f" "in") [ "second" ] ] }
      in (unFlow (collapse [] Set.empty doubled)).wires == [ wire (reads "a" "f" "in") [ "first", "second" ] ]
    )

  section "collapse: the inner group shut"
  let innerShut = unFlow (collapse forest (Set.singleton inner) (pipeline inOrder))
  expect "b, which only the group writes and reads, is hidden; the other items stay"
    (map _.id innerShut.items == [ ItemId "a", ItemId "c", ItemId "d", ItemId "e" ])
  expect "f and g are one step where f was, named for the group, with their payloads in step order; h and k are kept"
    ( map _.id innerShut.steps == [ GroupStep inner, StepId "h", StepId "k" ]
        && map _.name innerShut.steps == [ Just "Inner", Just "h", Just "k" ]
        && map _.payload innerShut.steps == [ Closed inner [ "f", "g" ], Kept "h", Kept "k" ]
    )
  expect "the group's ports are its members' ports that still meet a wire or never did, each the member and its port"
    ( map _.inputs innerShut.steps == [ [ member "f" "in" ], [ PortName "in" ], [ PortName "in", PortName "spare" ] ]
        && map _.outputs innerShut.steps == [ [ member "g" "out" ], [ PortName "out" ], [ PortName "out" ] ]
    )
  expect "f's and g's reads of a are one wire, at the port of the first, carrying both in wire order"
    ( innerShut.wires ==
        [ wire (groupReads "a" inner "f" "in") [ show (reads "a" "f" "in"), show (reads "a" "g" "extra") ]
        , wire (groupWrites inner "g" "out" "c") [ show (writes "g" "out" "c") ]
        , same (reads "c" "h" "in")
        , same (writes "h" "out" "d")
        , same (reads "d" "k" "in")
        , same (writes "k" "out" "e")
        ]
    )
  let gFirstShut = unFlow (collapse forest (Set.singleton inner) (pipeline gFirst))
  expect "with g's read first among the wires, the merged wire is at g's port and carries g's first"
    ( Array.head gFirstShut.wires == Just (wire (groupReads "a" inner "g" "extra") [ show (reads "a" "g" "extra"), show (reads "a" "f" "in") ])
        && map _.inputs (Array.take 1 gFirstShut.steps) == [ [ member "g" "extra" ] ]
        && Array.length gFirstShut.wires == 6
    )

  section "collapse: the outer group shut"
  let outerShut = unFlow (collapse forest (Set.singleton outer) (pipeline inOrder))
  expect "a closed group shuts everything under it: f, g and h are one step, b and c are hidden"
    ( map _.id outerShut.items == [ ItemId "a", ItemId "d", ItemId "e" ]
        && map _.id outerShut.steps == [ GroupStep outer, StepId "k" ]
        && map _.payload outerShut.steps == [ Closed outer [ "f", "g", "h" ], Kept "k" ]
        && map _.inputs outerShut.steps == [ [ member "f" "in" ], [ PortName "in", PortName "spare" ] ]
        && map _.outputs outerShut.steps == [ [ member "h" "out" ], [ PortName "out" ] ]
    )
  expect "its wires: one merged read of a, the write of d, and k's two untouched"
    ( outerShut.wires ==
        [ wire (groupReads "a" outer "f" "in") [ show (reads "a" "f" "in"), show (reads "a" "g" "extra") ]
        , wire (groupWrites outer "h" "out" "d") [ show (writes "h" "out" "d") ]
        , same (reads "d" "k" "in")
        , same (writes "k" "out" "e")
        ]
    )
  expect "shutting the inner group as well changes nothing: the outermost closed group is the one drawn"
    (collapse forest (Set.fromFoldable [ outer, inner ]) (pipeline inOrder) == collapse forest (Set.singleton outer) (pipeline inOrder))

  section "collapse: a group that is a whole cycle, and one that is a single step"
  let ringShut = unFlow (collapse [ Group (GroupId "ring") "Ring" [ Member (StepId "f"), Member (StepId "g") ] ] (Set.singleton (GroupId "ring")) ringFlow)
  expect "a ring shut whole is one step with no port, no item and no wire"
    ( ringShut.items == [] && ringShut.wires == []
        && map _.id ringShut.steps == [ GroupStep (GroupId "ring") ]
        && map _.inputs ringShut.steps == [ [] ]
        && map _.outputs ringShut.steps == [ [] ]
    )
  let aloneShut = unFlow (collapse [ Group (GroupId "one") "One" [ Member (StepId "h") ] ] (Set.singleton (GroupId "one")) (pipeline inOrder))
  expect "a group of one step hides no item and merges no wire: the step is renamed and its ports qualified"
    ( Array.length aloneShut.items == 5 && Array.length aloneShut.wires == 9
        && Array.elem (wire (groupReads "c" (GroupId "one") "h" "in") [ show (reads "c" "h" "in") ]) aloneShut.wires
        && Array.elem (wire (groupWrites (GroupId "one") "h" "out" "d") [ show (writes "h" "out" "d") ]) aloneShut.wires
    )

  section "collapse: the coarser flow is well formed"
  for_
    [ { name: "nothing", closed: Set.empty }
    , { name: "inner", closed: Set.singleton inner }
    , { name: "outer", closed: Set.singleton outer }
    , { name: "both", closed: Set.fromFoldable [ inner, outer ] }
    ]
    \cut -> for_ [ { name: "in order", wires: inOrder }, { name: "g first", wires: gFirst } ] \order -> do
      let coarser = collapse forest cut.closed (pipeline order.wires)
      expect ("the pipeline (" <> order.name <> ") with " <> cut.name <> " shut has no problems (found " <> show (problems coarser) <> ")") (problems coarser == [])
      expectNone ("and its projection under Both is lawful") (projectionFaults Both coarser)

  -- Once a defect (found 2026-11-20 by this search): a port of a closed
  -- group's member that met both an item the collapse hides and an item it
  -- keeps was dropped, leaving the kept wire naming a port the group had
  -- not. A port now goes only when every wire at it meets a hidden item.
  -- The search is over every flow of one step (one input port, one output
  -- port) and two items, the step in a closed group of its own.
  section "collapse: a port that meets a hidden item and a visible one stays"
  let found = Array.filter (\c -> problems c.flow == [] && problems (collapse solo (Set.singleton (GroupId "G")) c.flow) /= []) candidates
  expect ("of the 16 such flows, none collapses to a flow with problems (found " <> show (map _.wires found) <> ")")
    (Array.null found)
  expect "f reads x, writes x and writes y by the same port; shut, x is hidden, and the group keeps the port for the wire to y"
    ( let
        reproduction = bare [ item "x", item "y" ] [ step "f" [ "in" ] [ "out" ] ] [ reads "x" "f" "in", writes "f" "out" "x", writes "f" "out" "y" ]
        coarser = collapse solo (Set.singleton (GroupId "G")) reproduction
      in
        problems reproduction == []
          && problems coarser == []
          && map _.id (unFlow coarser).wires == [ groupWrites (GroupId "G") "f" "out" "y" ]
          && map _.inputs (unFlow coarser).steps == [ [] ]
          && map _.outputs (unFlow coarser).steps == [ [ member "f" "out" ] ]
    )

  -- A step the source named as a group's step once was, and a port named
  -- as a group's port once was, beside the group and the member they
  -- would have been taken for.
  section "collapse: a source's names are never a group's"
  let
    namesakes = bare
      [ item "x", item "y", item "z" ]
      [ step "f" [ "in" ] [ "out" ], step "group:G" [ "f.in" ] [ "f.out" ] ]
      [ reads "x" "f" "in", writes "f" "out" "y", reads "x" "group:G" "f.in", writes "group:G" "f.out" "z" ]
    shut = collapse solo (Set.singleton (GroupId "G")) namesakes
  expect ("a step named group:G with a port named f.out, beside a group G of a step f with a port out: no problems, before or after (found " <> show (problems shut) <> ")")
    (problems namesakes == [] && problems shut == [])
  expect "the group's step and the step named group:G are two steps, each with its own ports"
    ( map _.id (unFlow shut).steps == [ GroupStep (GroupId "G"), StepId "group:G" ]
        && map _.outputs (unFlow shut).steps == [ [ member "f" "out" ], [ PortName "f.out" ] ]
        && map _.inputs (unFlow shut).steps == [ [ member "f" "in" ], [ PortName "f.in" ] ]
    )
  expect "their wires are four, none merged: x is read twice and y and z are each written once"
    ( map _.id (unFlow shut).wires ==
        [ groupReads "x" (GroupId "G") "f" "in"
        , groupWrites (GroupId "G") "f" "out" "y"
        , reads "x" "group:G" "f.in"
        , writes "group:G" "f.out" "z"
        ]
    )
  expect "drawn as steps they are two nodes with two keys, the group under its name"
    (map _.label (project Steps shut).nodes == [ "G", "group:G" ] && Array.length (Array.nub (map _.key (project Steps shut).nodes)) == 2)
  expectNone "and the projection under Both is lawful" (projectionFaults Both shut)

  -- A flow already collapsed, collapsed again: every step of the coarser
  -- flow, closed groups among them, in one group of the second cut.
  section "collapse: a collapsed flow collapses"
  for_ [ { name: "inner", closed: Set.singleton inner }, { name: "outer", closed: Set.singleton outer } ] \cut -> do
    let
      once = collapse forest cut.closed (pipeline inOrder)
      again = collapse [ Group (GroupId "again") "Again" (map (Member <<< _.id) (Array.take 2 (unFlow once).steps)) ] (Set.singleton (GroupId "again")) once
    expect ("the pipeline with " <> cut.name <> " shut, its first two steps then shut as one: no problems (found " <> show (problems again) <> ")")
      (problems once == [] && problems again == [])
    expectNone "and its projection under Both is lawful" (projectionFaults Both again)
  expect "the inner group shut, then it and h shut: the port is the first cut's port, seen from outside the second"
    ( let
        once = collapse forest (Set.singleton inner) (pipeline inOrder)
        again = unFlow (collapse [ Group (GroupId "again") "Again" [ Member (GroupStep inner), Member (StepId "h") ] ] (Set.singleton (GroupId "again")) once)
      in
        map _.id again.steps == [ GroupStep (GroupId "again"), StepId "k" ]
          && map _.inputs (Array.take 1 again.steps) == [ [ MemberPort (GroupStep inner) (member "f" "in") ] ]
          && map _.outputs (Array.take 1 again.steps) == [ [ MemberPort (StepId "h") (PortName "out") ] ]
          && map _.id again.items == [ ItemId "a", ItemId "d", ItemId "e" ]
    )
  for_ fixtures \fixture -> do
    let
      steps = map _.id (unFlow fixture.flow).steps
      first = [ Group (GroupId "first") "First" (map Member (Array.take 1 steps)) ]
      once = collapse first (Set.singleton (GroupId "first")) fixture.flow
      second = [ Group (GroupId "second") "Second" (map (Member <<< _.id) (unFlow once).steps) ]
      again = collapse second (Set.singleton (GroupId "second")) once
    expect ("the fixture " <> fixture.name <> ", its first step shut and then every step shut: no problems either time (found " <> show (problems once <> problems again) <> ")")
      (problems once == [] && problems again == [])
  where
  ringFlow = told
    ( bare [ item "a", item "b" ] [ step "f" [ "in" ] [ "out" ], step "g" [ "in" ] [ "out" ] ]
        [ reads "a" "f" "in", writes "f" "out" "b", reads "b" "g" "in", writes "g" "out" "a" ]
    )

  solo = [ Group (GroupId "G") "G" [ Member (StepId "f") ] ]

  -- every subset of the four wires there can be, fewest wires first
  candidates = Array.sortWith Array.length (subsets possible) <#> \wires ->
    { wires, flow: bare [ item "x", item "y" ] [ step "f" [ "in" ] [ "out" ] ] wires }
  possible = [ reads "x" "f" "in", reads "y" "f" "in", writes "f" "out" "x", writes "f" "out" "y" ]

  subsets :: forall a. Array a -> Array (Array a)
  subsets xs = case Array.uncons xs of
    Nothing -> [ [] ]
    Just { head, tail } -> let rest = subsets tail in map (Array.cons head) rest <> rest
