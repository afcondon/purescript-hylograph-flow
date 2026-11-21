-- | Groups that hold items as well as steps (0.2.0): each case of the
-- | module header of `Hylograph.Flow.Grouping` on a small flow, stated wire
-- | by wire, and then the laws over made flows from a fixed seed. The laws
-- | are stated from the rules, not from `collapse`: which closed group each
-- | step and item is inside is worked out here, from where the generator put
-- | it.
module Test.Flow.Holding (suite) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Foldable (all, for_)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Traversable (for)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Hylograph.Flow (Drawing(..), ElementKey(..), Flow(..), ItemId(..), PortName(..), StepId(..), WireId(..), itemLabel, mapPayloads, portLabel, problems, project, unFlow, wireEnds)
import Hylograph.Flow.Grouping (Collapsed(..), CollapsedItem(..), GroupId(..), Grouping(..), GroupingProblem(..), collapse, groupItem, groupStep, groupingProblems, held, members)
import Hylograph.Flow.Laws (projectionFaults)
import Test.Flow.Check (expect, expectNone, section)
import Test.Flow.Generated (Made, Ports(..), describe, flowOf, holds, someOf, verdict)
import Test.QuickCheck.Gen (Gen, chooseInt)

-- | Items carry their names, steps theirs, wires say what they are.
type Told = Flow String String (Array String)

told :: Array String -> Array { name :: String, inputs :: Array String, outputs :: Array String } -> Array WireId -> Told
told items steps wires = Flow
  { items: items <#> \name -> { id: ItemId name, name: Just name, count: 1, payload: name }
  , steps: steps <#> \st -> { id: StepId st.name, name: Just st.name, inputs: map PortName st.inputs, outputs: map PortName st.outputs, payload: st.name }
  , wires: wires <#> \id -> { id, payload: [ say id ] }
  }

-- | A wire of a source, said shortly: `a>f` for a read, `f>a` for a write.
say :: WireId -> String
say = case _ of
  Reads item step _ -> itemLabel item <> ">" <> stepName step
  Writes step _ item -> stepName step <> ">" <> itemLabel item
  where
  stepName = case _ of
    StepId name -> name
    GroupStep (GroupId group) -> "group:" <> group

reads :: String -> String -> String -> WireId
reads item step port = Reads (ItemId item) (StepId step) (PortName port)

writes :: String -> String -> String -> WireId
writes step port item = Writes (StepId step) (PortName port) (ItemId item)

-- | Two sheets. `Data` holds a table of entered figures, which nothing
-- | writes, and a total its own step makes of them. `Report` reads both.
-- |
-- |     Data:   entered -sum-> total
-- |     Report: entered, total -show-> report
workbook :: Told
workbook = told [ "entered", "total", "report" ]
  [ { name: "sum", inputs: [ "in" ], outputs: [ "out" ] }, { name: "show", inputs: [ "figures", "total" ], outputs: [ "out" ] } ]
  [ reads "entered" "sum" "in"
  , writes "sum" "out" "total"
  , reads "entered" "show" "figures"
  , reads "total" "show" "total"
  , writes "show" "out" "report"
  ]

dataSheet :: GroupId
dataSheet = GroupId "Data"

reportSheet :: GroupId
reportSheet = GroupId "Report"

bySheet :: Array Grouping
bySheet =
  [ Group dataSheet "Data" [ Holds (ItemId "entered"), Holds (ItemId "total"), Member (StepId "sum") ]
  , Group reportSheet "Report" [ Holds (ItemId "report"), Member (StepId "show") ]
  ]

-- | The summary's write, and a read of it by another closed group.
provides :: GroupId -> WireId
provides group = Writes (groupStep group) (SummaryPort group) (groupItem group)

takes :: GroupId -> GroupId -> WireId
takes reader provider = Reads (groupItem provider) (groupStep reader) (SummaryPort provider)

-- | Each ordered pair of nodes an edge joins, in edge order.
ends :: forall i s w. Drawing -> Flow i s w -> Array (Tuple ElementKey ElementKey)
ends drawing flow = (project drawing flow).edges <#> \edge -> Tuple edge.from edge.to

wire :: forall wire. WireId -> wire -> { id :: WireId, payload :: wire }
wire id payload = { id, payload }

-- ---------------------------------------------------------------------------
-- Made flows with items in groups
-- ---------------------------------------------------------------------------

first :: GroupId
first = GroupId "first"

nested :: GroupId
nested = GroupId "nested"

second :: GroupId
second = GroupId "second"

-- | A made flow; where each step and item was put (0: in no group; 1: in
-- | `first`; 2: in `nested`, which is in `first`; 3: in `second`); the
-- | forest that says so; and the groups shut.
type Holding =
  { flow :: Made
  , stepPlaces :: Map StepId Int
  , itemPlaces :: Map ItemId Int
  , forest :: Array Grouping
  , closed :: Set GroupId
  }

holdingOf :: Ports -> Gen Holding
holdingOf ports = do
  flow <- flowOf ports
  steps <- for (unFlow flow).steps \st -> chooseInt 0 3 <#> \place -> Tuple st.id place
  items <- for (unFlow flow).items \it -> chooseInt 0 3 <#> \place -> Tuple it.id place
  closed <- someOf 3 [ first, nested, second ]
  let at place = (Array.filter (\(Tuple _ p) -> p == place) steps <#> \(Tuple step _) -> Member step) <> (Array.filter (\(Tuple _ p) -> p == place) items <#> \(Tuple item _) -> Holds item)
  pure
    { flow
    , stepPlaces: Map.fromFoldable steps
    , itemPlaces: Map.fromFoldable items
    , forest: [ Group first "First" (at 1 <> [ Group nested "Nested" (at 2) ]), Group second "Second" (at 3) ] <> Array.filter isStep (at 0)
    , closed: Set.fromFoldable closed
    }
  where
  isStep = case _ of
    Member _ -> true
    _ -> false

describeHolding :: Holding -> String
describeHolding h = describe h.flow <> "; closed " <> show (Array.fromFoldable h.closed) <> "; steps at " <> show (Map.toUnfoldable h.stepPlaces :: Array _) <> "; items at " <> show (Map.toUnfoldable h.itemPlaces :: Array _)

-- | The outermost closed group over a place.
closedOver :: Set GroupId -> Int -> Maybe GroupId
closedOver closed = case _ of
  1 | Set.member first closed -> Just first
  2
    | Set.member first closed -> Just first
    | Set.member nested closed -> Just nested
  3 | Set.member second closed -> Just second
  _ -> Nothing

-- | From the rules: the closed group each step and each item is inside.
type Sides = { ownerOf :: StepId -> Maybe GroupId, insideOf :: ItemId -> Maybe GroupId }

sidesOf :: Holding -> Sides
sidesOf h = { ownerOf, insideOf }
  where
  wires = map _.id (unFlow h.flow).wires
  ownerOf step = Map.lookup step h.stepPlaces >>= closedOver h.closed
  writersOf item = wires # Array.mapMaybe case _ of
    Writes step _ written | written == item -> Just step
    _ -> Nothing
  readersOf item = wires # Array.mapMaybe case _ of
    Reads read step _ | read == item -> Just step
    _ -> Nothing
  insideOf item = case Map.lookup item h.itemPlaces >>= closedOver h.closed of
    -- held by a closed group: inside it, unless an open step writes it
    Just group -> if all (isJust <<< ownerOf) (writersOf item) then Just group else Nothing
    -- held by none: written in a closed group, read, and read only there
    Nothing -> case writersOf item of
      [ writer ] -> ownerOf writer >>= \group ->
        if not (Array.null (readersOf item)) && all (\reader -> ownerOf reader == Just group) (readersOf item) then Just group else Nothing
      _ -> Nothing

-- | The closed groups at a wire's two ends, the item's and then the step's.
sidesOfWire :: Sides -> WireId -> { item :: Maybe GroupId, step :: Maybe GroupId }
sidesOfWire sides = case _ of
  Reads item step _ -> { item: sides.insideOf item, step: sides.ownerOf step }
  Writes step _ item -> { item: sides.insideOf item, step: sides.ownerOf step }

-- | The closed group a coarser flow's element belongs to: its step or its
-- | summary.
groupOfKey :: ElementKey -> Maybe GroupId
groupOfKey = case _ of
  StepKey (GroupStep group) -> Just group
  ItemKey (GroupItem group) -> Just group
  _ -> Nothing

type Coarser = Flow (CollapsedItem Int) (Collapsed Int String) (Array Int)

isSummaryWrite :: WireId -> Boolean
isSummaryWrite = case _ of
  Writes (GroupStep group) (SummaryPort port) (GroupItem summary) -> group == port && group == summary
  _ -> false

-- | What `collapse` promises of a flow without problems under a forest that
-- | is a partition.
holdingFaults :: Holding -> Coarser -> Array String
holdingFaults h coarser =
  -- conservation at each group's boundary
  ( [ first, nested, second ] # Array.concatMap \group ->
      let
        crossedBefore = Array.sort $ fine.wires # Array.concatMap \w ->
          let at = sidesOfWire sides w.id in if (at.item == Just group) /= (at.step == Just group) then w.payload else []
        crossedAfter = Array.sort $ c.wires # Array.concatMap \w ->
          let at = wireEnds w.id in if (groupOfKey at.from == Just group) /= (groupOfKey at.to == Just group) then w.payload else []
      in
        if crossedBefore == crossedAfter then [] else [ "what crosses the boundary of " <> show group <> " was " <> show crossedBefore <> " and is " <> show crossedAfter ]
  )
    -- every wire is inside one closed group and gone, or is carried once
    <>
      ( let
          outside = Array.sort $ fine.wires # Array.concatMap \w ->
            let at = sidesOfWire sides w.id in if isJust at.item && at.item == at.step then [] else w.payload
          carried = Array.sort (Array.concatMap _.payload (Array.filter (not <<< isSummaryWrite <<< _.id) c.wires))
        in
          if outside == carried then [] else [ "the wires not inside a closed group carried " <> show outside <> " and the coarser flow's carry " <> show carried ]
      )
    -- a summary's write carries what the reads of it carry, and it is read
    <>
      ( Array.filter (isSummaryWrite <<< _.id) c.wires # Array.concatMap \w ->
          let
            summary = (wireEnds w.id).to
            read = Array.sort (Array.concatMap _.payload (Array.filter (\other -> (wireEnds other.id).from == summary) c.wires))
          in
            if not (Array.null read) && Array.sort w.payload == read then [] else [ "the write of " <> show summary <> " carries " <> show w.payload <> " and the reads of it " <> show read ]
      )
    -- every summary item has its write, and no other item is a summary
    <>
      ( c.items # Array.concatMap \it -> case it.id, it.payload of
          GroupItem group, Summary payloadGroup _ | group == payloadGroup ->
            if Array.length (Array.filter (\w -> w.id == Writes (groupStep group) (SummaryPort group) (groupItem group)) c.wires) == 1 then [] else [ "the summary of " <> show group <> " is not written once by its group" ]
          ItemId _, KeptItem _ -> []
          _, _ -> [ show it.id <> " carries the payload of the other kind of item" ]
      )
    -- every step and item of the finer flow is kept or is in one closed group
    <> (if Array.sort (map _.payload fine.steps) == Array.sort stepsAccounted then [] else [ "the steps kept and the steps in closed groups are not the steps there were" ])
    <> (if Array.sort (map _.payload fine.items) == Array.sort itemsAccounted then [] else [ "the items kept and the items in closed groups are not the items there were" ])
    -- between two closed groups, through items inside either: one edge each way
    <>
      ( let
          between = (project Steps coarser).edges # Array.mapMaybe \edge -> case groupOfKey edge.from, groupOfKey edge.to of
            Just from, Just to -> Just { from, to, via: edge.via }
            _, _ -> Nothing
          bySummary = Array.filter (\edge -> Array.any (isJust <<< groupOfKey) edge.via) between <#> \edge -> Tuple edge.from edge.to
          byHeldItem = between # Array.filter \edge -> edge.via # Array.any case _ of
            ItemKey item@(ItemId _) -> isJust (Map.lookup item h.itemPlaces >>= closedOver h.closed)
            _ -> false
        in
          (if Array.length (Array.nub bySummary) == Array.length bySummary then [] else [ "two edges the same way between two closed groups through one summary" ])
            <> (if Array.null byHeldItem then [] else [ "an edge between two closed groups through an item a closed group holds" ])
      )
  where
  sides = sidesOf h
  fine = unFlow h.flow
  c = unFlow coarser
  stepsAccounted = c.steps # Array.concatMap \st -> case st.payload of
    Kept payload -> [ payload ]
    Closed _ inside -> inside.steps
  itemsAccounted = Array.concatMap
    ( \it -> case it.payload of
        KeptItem payload -> [ payload ]
        Summary _ _ -> []
    )
    c.items
    <> Array.concatMap
      ( \st -> case st.payload of
          Kept _ -> []
          Closed _ inside -> inside.items
      )
      c.steps

-- | A coarser flow as ids and what goes between them, ports and the order of
-- | a merge aside: for comparing a cut made at once with one made in stages.
type Outline = { items :: Array ItemId, steps :: Array StepId, between :: Map (Tuple ElementKey ElementKey) (Array Int) }

outline :: forall i s. Flow i s (Array Int) -> Outline
outline (Flow f) =
  { items: Array.sort (map _.id f.items)
  , steps: Array.sort (map _.id f.steps)
  , between: map Array.sort (Map.fromFoldableWith (<>) (f.wires <#> \w -> let at = wireEnds w.id in Tuple (Tuple at.from at.to) w.payload))
  }

-- | `first` shut at once, against `nested` shut and then, in the flow that
-- | gives, `first` shut over what it has become: its own steps and items,
-- | the step `nested` became, and `nested`'s summary.
inStages :: Holding -> Array String
inStages h = if outline atOnce == outline staged then [] else [ "shut at once: " <> show (outline atOnce).between <> "; in stages: " <> show (outline staged).between ]
  where
  atOnce = collapse h.forest (Set.singleton first) h.flow
  inner = collapse h.forest (Set.singleton nested) h.flow
  stillThere = Set.fromFoldable (map _.id (unFlow inner).items)
  stepsStillThere = Set.fromFoldable (map _.id (unFlow inner).steps)
  under = Array.find
    ( case _ of
        Group group _ _ -> group == first
        _ -> false
    )
    h.forest
  became = case under of
    Just group ->
      [ Group first "First"
          ( map Member (Array.filter (\step -> Set.member step stepsStillThere) ([ groupStep nested ] <> members group))
              <> map Holds (Array.filter (\item -> Set.member item stillThere) ([ groupItem nested ] <> held group))
          )
      ]
    Nothing -> []
  staged = collapse became (Set.singleton first) inner

suite :: Effect Unit
suite = do
  section "Grouping: a group names steps and items"
  expect "members are the steps under a grouping and held the items, nested or not, in order"
    ( Array.concatMap members bySheet == [ StepId "sum", StepId "show" ]
        && Array.concatMap held bySheet == [ ItemId "entered", ItemId "total", ItemId "report" ]
        && held (Member (StepId "sum")) == []
        && members (Holds (ItemId "total")) == []
    )
  expect "a forest that is a partition has no problems; a step, an item or a group id said twice is one each"
    ( groupingProblems bySheet == []
        && groupingProblems [ Group dataSheet "Data" [ Member (StepId "sum"), Holds (ItemId "total") ], Group dataSheet "again" [ Member (StepId "sum"), Group reportSheet "Report" [ Holds (ItemId "total") ] ] ]
          == [ StepTwice (StepId "sum"), ItemTwice (ItemId "total"), GroupTwice dataSheet ]
    )
  expect "a summary item and its port are the group's own, and no item or port a source can name"
    ( groupItem dataSheet == GroupItem dataSheet
        && groupItem dataSheet /= ItemId "group:Data"
        && itemLabel (groupItem dataSheet) == "group:Data"
        && SummaryPort dataSheet /= PortName "summary:Data"
        && portLabel (SummaryPort dataSheet) == "summary:Data"
        && show (groupItem dataSheet) == "GroupItem (GroupId \"Data\")"
    )

  section "collapse: two sheets, both shut"
  let bothShut = collapse bySheet (Set.fromFoldable [ dataSheet, reportSheet ]) workbook
  let both = unFlow bothShut
  expect "each sheet is one step, carrying its steps' payloads and its items'"
    ( map _.id both.steps == [ groupStep dataSheet, groupStep reportSheet ]
        && map _.payload both.steps == [ Closed dataSheet { steps: [ "sum" ], items: [ "entered", "total" ] }, Closed reportSheet { steps: [ "show" ], items: [ "report" ] } ]
    )
  expect "the one item left is Data's summary, named for the sheet, standing for the two items Report reads, with their count"
    ( map _.id both.items == [ groupItem dataSheet ]
        && map _.name both.items == [ Just "Data" ]
        && map _.count both.items == [ 2 ]
        && map _.payload both.items == [ Summary dataSheet (NonEmptyArray.cons' "entered" [ "total" ]) ]
    )
  expect "two wires: Data writes its summary and Report reads it, each carrying both reads that crossed, in wire order"
    ( both.wires ==
        [ wire (provides dataSheet) [ "entered>show", "total>show" ]
        , wire (takes reportSheet dataSheet) [ "entered>show", "total>show" ]
        ]
    )
  expect "the sheets' ports are the summary's: Data writes by it, Report reads by it; no member's port is left"
    ( map _.inputs both.steps == [ [], [ SummaryPort dataSheet ] ]
        && map _.outputs both.steps == [ [ SummaryPort dataSheet ], [] ]
    )
  expect ("no problems (found " <> show (problems bothShut) <> ")") (problems workbook == [] && problems bothShut == [])
  expect "as steps: one edge, Data to Report; as holders: the summary alone; as both: step, summary, step"
    ( ends Steps bothShut == [ Tuple (StepKey (groupStep dataSheet)) (StepKey (groupStep reportSheet)) ]
        && map _.key (project Holders bothShut).nodes == [ ItemKey (groupItem dataSheet) ]
        && ends Holders bothShut == []
        && ends Both bothShut == [ Tuple (StepKey (groupStep dataSheet)) (ItemKey (groupItem dataSheet)), Tuple (ItemKey (groupItem dataSheet)) (StepKey (groupStep reportSheet)) ]
    )
  for_ [ Holders, Steps, Both ] \drawing -> expectNone "and the projection is lawful" (projectionFaults drawing bothShut)

  section "collapse: one sheet shut, the other open"
  let dataShut = unFlow (collapse bySheet (Set.singleton dataSheet) workbook)
  expect "Data shut: show stays a step and reads the summary by its own two ports; report stays"
    ( map _.id dataShut.steps == [ groupStep dataSheet, StepId "show" ]
        && map _.id dataShut.items == [ ItemId "report", groupItem dataSheet ]
        && dataShut.wires ==
          [ wire (provides dataSheet) [ "entered>show", "total>show" ]
          , wire (Reads (groupItem dataSheet) (StepId "show") (PortName "figures")) [ "entered>show" ]
          , wire (Reads (groupItem dataSheet) (StepId "show") (PortName "total")) [ "total>show" ]
          , wire (writes "show" "out" "report") [ "show>report" ]
          ]
    )
  let reportShut = unFlow (collapse bySheet (Set.singleton reportSheet) workbook)
  expect "Report shut: it reads entered and total, which stay, by its member's ports, and has no summary: nothing reads its report"
    ( map _.id reportShut.steps == [ StepId "sum", groupStep reportSheet ]
        && map _.id reportShut.items == [ ItemId "entered", ItemId "total" ]
        && map _.id reportShut.wires ==
          [ reads "entered" "sum" "in"
          , writes "sum" "out" "total"
          , Reads (ItemId "entered") (groupStep reportSheet) (MemberPort (StepId "show") (PortName "figures"))
          , Reads (ItemId "total") (groupStep reportSheet) (MemberPort (StepId "show") (PortName "total"))
          ]
        && map _.payload (Array.drop 1 reportShut.steps) == [ Closed reportSheet { steps: [ "show" ], items: [ "report" ] } ]
    )

  section "collapse: a group of items only"
  let
    entry = [ Group (GroupId "Entry") "Entry" [ Holds (ItemId "entered") ], Group (GroupId "Sums") "Sums" [ Member (StepId "sum"), Member (StepId "show"), Holds (ItemId "total"), Holds (ItemId "report") ] ]
    entryShut = collapse entry (Set.singleton (GroupId "Entry")) workbook
    entryOnly = unFlow entryShut
  expect "a sheet of entered data alone, shut: a step with no step inside, after the other steps, that writes its summary, which the readers read"
    ( map _.id entryOnly.steps == [ StepId "sum", StepId "show", groupStep (GroupId "Entry") ]
        && map _.payload (Array.drop 2 entryOnly.steps) == [ Closed (GroupId "Entry") { steps: [], items: [ "entered" ] } ]
        && map _.id entryOnly.items == [ ItemId "total", ItemId "report", groupItem (GroupId "Entry") ]
        && Array.take 3 entryOnly.wires ==
          [ wire (provides (GroupId "Entry")) [ "entered>sum", "entered>show" ]
          , wire (Reads (groupItem (GroupId "Entry")) (StepId "sum") (PortName "in")) [ "entered>sum" ]
          , wire (writes "sum" "out" "total") [ "sum>total" ]
          ]
        && problems entryShut == []
    )
  let unread = collapse [ Group (GroupId "Notes") "Notes" [ Holds (ItemId "report") ] ] (Set.singleton (GroupId "Notes")) workbook
  expect "a group whose one item an open step writes, shut: the item stays, and the group, with nothing inside it, is no step"
    ( map _.id (unFlow unread).items == [ ItemId "entered", ItemId "total", ItemId "report" ]
        && map _.id (unFlow unread).steps == [ StepId "sum", StepId "show" ]
    )
  let lonely = collapse [ Group (GroupId "Notes") "Notes" [ Holds (ItemId "entered") ] ] (Set.singleton (GroupId "Notes")) (told [ "entered" ] [] [])
  expect "a group of one item that nothing writes or reads, shut: one step, no item, no wire"
    ( map _.id (unFlow lonely).steps == [ groupStep (GroupId "Notes") ]
        && (unFlow lonely).items == []
        && (unFlow lonely).wires == []
        && map (\st -> st.inputs <> st.outputs) (unFlow lonely).steps == [ [] ]
    )

  section "collapse: an item held by one group and written by another"
  -- `show`, on Report, writes `total`'s neighbour `posted`, which is on Data.
  let
    posting = told [ "entered", "posted" ] [ { name: "show", inputs: [ "in" ], outputs: [ "out" ] } ]
      [ reads "entered" "show" "in", writes "show" "out" "posted" ]
    postingForest = [ Group dataSheet "Data" [ Holds (ItemId "entered"), Holds (ItemId "posted") ], Group reportSheet "Report" [ Member (StepId "show") ] ]
    posted = collapse postingForest (Set.fromFoldable [ dataSheet, reportSheet ]) posting
  expect "both shut: each group provides to the other, by its own summary: Data what Report reads, Report what it writes into Data"
    ( (unFlow posted).wires ==
        [ wire (provides dataSheet) [ "entered>show" ]
        , wire (takes reportSheet dataSheet) [ "entered>show" ]
        , wire (provides reportSheet) [ "show>posted" ]
        , wire (takes dataSheet reportSheet) [ "show>posted" ]
        ]
        && map _.payload (unFlow posted).items == [ Summary dataSheet (NonEmptyArray.singleton "entered"), Summary reportSheet (NonEmptyArray.singleton "posted") ]
        && problems posted == []
    )
  expect "as steps that is a cycle of two edges, neither dropped"
    ( ends Steps posted ==
        [ Tuple (StepKey (groupStep dataSheet)) (StepKey (groupStep reportSheet))
        , Tuple (StepKey (groupStep reportSheet)) (StepKey (groupStep dataSheet))
        ]
    )
  let postedOpen = collapse postingForest (Set.singleton dataSheet) posting
  expect "Data shut and Report open: posted, which an open step writes, stays drawn, though Data holds it; entered is inside"
    ( map _.id (unFlow postedOpen).items == [ ItemId "posted", groupItem dataSheet ]
        && map _.id (unFlow postedOpen).wires == [ provides dataSheet, Reads (groupItem dataSheet) (StepId "show") (PortName "in"), writes "show" "out" "posted" ]
        && map _.payload (Array.drop 1 (unFlow postedOpen).steps) == [ Closed dataSheet { steps: [], items: [ "entered" ] } ]
        && problems postedOpen == []
    )

  section "collapse: nested groups, only some shut"
  let
    -- Book [ Data [ entered, total, sum ], report, show ]
    nestedSheets = [ Group (GroupId "Book") "Book" [ Group dataSheet "Data" [ Holds (ItemId "entered"), Holds (ItemId "total"), Member (StepId "sum") ], Holds (ItemId "report"), Member (StepId "show") ] ]
    innerShut = collapse nestedSheets (Set.singleton dataSheet) workbook
    outerShut = collapse nestedSheets (Set.fromFoldable [ GroupId "Book", dataSheet ]) workbook
  expect "the inner group shut in an open outer one: as Data shut alone"
    (map _.id (unFlow innerShut).wires == map _.id dataShut.wires && map _.id (unFlow innerShut).items == map _.id dataShut.items)
  expect "the outer shut: one step with everything inside, no item, no wire, whatever is said of the inner"
    ( (unFlow outerShut).items == [] && (unFlow outerShut).wires == []
        && map _.payload (unFlow outerShut).steps == [ Closed (GroupId "Book") { steps: [ "sum", "show" ], items: [ "entered", "total", "report" ] } ]
    )

  section "collapse: a collapsed flow, with its summary, collapses"
  let
    again = collapse
      [ Group (GroupId "Book") "Book" [ Member (groupStep dataSheet), Holds (groupItem dataSheet), Member (StepId "show"), Holds (ItemId "report") ] ]
      (Set.singleton (GroupId "Book"))
      (collapse bySheet (Set.singleton dataSheet) workbook)
  expect "Data shut, then Data's step and summary shut with the rest: one step, no item, no wire, no problems"
    ( map _.id (unFlow again).steps == [ groupStep (GroupId "Book") ] && (unFlow again).items == [] && (unFlow again).wires == [] && problems again == [] )
  let
    namesake = told [ "group:Data", "entered" ] [ { name: "show", inputs: [ "in" ], outputs: [] } ] [ reads "group:Data" "show" "in", reads "entered" "show" "in" ]
    namesakeShut = collapse [ Group dataSheet "Data" [ Holds (ItemId "entered") ] ] (Set.singleton dataSheet) namesake
  expect "an item the source named group:Data beside the summary of a group Data: two items, two wires, no problems"
    ( map _.id (unFlow namesakeShut).items == [ ItemId "group:Data", groupItem dataSheet ]
        && Array.length (unFlow namesakeShut).wires == 3
        && problems namesakeShut == []
    )

  section "made flows with items in groups: collapse"
  holds "the forests made are partitions" 300 $ holdingOf Shared <#> \h ->
    verdict (describeHolding h) (map show (groupingProblems h.forest))
  holds "with nothing shut, collapse is mapPayloads KeptItem Kept identity" 300 $ holdingOf Shared <#> \h ->
    verdict (describeHolding h) (if collapse h.forest Set.empty h.flow == mapPayloads KeptItem Kept identity h.flow then [] else [ "collapsing nothing changed the flow" ])
  holds "one wire to a port: the coarser flow has no problems" 1000 $ holdingOf OneWireEach <#> \h ->
    verdict (describeHolding h) (map show (problems (collapse h.forest h.closed h.flow)))
  holds "ports shared: the coarser flow has no problems" 1000 $ holdingOf Shared <#> \h ->
    verdict (describeHolding h) (map show (problems (collapse h.forest h.closed h.flow)))
  holds "the coarser flow's projections are lawful under Holders, Steps and Both" 500 $ holdingOf Shared <#> \h ->
    verdict (describeHolding h) ([ Holders, Steps, Both ] # Array.concatMap \drawing -> projectionFaults drawing (collapse h.forest h.closed h.flow))
  holds "what crosses each group's boundary is conserved; a summary's write carries its reads; everything is kept or inside one group; one edge each way between closed groups" 1500 $ holdingOf Shared <#> \h ->
    verdict (describeHolding h) (holdingFaults h (collapse h.forest h.closed h.flow))
  holds "likewise with one wire to a port" 500 $ holdingOf OneWireEach <#> \h ->
    verdict (describeHolding h) (holdingFaults h (collapse h.forest h.closed h.flow))
  holds "a group shut at once is the group shut in stages: the inner, then the outer over what the inner became" 1000 $ holdingOf Shared <#> \h ->
    verdict (describeHolding h) (inStages h)
  holds "collapse is a function: the same cut twice is the same flow" 200 $ holdingOf Shared <#> \h ->
    verdict (describeHolding h) (if collapse h.forest h.closed h.flow == collapse h.forest h.closed h.flow then [] else [ "two collapses of one flow differ" ])
