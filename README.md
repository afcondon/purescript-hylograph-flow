# hylograph-flow

One value for pictures of computation.

A spreadsheet's columns and formulas, a Makefile's files and rules, a signal
chain's blocks and streams, a Sankey chart's stocks and transfers: each is
things that hold a value, things that compute, and connections between
them. `hylograph-flow` is that shape as a PureScript value, with the
questions a drawing asks of it answered once, for all of them.

It draws nothing itself. A renderer takes a *projection* of a flow, which
is plain nodes and edges; `hylograph-flow-render` is one such renderer.

## What a flow is

A `Flow item step wire` has

- **items**: what holds a value (a column, a file, a stream, a store);
- **steps**: what computes, each with named input and output ports;
- **wires**: a step *reads* an item by an input port, or *writes* an item by
  an output port. There is no wire from item to item or from step to step.

Whatever else a domain knows rides in the three type parameters (a formula
on a step, a quantity on a wire) and the library never looks at it. A flow
with nothing to say there uses `Unit`.

Three things are deliberately not in the value, and sit beside it:

- *what is drawn* is an argument to `project`;
- *run state* (stale, running, broken) is an `Overlay`, keyed by element;
- *grouping* is a `Grouping`, and `collapse` makes the coarser flow.

## The types

```purescript
newtype GroupId = GroupId String

data ItemId
  = ItemId String        -- an item of the source
  | GroupItem GroupId    -- the summary item of a closed group: what it provides

data StepId
  = StepId String        -- a step of the source
  | GroupStep GroupId    -- the step a closed group became

data PortName
  = PortName String              -- a port of the source's step
  | MemberPort StepId PortName   -- a closed group's port: a member's port, seen from outside
  | SummaryPort GroupId          -- a closed group's port to a summary item: its own, or one it reads

type Item item = { id :: ItemId, name :: Maybe String, count :: Int, payload :: item }
type Step step = { id :: StepId, name :: Maybe String, inputs :: Array PortName, outputs :: Array PortName, payload :: step }

data WireId
  = Reads ItemId StepId PortName
  | Writes StepId PortName ItemId

type Wire wire = { id :: WireId, payload :: wire }

newtype Flow item step wire = Flow
  { items :: Array (Item item)
  , steps :: Array (Step step)
  , wires :: Array (Wire wire)
  }
```

A wire's identity is its two ends and its port; what it carries is no part
of it. A closed group's step, its summary item and its ports have
constructors of their own, so no name a source gives a step, an item or a
port can be mistaken for a group's, and a collapsed flow can be collapsed
again. `stepLabel`, `itemLabel` and `portLabel` turn an id into words for a
reader.

And beside the flow (`Hylograph.Flow.Grouping`):

```purescript
data Grouping
  = Member StepId                         -- a step
  | Holds ItemId                          -- an item
  | Group GroupId String (Array Grouping)

-- a step of the coarser flow
data Collapsed item step
  = Kept step
  | Closed GroupId { steps :: Array step, items :: Array item }

-- an item of the coarser flow
data CollapsedItem item
  = KeptItem item
  | Summary GroupId (NonEmptyArray item)

collapse :: Semigroup w => Array Grouping -> Set GroupId -> Flow i s w -> Flow (CollapsedItem i) (Collapsed i s) w
```

A closed group is **one step**, which carries the payloads of every step
and item shut inside it, and **at most one item**, its summary, which the
group's step writes and whatever reads from the group reads. Everything
that goes from one closed group to another through items inside either is
one wire, so as `Steps` there is one edge each way between them. What
crosses a group's boundary is conserved: the wires that crossed are the
wires at the group's two nodes, their payloads merged with `<>` where
several became one. The header of `Hylograph.Flow.Grouping` has the rule
for every wire.

## The modules

| Module | What it holds |
|---|---|
| `Hylograph.Flow` | The value. `problems` (well-formedness). `project :: Drawing -> Flow i s w -> { nodes, edges }`, which is what a renderer draws; each edge carries the wires it stands for. `layers`, `leafMass`, `isolated`, `writerOf`, `readersOf`. `mapPayloads`, `erase`. |
| `Hylograph.Flow.Laws` | `projectionFaults`: what must hold of any projection of any flow, as a function that returns what does not. Use it in your own tests, on your own flows. |
| `Hylograph.Flow.Overlay` | Facts about elements, beside the flow, keyed by `ElementKey`. |
| `Hylograph.Flow.Grouping` | A nested partition of steps and items, and `collapse`, which shuts each closed group into one step and, where it provides anything, one summary item, and merges the wires left parallel. `groupingProblems`: what keeps a forest from being a partition. |
| `Hylograph.Flow.Collection` | One layer up: a step payload saying a step is a `Map`, `Fold`, `Scan`, `Join` or `Filter` over a collection, or `Unclassified`. |

An importer from some existing source should not throw. The type
`Imported misfit item step wire` is a flow together with the list of what
did not fit.

### Drawings

`project` takes a `Drawing`, which says which elements are nodes. Every
element not drawn is contracted: an edge runs between two drawn elements for
each path of wires between them through undrawn ones.

| Drawing | Nodes | Good for |
|---|---|---|
| `Holders` | items; each step becomes edges | columns of a spreadsheet, files of a build |
| `Steps` | steps; each item becomes edges | blocks of a signal chain |
| `Both` | items and steps; every wire is an edge | the bipartite picture |
| `Chosen f` | whatever `f` says, element by element | pictures that draw some of each |

## A worked example

Flour and water are mixed into dough, and dough is baked into bread.

```purescript
import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Hylograph.Flow (Drawing(..), Flow(..), ItemId(..), PortName(..), Projection, StepId(..), WireId(..), plain, problems, project)
import Hylograph.Flow.Grouping (Collapsed, CollapsedItem, GroupId(..), Grouping(..), collapse)

bakery :: Flow Unit Unit Unit
bakery = Flow
  { items: [ item "flour", item "water", item "dough", item "bread" ]
  , steps:
      [ { id: StepId "mix", name: Just "mix", inputs: [ PortName "dry", PortName "wet" ], outputs: [ PortName "out" ], payload: unit }
      , { id: StepId "bake", name: Just "bake", inputs: [ PortName "in" ], outputs: [ PortName "out" ], payload: unit }
      ]
  , wires: map plain
      [ Reads (ItemId "flour") (StepId "mix") (PortName "dry")
      , Reads (ItemId "water") (StepId "mix") (PortName "wet")
      , Writes (StepId "mix") (PortName "out") (ItemId "dough")
      , Reads (ItemId "dough") (StepId "bake") (PortName "in")
      , Writes (StepId "bake") (PortName "out") (ItemId "bread")
      ]
  }
  where
  item name = { id: ItemId name, name: Just name, count: 1, payload: unit }

-- problems bakery == []

-- Items as nodes, steps contracted to edges:
-- flour -> dough, water -> dough, dough -> bread.
byItems :: Projection Unit
byItems = project Holders bakery

-- Steps as nodes, items contracted to edges: mix -> bake.
bySteps :: Projection Unit
bySteps = project Steps bakery

-- Both steps shut into one group. The dough, which only the group writes
-- and reads, is hidden; the group reads flour and water and writes bread.
kitchen :: Flow (CollapsedItem Unit) (Collapsed Unit Unit) Unit
kitchen = collapse
  [ Group (GroupId "kitchen") "Kitchen" [ Member (StepId "mix"), Member (StepId "bake") ] ]
  (Set.singleton (GroupId "kitchen"))
  bakery

-- A group holds items as well as steps. The larder holds the flour and the
-- water; the kitchen holds its two steps, the dough and the bread. Both
-- shut, there are two steps and one item between them: the larder's
-- summary, which the larder writes and the kitchen reads.
shops :: Flow (CollapsedItem Unit) (Collapsed Unit Unit) Unit
shops = collapse
  [ Group (GroupId "larder") "Larder" [ Holds (ItemId "flour"), Holds (ItemId "water") ]
  , Group (GroupId "kitchen") "Kitchen" [ Member (StepId "mix"), Member (StepId "bake"), Holds (ItemId "dough"), Holds (ItemId "bread") ]
  ]
  (Set.fromFoldable [ GroupId "larder", GroupId "kitchen" ])
  bakery
```

`kitchen` has one step, `GroupStep (GroupId "kitchen")`, whose ports are
`MemberPort (StepId "mix") (PortName "dry")`, `MemberPort (StepId "mix")
(PortName "wet")` and `MemberPort (StepId "bake") (PortName "out")`.

`shops` has two steps, `GroupStep (GroupId "kitchen")` and `GroupStep
(GroupId "larder")`, one item, `GroupItem (GroupId "larder")`, and two
wires: `Writes (GroupStep (GroupId "larder")) (SummaryPort (GroupId
"larder")) (GroupItem (GroupId "larder"))` and `Reads (GroupItem (GroupId
"larder")) (GroupStep (GroupId "kitchen")) (SummaryPort (GroupId
"larder"))`. Drawn as `Steps` that is one edge, larder to kitchen, standing
for the two reads that crossed. The kitchen's step carries `Closed (GroupId
"kitchen") { steps: [ unit, unit ], items: [ unit, unit ] }`: what is shut
inside it.

This example is compiled and checked with the tests
(`test/Test/Flow/Example.purs`).

## Changes in 0.2.0

A group holds items as well as steps. What breaks:

- `Grouping` has a third constructor, `Holds ItemId`. A grouping written
  with `Member` and `Group` alone compiles and means what it meant; a
  `case` over `Grouping` needs a branch for `Holds`.
- `collapse` gives `Flow (CollapsedItem i) (Collapsed i s) w`, where it gave
  `Flow i (Collapsed s) w`: an item's payload is in `KeptItem`, or is a
  `Summary`.
- `Collapsed` has two parameters, the item's payload and the step's, and
  `Closed` carries a record, `{ steps, items }`, where it carried the
  steps' payloads alone. `items` holds the payloads of the items shut
  inside the group, those 0.1 hid among them.
- `ItemId` is a `data` type with a second constructor, `GroupItem GroupId`,
  where it was a `newtype`. `ItemId "a"` still makes one; a pattern
  `(ItemId name)` that was total is partial now (`itemLabel`, new, gives
  the words).
- `PortName` has a third constructor, `SummaryPort GroupId`.
- A closed group with no step of the flow under it, which was nothing, is
  a step where it holds items of the flow.

On a grouping that names steps alone, `collapse` gives the wires, the
steps, their ports and the items that 0.1.1 gave, in the same order (the
tests hold it to 0.1.1's `collapse`, word for word, on made flows with
and without problems). New: `Holds`, `held`, `groupItem`, `itemLabel`,
`CollapsedItem`, `groupingProblems` and `GroupingProblem`.

## Installing

```bash
spago install hylograph-flow
```

It depends on `arrays`, `foldable-traversable`, `maybe`,
`ordered-collections`, `prelude` and `tuples`, and has no foreign code, so
it builds on any PureScript backend.

## Running the tests

```bash
spago test
```

The suite makes its own flows and needs registry packages only. It covers:
a flow for each `Problem`, with that problem alone; `project` under every
drawing, stated edge by edge on a chain, a fan-out, a fan-in, a one-step
cycle, a ring and isolated elements; the projection law on every fixture
under every drawing; overlays before and after `erase`; `collapse` on
nested groups, on a flow already collapsed, on parallel wires, and on steps
and ports whose names look like a group's; groups that hold items (a group
of items only, an item held by one group and written by another, two
groups that read each other, nested groups with some shut); and properties
over small generated flows from a fixed seed, run pure, so that every run
is the same run. Among those properties: the coarser flow has no problems;
what crosses each group's boundary is conserved; a group shut at once is
the same group shut in stages; and, on every grouping of steps alone,
`collapse` gives what 0.1.1's gave (kept in `test/Test/Flow/Reference011.purs`).

The library was developed against four real sources of quite different
kinds, each imported, erased, and compared fact for fact with the picture
its own program draws.

## Known limits

`collapse` gives a flow without problems from a flow without problems; the
generated tests hold it to that, with ports shared by several wires or not,
and with items in groups or not.

An item that a closed group holds and an *open* step writes stays drawn,
outside the group that holds it. A wire joins an item and a step, so
something must stand between the open step and the group's step, and the
item itself is the one thing there that is not invented. Shut the writer's
group too and the item goes inside.

Between two closed groups there is one edge each way under `Steps` through
the items inside them; an item that no closed group holds, written by one
and read by the other, is still drawn and is a second way between them.
Hold every item and there is one.

One case is left to the caller: collapsing an already collapsed flow with a
group id the first collapse used, in a group that does not contain that
first group's step, gives two steps one identity (`DuplicateStep`). Use a
group id once.

## Licence

MIT.
