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
newtype ItemId = ItemId String
newtype GroupId = GroupId String

data StepId
  = StepId String        -- a step of the source
  | GroupStep GroupId    -- the step a closed group became

data PortName
  = PortName String              -- a port of the source's step
  | MemberPort StepId PortName   -- a closed group's port: a member's port, seen from outside

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
of it. A closed group's step and ports have constructors of their own, so
no name a source gives a step or a port can be mistaken for a group's, and
a collapsed flow can be collapsed again. `stepLabel` and `portLabel` turn
an id into words for a reader.

## The modules

| Module | What it holds |
|---|---|
| `Hylograph.Flow` | The value. `problems` (well-formedness). `project :: Drawing -> Flow i s w -> { nodes, edges }`, which is what a renderer draws; each edge carries the wires it stands for. `layers`, `leafMass`, `isolated`, `writerOf`, `readersOf`. `mapPayloads`, `erase`. |
| `Hylograph.Flow.Laws` | `projectionFaults`: what must hold of any projection of any flow, as a function that returns what does not. Use it in your own tests, on your own flows. |
| `Hylograph.Flow.Overlay` | Facts about elements, beside the flow, keyed by `ElementKey`. |
| `Hylograph.Flow.Grouping` | A nested partition of steps, and `collapse`, which shuts groups into single steps, hides the items only they touch, and merges the wires left parallel. |
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
import Hylograph.Flow.Grouping (Collapsed, GroupId(..), Grouping(..), collapse)

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
kitchen :: Flow Unit (Collapsed Unit) Unit
kitchen = collapse
  [ Group (GroupId "kitchen") "Kitchen" [ Member (StepId "mix"), Member (StepId "bake") ] ]
  (Set.singleton (GroupId "kitchen"))
  bakery
```

`kitchen` has one step, `GroupStep (GroupId "kitchen")`, whose ports are
`MemberPort (StepId "mix") (PortName "dry")`, `MemberPort (StepId "mix")
(PortName "wet")` and `MemberPort (StepId "bake") (PortName "out")`.

This example is compiled and checked with the tests
(`test/Test/Flow/Example.purs`).

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
and ports whose names look like a group's; and properties over small
generated flows from a fixed seed, run pure, so that every run is the same
run.

The library was developed against four real sources of quite different
kinds, each imported, erased, and compared fact for fact with the picture
its own program draws.

## Known limit

`collapse` gives a flow without problems from a flow without problems; the
generated tests hold it to that, with ports shared by several wires or not.
One case is left to the caller: collapsing an already collapsed flow with a
group id the first collapse used, in a group that does not contain that
first group's step, gives two steps one identity (`DuplicateStep`). Use a
group id once.

## Licence

MIT.
