-- | The worked example of the README, compiled and held to what the README
-- | says of it.
module Test.Flow.Example
  ( bakery
  , byItems
  , bySteps
  , kitchen
  , shops
  , suite
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect (Effect)
import Hylograph.Flow (Drawing(..), Flow(..), ItemId(..), PortName(..), Projection, StepId(..), WireId(..), plain, problems, project, unFlow)
import Hylograph.Flow.Grouping (Collapsed(..), CollapsedItem, GroupId(..), Grouping(..), collapse)
import Test.Flow.Check (expect, section)

-- | Flour and water are mixed into dough; dough is baked into bread.
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

-- | Items as nodes, steps contracted to edges:
-- | flour -> dough, water -> dough, dough -> bread.
byItems :: Projection Unit
byItems = project Holders bakery

-- | Steps as nodes, items contracted to edges: mix -> bake.
bySteps :: Projection Unit
bySteps = project Steps bakery

-- | Both steps shut into one group. The dough, which only the group writes
-- | and reads, is hidden; the group reads flour and water and writes bread.
kitchen :: Flow (CollapsedItem Unit) (Collapsed Unit Unit) Unit
kitchen = collapse
  [ Group (GroupId "kitchen") "Kitchen" [ Member (StepId "mix"), Member (StepId "bake") ] ]
  (Set.singleton (GroupId "kitchen"))
  bakery

-- | A group holds items as well as steps. The larder holds the flour and
-- | the water; the kitchen holds its two steps, the dough and the bread.
-- | Both shut, there are two steps and one item between them: the larder's
-- | summary, which the larder writes and the kitchen reads.
shops :: Flow (CollapsedItem Unit) (Collapsed Unit Unit) Unit
shops = collapse
  [ Group (GroupId "larder") "Larder" [ Holds (ItemId "flour"), Holds (ItemId "water") ]
  , Group (GroupId "kitchen") "Kitchen" [ Member (StepId "mix"), Member (StepId "bake"), Holds (ItemId "dough"), Holds (ItemId "bread") ]
  ]
  (Set.fromFoldable [ GroupId "larder", GroupId "kitchen" ])
  bakery

suite :: Effect Unit
suite = do
  section "the README's example"
  expect "the bakery has no problems" (problems bakery == [])
  expect "by items: four nodes and three edges" (Array.length byItems.nodes == 4 && Array.length byItems.edges == 3)
  expect "by steps: two nodes and one edge" (Array.length bySteps.nodes == 2 && Array.length bySteps.edges == 1)
  expect "the kitchen is one step, the group's own, over flour, water and bread, with no problems"
    ( map _.id (unFlow kitchen).steps == [ GroupStep (GroupId "kitchen") ]
        && map _.id (unFlow kitchen).items == [ ItemId "flour", ItemId "water", ItemId "bread" ]
        && Array.length (unFlow kitchen).wires == 3
        && problems kitchen == []
        && map _.inputs (unFlow kitchen).steps == [ [ MemberPort (StepId "mix") (PortName "dry"), MemberPort (StepId "mix") (PortName "wet") ] ]
        && map _.outputs (unFlow kitchen).steps == [ [ MemberPort (StepId "bake") (PortName "out") ] ]
    )
  expect "the shops are two steps, the larder's summary between them, and two wires; as steps, one edge"
    ( map _.id (unFlow shops).steps == [ GroupStep (GroupId "kitchen"), GroupStep (GroupId "larder") ]
        && map _.id (unFlow shops).items == [ GroupItem (GroupId "larder") ]
        && map _.id (unFlow shops).wires ==
          [ Writes (GroupStep (GroupId "larder")) (SummaryPort (GroupId "larder")) (GroupItem (GroupId "larder"))
          , Reads (GroupItem (GroupId "larder")) (GroupStep (GroupId "kitchen")) (SummaryPort (GroupId "larder"))
          ]
        && Array.length (project Steps shops).edges == 1
        && map _.payload (Array.take 1 (unFlow shops).steps) == [ Closed (GroupId "kitchen") { steps: [ unit, unit ], items: [ unit, unit ] } ]
        && problems shops == []
    )
