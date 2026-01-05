module Examples.HouseSantaClaus (main, spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Examples.Common (assertWalk, hasEulerTrail, (~>))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (>|), type (|<), Transit, TransitCore, match, mkStateGraph, mkUpdate, mkUpdateAuto, return)
import Transit.Data.DotLang (GraphvizGraph)
import Transit.Data.DotLang as Graphviz
import Transit.Data.Table (Table)
import Transit.Data.Table as Table
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

type State = Variant
  ( "1" :: {}
  , "2" :: {}
  , "3" :: {}
  , "4" :: {}
  , "5" :: {}
  )

type Msg = Variant
  ( "a" :: {}
  , "b" :: {}
  , "c" :: {}
  , "d" :: {}
  , "e" :: {}
  , "f" :: {}
  , "g" :: {}
  , "h" :: {}
  )

type HouseSantaClausTransit =
  Transit
    :* ("1" |< "a" >| "2")
    :* ("2" |< "b" >| "3")
    :* ("3" |< "c" >| "5")
    :* ("5" |< "d" >| "4")
    :* ("4" |< "e" >| "1")
    :* ("1" |< "f" >| "3")
    :* ("2" |< "g" >| "4")
    :* ("3" |< "h" >| "4")

update :: State -> Msg -> State
update =
  mkUpdateAuto @HouseSantaClausTransit

houseSantaClausTransit :: TransitCore
houseSantaClausTransit = reflectType (Proxy @HouseSantaClausTransit)

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

assert1 :: Aff Unit
assert1 =
  assertWalk update
    (v @"1")
    [ v @"f" ~> v @"3"
    , v @"h" ~> v @"4"
    , v @"g" ~> v @"2"
    , v @"a" ~> v @"1"
    , v @"e" ~> v @"4"
    , v @"d" ~> v @"5"
    , v @"c" ~> v @"3"
    , v @"b" ~> v @"2"
    ]

assert2 :: Aff Unit
assert2 =
  let
    graph = mkStateGraph (reflectType (Proxy @HouseSantaClausTransit))
  in
    hasEulerTrail graph `shouldEqual` true

spec :: Spec Unit
spec = do
  describe "House of Santa Claus" do
    it "asserts" do
      assert1
      assert2

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

generateStateDiagramLight :: Effect Unit
generateStateDiagramLight = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate houseSantaClausTransit _
      { useUndirectedEdges = true
      , theme = themeHarmonyLight
      , layout = TransitGraphviz.Manual
          [ { node: "1", x: 0.0, y: 0.0, exact: true }
          , { node: "2", x: 2.0, y: 0.0, exact: true }
          , { node: "3", x: 2.0, y: 2.0, exact: true }
          , { node: "4", x: 0.0, y: 2.0, exact: true }
          , { node: "5", x: 1.0, y: 3.0, exact: true }
          ]
      }

  FS.writeTextFile UTF8
    "renders/house-santa-claus_graph-light.dot"
    (Graphviz.toDotStr graph)

generateStateDiagramDark :: Effect Unit
generateStateDiagramDark = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate houseSantaClausTransit _
      { useUndirectedEdges = true
      , globalAttrsRaw = Just "layout=neato"
      , theme = themeHarmonyDark
      , layout = TransitGraphviz.Manual
          [ { node: "1", x: 0.0, y: 0.0, exact: true }
          , { node: "2", x: 2.0, y: 0.0, exact: true }
          , { node: "3", x: 2.0, y: 2.0, exact: true }
          , { node: "4", x: 0.0, y: 2.0, exact: true }
          , { node: "5", x: 1.0, y: 3.0, exact: true }
          ]
      }

  FS.writeTextFile UTF8
    "renders/house-santa-claus_graph-dark.dot"
    (Graphviz.toDotStr graph)

generateTransitionTable :: Effect Unit
generateTransitionTable = do
  let
    table :: Table
    table = TransitTable.generate houseSantaClausTransit _
      { useUndirectedEdges = true
      }

  FS.writeTextFile UTF8
    "renders/house-santa-claus_table.md"
    (Table.toMarkdown table)

main :: Effect Unit
main = do
  generateStateDiagramLight
  generateStateDiagramDark
  generateTransitionTable