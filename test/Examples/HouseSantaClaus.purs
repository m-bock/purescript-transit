module Examples.HouseSantaClaus (main, spec) where

import Prelude

import Data.Int as Int
import Data.Newtype (unwrap)
import Data.Reflectable (reflectType)
import Data.Variant (Variant)
import Effect (Effect)
import Examples.Common (assertWalk, hasEulerTrail, (~>))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (>|), type (|<), Transit, TransitCore, StateGraph, mkStateGraph, mkUpdateAuto)
import Transit.Data.DotLang (GraphvizGraph)
import Transit.Data.DotLang as Graphviz
import Transit.Data.Table (Table)
import Transit.Data.Table as Table
import Transit.Render.Graphviz (Inch(..), Layout(..))
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Prelude (Proxy(..))

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

type SantaTransit =
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
  mkUpdateAuto @SantaTransit

santaTransit :: TransitCore
santaTransit = reflectType (Proxy @SantaTransit)

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

specWalk :: Spec Unit
specWalk =
  it "should follow the walk and visit the expected intermediate states" do
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

specEulerTrail :: Spec Unit
specEulerTrail =
  it "should have an Eulerian trail" do
    let
      graph :: StateGraph
      graph = mkStateGraph santaTransit

    hasEulerTrail graph `shouldEqual` true

spec :: Spec Unit
spec = do
  describe "House of Santa Claus" do
    specWalk
    specEulerTrail

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

baseUnit :: Inch
baseUnit = Inch 0.6

units2D :: Int -> Int -> { x :: Inch, y :: Inch }
units2D x y =
  { x: Inch (Int.toNumber x * unwrap baseUnit)
  , y: Inch (Int.toNumber y * unwrap baseUnit)
  }

generateGraphLight :: Effect Unit
generateGraphLight = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate santaTransit _
      { undirectedEdges = true
      , theme = themeHarmonyLight
      , layout = Manual
          [ { node: "1", pos: units2D 0 0, exact: true }
          , { node: "2", pos: units2D 2 0, exact: true }
          , { node: "3", pos: units2D 2 2, exact: true }
          , { node: "4", pos: units2D 0 2, exact: true }
          , { node: "5", pos: units2D 1 4, exact: true }
          ]
      , fixedNodeSize = pure $ units2D 1 1
      , fontSize = 16.0
      }

  FS.writeTextFile UTF8
    "renders/house-santa-claus_graph-light.dot"
    (Graphviz.toDotStr graph)

generateGraphDark :: Effect Unit
generateGraphDark = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate santaTransit _
      { undirectedEdges = true
      , theme = themeHarmonyDark
      , layout = Manual
          [ { node: "1", pos: units2D 0 0, exact: true }
          , { node: "2", pos: units2D 2 0, exact: true }
          , { node: "3", pos: units2D 2 2, exact: true }
          , { node: "4", pos: units2D 0 2, exact: true }
          , { node: "5", pos: units2D 1 4, exact: true }
          ]
      , fixedNodeSize = pure $ units2D 1 1
      , fontSize = 16.0
      }

  FS.writeTextFile UTF8
    "renders/house-santa-claus_graph-dark.dot"
    (Graphviz.toDotStr graph)

generateTable :: Effect Unit
generateTable = do
  let
    table :: Table
    table = TransitTable.generate santaTransit _
      { undirectedEdges = true
      }

  FS.writeTextFile UTF8
    "renders/house-santa-claus_table.md"
    (Table.toMarkdown table)

main :: Effect Unit
main = do
  generateGraphLight
  generateGraphDark
  generateTable