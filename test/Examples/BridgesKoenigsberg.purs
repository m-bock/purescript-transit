module Examples.BridgesKoenigsberg (main, spec) where

import Prelude

import Data.Reflectable (reflectType)
import Data.Variant (Variant)
import Effect (Effect)
import Examples.Common (assertWalk, hasEulerTrail, nodeDegree, (~>))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (>|), type (|<), Transit, TransitCore, match, mkUpdate, return)
import Transit.Data.DotLang (GraphvizGraph)
import Transit.Data.DotLang as Graphviz
import Transit.Data.Table (Table)
import Transit.Data.Table as Table
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.TransitionTable as TransitTable
import Transit.StateGraph (StateGraph, mkStateGraph)
import Transit.VariantUtils (v)
import Type.Prelude (Proxy(..))

type State = Variant
  ( "A" :: {}
  , "B" :: {}
  , "C" :: {}
  , "D" :: {}
  )

type Msg = Variant
  ( "a" :: {}
  , "b" :: {}
  , "c" :: {}
  , "d" :: {}
  , "e" :: {}
  , "f" :: {}
  , "g" :: {}
  )

type BridgesTransit =
  Transit
    :* ("A" |< "a" >| "B")
    :* ("A" |< "b" >| "B")
    :* ("A" |< "c" >| "C")
    :* ("A" |< "d" >| "C")
    :* ("A" |< "e" >| "D")
    :* ("B" |< "f" >| "D")
    :* ("C" |< "g" >| "D")

update :: State -> Msg -> State
update = mkUpdate @BridgesTransit
  (match @"A" @"a" \_ _ -> return @"B")
  (match @"B" @"a" \_ _ -> return @"A")

  (match @"A" @"b" \_ _ -> return @"B")
  (match @"B" @"b" \_ _ -> return @"A")

  (match @"A" @"c" \_ _ -> return @"C")
  (match @"C" @"c" \_ _ -> return @"A")

  (match @"A" @"d" \_ _ -> return @"C")
  (match @"C" @"d" \_ _ -> return @"A")

  (match @"A" @"e" \_ _ -> return @"D")
  (match @"D" @"e" \_ _ -> return @"A")

  (match @"B" @"f" \_ _ -> return @"D")
  (match @"D" @"f" \_ _ -> return @"B")

  (match @"C" @"g" \_ _ -> return @"D")
  (match @"D" @"g" \_ _ -> return @"C")

bridgesTransit :: TransitCore
bridgesTransit = reflectType (Proxy @BridgesTransit)

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

specSampleWalk :: Spec Unit
specSampleWalk =
  it "should follow the sample walk and visit the expected intermediate states" do
    assertWalk update
      (v @"A")
      [ v @"a" ~> v @"B"
      , v @"f" ~> v @"D"
      , v @"g" ~> v @"C"
      , v @"c" ~> v @"A"
      , v @"e" ~> v @"D"
      , v @"g" ~> v @"C"
      , v @"d" ~> v @"A"
      , v @"b" ~> v @"B"
      ]

bridgesGraph :: StateGraph
bridgesGraph = mkStateGraph bridgesTransit

specNodeDegree :: Spec Unit
specNodeDegree = do
  it "should each node have the expected degree" do
    nodeDegree bridgesGraph "A" `shouldEqual` 5
    nodeDegree bridgesGraph "B" `shouldEqual` 3
    nodeDegree bridgesGraph "C" `shouldEqual` 3
    nodeDegree bridgesGraph "D" `shouldEqual` 3

specEulerTrail :: Spec Unit
specEulerTrail = do
  it "should not have an Eulerian trail" do
    hasEulerTrail bridgesGraph `shouldEqual` false

spec :: Spec Unit
spec = do
  describe "BridgesKoenigsberg" do
    specSampleWalk
    specNodeDegree
    specEulerTrail

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

generateGraphLight :: Effect Unit
generateGraphLight = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate bridgesTransit _
      { theme = themeHarmonyLight
      , undirectedEdges = true
      , fontSize = 15.0
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg_graph-light.dot" (Graphviz.toDotStr graph)

generateGraphDark :: Effect Unit
generateGraphDark = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate bridgesTransit _
      { theme = themeHarmonyDark
      , undirectedEdges = true
      , fontSize = 15.0
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg_graph-dark.dot" (Graphviz.toDotStr graph)

generateTable :: Effect Unit
generateTable = do
  let
    table :: Table
    table = TransitTable.generate bridgesTransit _
      { undirectedEdges = true
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg_table.md" (Table.toMarkdown table)

main :: Effect Unit
main = do
  generateGraphLight
  generateGraphDark
  generateTable
