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

type BridgesKoenigsbergTransit =
  Transit
    :* ("A" |< "a" >| "B")
    :* ("A" |< "b" >| "B")
    :* ("A" |< "c" >| "C")
    :* ("A" |< "d" >| "C")
    :* ("A" |< "e" >| "D")
    :* ("B" |< "f" >| "D")
    :* ("C" |< "g" >| "D")

update :: State -> Msg -> State
update = mkUpdate @BridgesKoenigsbergTransit
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

bridgesKoenigsbergTransit :: TransitCore
bridgesKoenigsbergTransit = reflectType (Proxy @BridgesKoenigsbergTransit)

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

spec1 :: Spec Unit
spec1 =
  it "should follow the walk and visit the expected intermediate states" do
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

bridgesKoenigsbergGraph :: StateGraph
bridgesKoenigsbergGraph = mkStateGraph bridgesKoenigsbergTransit

spec2 :: Spec Unit
spec2 = do
  it "should each node have the expected degree" do
    nodeDegree bridgesKoenigsbergGraph "A" `shouldEqual` 5
    nodeDegree bridgesKoenigsbergGraph "B" `shouldEqual` 3
    nodeDegree bridgesKoenigsbergGraph "C" `shouldEqual` 3
    nodeDegree bridgesKoenigsbergGraph "D" `shouldEqual` 3

spec3 :: Spec Unit
spec3 = do
  it "should not have an Eulerian trail" do
    hasEulerTrail bridgesKoenigsbergGraph `shouldEqual` false

spec :: Spec Unit
spec = do
  describe "BridgesKoenigsberg" do
    spec1
    spec2
    spec3

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

generateStateDiagramLight :: Effect Unit
generateStateDiagramLight = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate bridgesKoenigsbergTransit _
      { theme = themeHarmonyLight
      , useUndirectedEdges = true
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg-light.dot" (Graphviz.toDotStr graph)

generateStateDiagramDark :: Effect Unit
generateStateDiagramDark = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate bridgesKoenigsbergTransit _
      { theme = themeHarmonyDark
      , useUndirectedEdges = true
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg-dark.dot" (Graphviz.toDotStr graph)

generateTransitionTable :: Effect Unit
generateTransitionTable = do
  let
    table :: Table
    table = TransitTable.generate bridgesKoenigsbergTransit _
      { useUndirectedEdges = true
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg.md" (Table.toMarkdown table)

main :: Effect Unit
main = do
  generateStateDiagramLight
  generateStateDiagramDark
  generateTransitionTable
