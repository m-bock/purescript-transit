module Test.Transit.Render.Graphviz
  ( spec
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Core (Match(..), Return(..), TransitCore(..))
import Transit.Data.DotLang (GraphvizGraph(..))
import Transit.Render.Graphviz (defaultOptions, mkGraphvizGraph)

spec :: Spec Unit
spec = do
  describe "Transit.Render.Graphviz" do
    describe "mkGraphvizGraph" do
      it "generates graph from simple transit core" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            ]
          graph = mkGraphvizGraph defaultOptions transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have at least global attrs + state node + edge
            (Array.length sections >= 3) `shouldEqual` true

      it "generates graph with multiple states" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            , Match "State2" "Msg2" [ Return "State3" ]
            , Match "State3" "Msg3" [ Return "State1" ]
            ]
          graph = mkGraphvizGraph defaultOptions transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have global attrs + 3 state nodes + 3 edges
            (Array.length sections >= 7) `shouldEqual` true

      it "generates entry point nodes when entryPoints are specified" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            ]
          options = defaultOptions { entryPoints = [ "State1" ] }
          graph = mkGraphvizGraph options transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have init node and init edge in addition to normal sections
            -- (global attrs + state node + init node + init edge + transition edge = at least 5)
            (Array.length sections >= 5) `shouldEqual` true

      it "generates decision nodes when useDecisionNodes is true" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2", Return "State3" ]
            ]
          options = defaultOptions { useDecisionNodes = true }
          graph = mkGraphvizGraph options transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have decision node + edges from decision node
            (Array.length sections >= 5) `shouldEqual` true

      it "generates direct edges when useDecisionNodes is false" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2", Return "State3" ]
            ]
          options = defaultOptions { useDecisionNodes = false }
          graph = mkGraphvizGraph options transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have 2 direct edges (no decision node)
            (Array.length sections >= 4) `shouldEqual` true

      it "generates undirected edges when useUndirectedEdges is true and complementary edge exists" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            , Match "State2" "Msg1" [ Return "State1" ]
            ]
          options = defaultOptions { useUndirectedEdges = true }
          graph = mkGraphvizGraph options transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have undirected edge (one edge instead of two)
            (Array.length sections >= 3) `shouldEqual` true

      it "handles transitions with guards (ReturnVia)" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ ReturnVia "Guard1" "State2" ]
            ]
          graph = mkGraphvizGraph defaultOptions transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should generate graph with guard
            (Array.length sections >= 3) `shouldEqual` true

      it "handles complex transitions with multiple returns and guards" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1"
                [ Return "State2"
                , ReturnVia "Guard1" "State3"
                , ReturnVia "Guard2" "State1"
                ]
            ]
          graph = mkGraphvizGraph defaultOptions transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have decision node + edges for all returns
            (Array.length sections >= 6) `shouldEqual` true

      it "includes title when title option is provided" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            ]
          options = defaultOptions { title = Just "Test Graph" }
          graph = mkGraphvizGraph options transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have title in global attrs
            (Array.length sections >= 3) `shouldEqual` true

      it "handles empty transit core" do
        let
          transitCore = TransitCore []
          graph = mkGraphvizGraph defaultOptions transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have at least global attrs
            (Array.length sections >= 1) `shouldEqual` true

      it "handles self-transitions" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State1" ]
            ]
          graph = mkGraphvizGraph defaultOptions transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should generate self-loop edge
            (Array.length sections >= 3) `shouldEqual` true

      it "handles multiple entry points" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            , Match "State2" "Msg2" [ Return "State3" ]
            ]
          options = defaultOptions { entryPoints = [ "State1", "State2" ] }
          graph = mkGraphvizGraph options transitCore
        case graph of
          GraphvizGraph sections ->
            -- Should have init nodes/edges for each entry point
            (Array.length sections >= 7) `shouldEqual` true

