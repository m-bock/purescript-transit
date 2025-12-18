module Test.Transit.Data.Graph
  ( spec
  ) where

import Prelude

import Data.Set (Set)
import Data.Set as Set
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Data.Graph as Graph

spec :: Spec Unit
spec = do
  describe "Transit.Data.Graph" do
    describe "fromEdges" do
      it "creates empty graph from empty set" do
        let graph = Graph.fromEdges (Set.empty :: Set (Graph.Edge String String))
        Graph.getEdges graph `shouldEqual` (Set.empty :: Set (Graph.Edge String String))

      it "creates graph from single edge" do
        let
          edge = { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
          graph = Graph.fromEdges (Set.singleton edge)
        Graph.getEdges graph `shouldEqual` Set.singleton edge

      it "creates graph from multiple edges" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            , { fromNode: "B", edgeLabel: "edge2", toNode: "C" }
            ]
          graph = Graph.fromEdges edges
        Graph.getEdges graph `shouldEqual` edges

    describe "getEdges" do
      it "returns all edges from graph" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            , { fromNode: "B", edgeLabel: "edge2", toNode: "C" }
            ]
          graph = Graph.fromEdges edges
        Graph.getEdges graph `shouldEqual` edges

    describe "getNodes" do
      it "returns empty set for empty graph" do
        let graph = Graph.fromEdges (Set.empty :: Set (Graph.Edge String String))
        Graph.getNodes graph `shouldEqual` (Set.empty :: Set String)

      it "returns all unique nodes from edges" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            , { fromNode: "B", edgeLabel: "edge2", toNode: "C" }
            ]
          graph = Graph.fromEdges edges
        Graph.getNodes graph `shouldEqual` Set.fromFoldable [ "A", "B", "C" ]

      it "deduplicates nodes" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            , { fromNode: "A", edgeLabel: "edge2", toNode: "C" }
            , { fromNode: "B", edgeLabel: "edge3", toNode: "A" }
            ]
          graph = Graph.fromEdges edges
        Graph.getNodes graph `shouldEqual` Set.fromFoldable [ "A", "B", "C" ]

    describe "getOutgoingEdges" do
      it "returns empty set for node with no outgoing edges" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            ]
          graph = Graph.fromEdges edges
        Graph.getOutgoingEdges "C" graph `shouldEqual` (Set.empty :: Set (Graph.Edge String String))

      it "returns all outgoing edges from a node" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            , { fromNode: "A", edgeLabel: "edge2", toNode: "C" }
            , { fromNode: "B", edgeLabel: "edge3", toNode: "C" }
            ]
          graph = Graph.fromEdges edges
        Graph.getOutgoingEdges "A" graph `shouldEqual` Set.fromFoldable
          [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
          , { fromNode: "A", edgeLabel: "edge2", toNode: "C" }
          ]

    describe "getIncomingEdges" do
      it "returns empty set for node with no incoming edges" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            ]
          graph = Graph.fromEdges edges
        Graph.getIncomingEdges "A" graph `shouldEqual` (Set.empty :: Set (Graph.Edge String String))

      it "returns all incoming edges to a node" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            , { fromNode: "C", edgeLabel: "edge2", toNode: "B" }
            , { fromNode: "B", edgeLabel: "edge3", toNode: "C" }
            ]
          graph = Graph.fromEdges edges
        Graph.getIncomingEdges "B" graph `shouldEqual` Set.fromFoldable
          [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
          , { fromNode: "C", edgeLabel: "edge2", toNode: "B" }
          ]

    describe "hasEdge" do
      it "returns false for edge not in graph" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            ]
          graph = Graph.fromEdges edges
          missingEdge = { fromNode: "A", edgeLabel: "edge2", toNode: "C" }
        Graph.hasEdge missingEdge graph `shouldEqual` false

      it "returns true for edge in graph" do
        let
          edge = { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
          edges = Set.fromFoldable [ edge ]
          graph = Graph.fromEdges edges
        Graph.hasEdge edge graph `shouldEqual` true

    describe "isUndirected" do
      it "returns true for empty graph" do
        let graph = Graph.fromEdges (Set.empty :: Set (Graph.Edge String String))
        Graph.isUndirected graph `shouldEqual` true

      it "returns false for directed graph" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            ]
          graph = Graph.fromEdges edges
        Graph.isUndirected graph `shouldEqual` false

      it "returns true for undirected graph (bidirectional edges)" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            , { fromNode: "B", edgeLabel: "edge1", toNode: "A" }
            ]
          graph = Graph.fromEdges edges
        Graph.isUndirected graph `shouldEqual` true

      it "returns false when edges are not symmetric" do
        let
          edges = Set.fromFoldable
            [ { fromNode: "A", edgeLabel: "edge1", toNode: "B" }
            , { fromNode: "B", edgeLabel: "edge2", toNode: "A" }
            ]
          graph = Graph.fromEdges edges
        Graph.isUndirected graph `shouldEqual` false
