module Test.Transit.Data.Graph
  ( spec
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set (Set)
import Data.Set as Set
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Data.Graph as Graph

spec :: Spec Unit
spec = do
  describe "Transit.Data.Graph" do
    describe "fromConnections" do
      it "creates empty graph from empty set" do
        let graph = Graph.fromConnections (Set.empty :: Set (Graph.Connection String String))
        Graph.getConnections graph `shouldEqual` (Set.empty :: Set (Graph.Connection String String))

      it "creates graph from single connection" do
        let
          conn = { fromNode: "A", edge: "edge1", toNode: "B" }
          graph = Graph.fromConnections (Set.singleton conn)
        Graph.getConnections graph `shouldEqual` Set.singleton conn

      it "creates graph from multiple connections" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "B", edge: "edge2", toNode: "C" }
            ]
          graph = Graph.fromConnections conns
        Graph.getConnections graph `shouldEqual` conns

    describe "getConnections" do
      it "returns all connections from graph" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "B", edge: "edge2", toNode: "C" }
            ]
          graph = Graph.fromConnections conns
        Graph.getConnections graph `shouldEqual` conns

    describe "getNodes" do
      it "returns empty set for empty graph" do
        let graph = Graph.fromConnections (Set.empty :: Set (Graph.Connection String String))
        Graph.getNodes graph `shouldEqual` (Set.empty :: Set String)

      it "returns all unique nodes from connections" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "B", edge: "edge2", toNode: "C" }
            ]
          graph = Graph.fromConnections conns
        Graph.getNodes graph `shouldEqual` Set.fromFoldable [ "A", "B", "C" ]

      it "deduplicates nodes" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "A", edge: "edge2", toNode: "C" }
            , { fromNode: "B", edge: "edge3", toNode: "A" }
            ]
          graph = Graph.fromConnections conns
        Graph.getNodes graph `shouldEqual` Set.fromFoldable [ "A", "B", "C" ]

    describe "getOutgoingEdges" do
      it "returns empty set for node with no outgoing edges" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            ]
          graph = Graph.fromConnections conns
        Graph.getOutgoingEdges "C" graph `shouldEqual` (Set.empty :: Set (Graph.Connection String String))

      it "returns all outgoing edges from a node" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "A", edge: "edge2", toNode: "C" }
            , { fromNode: "B", edge: "edge3", toNode: "C" }
            ]
          graph = Graph.fromConnections conns
        Graph.getOutgoingEdges "A" graph `shouldEqual` Set.fromFoldable
          [ { fromNode: "A", edge: "edge1", toNode: "B" }
          , { fromNode: "A", edge: "edge2", toNode: "C" }
          ]

    describe "getIncomingEdges" do
      it "returns empty set for node with no incoming edges" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            ]
          graph = Graph.fromConnections conns
        Graph.getIncomingEdges "A" graph `shouldEqual` (Set.empty :: Set (Graph.Connection String String))

      it "returns all incoming edges to a node" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "C", edge: "edge2", toNode: "B" }
            , { fromNode: "B", edge: "edge3", toNode: "C" }
            ]
          graph = Graph.fromConnections conns
        Graph.getIncomingEdges "B" graph `shouldEqual` Set.fromFoldable
          [ { fromNode: "A", edge: "edge1", toNode: "B" }
          , { fromNode: "C", edge: "edge2", toNode: "B" }
          ]

    describe "hasEdge" do
      it "returns false for edge not in graph" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            ]
          graph = Graph.fromConnections conns
          missingEdge = { fromNode: "A", edge: "edge2", toNode: "C" }
        Graph.hasEdge missingEdge graph `shouldEqual` false

      it "returns true for edge in graph" do
        let
          conn = { fromNode: "A", edge: "edge1", toNode: "B" }
          conns = Set.fromFoldable [ conn ]
          graph = Graph.fromConnections conns
        Graph.hasEdge conn graph `shouldEqual` true

    describe "isUndirected" do
      it "returns true for empty graph" do
        let graph = Graph.fromConnections (Set.empty :: Set (Graph.Connection String String))
        Graph.isUndirected graph `shouldEqual` true

      it "returns false for directed graph" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            ]
          graph = Graph.fromConnections conns
        Graph.isUndirected graph `shouldEqual` false

      it "returns true for undirected graph (bidirectional edges)" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "B", edge: "edge1", toNode: "A" }
            ]
          graph = Graph.fromConnections conns
        Graph.isUndirected graph `shouldEqual` true

      it "returns false when edges are not symmetric" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "B", edge: "edge2", toNode: "A" }
            ]
          graph = Graph.fromConnections conns
        Graph.isUndirected graph `shouldEqual` false

    describe "getGrouped" do
      it "returns empty set for empty graph" do
        let graph = Graph.fromConnections (Set.empty :: Set (Graph.Connection String String))
        Graph.getGrouped graph `shouldEqual` (Set.empty :: Set (Graph.NodeInfo String String))

      it "groups connections by fromNode" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "A", edge: "edge2", toNode: "C" }
            , { fromNode: "B", edge: "edge3", toNode: "C" }
            ]
          graph = Graph.fromConnections conns
          grouped = Graph.getGrouped graph
        Set.size grouped `shouldEqual` 2

      it "groups connections by fromNode and then by edge" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "A", edge: "edge1", toNode: "C" }
            , { fromNode: "A", edge: "edge2", toNode: "B" }
            ]
          graph = Graph.fromConnections conns
          grouped = Graph.getGrouped graph
        Set.size grouped `shouldEqual` 1
        let nodeInfo = Set.findMin grouped
        case nodeInfo of
          Just info -> do
            info.fromNode `shouldEqual` "A"
            Set.size info.edges `shouldEqual` 2
          Nothing -> pure unit

      it "handles multiple nodes with same edge to different targets" do
        let
          conns = Set.fromFoldable
            [ { fromNode: "A", edge: "edge1", toNode: "B" }
            , { fromNode: "A", edge: "edge1", toNode: "C" }
            ]
          graph = Graph.fromConnections conns
          grouped = Graph.getGrouped graph
        Set.size grouped `shouldEqual` 1
        let nodeInfo = Set.findMin grouped
        case nodeInfo of
          Just info -> do
            info.fromNode `shouldEqual` "A"
            Set.size info.edges `shouldEqual` 1
            let edgeInfo = Set.findMin info.edges
            case edgeInfo of
              Just ei -> do
                ei.edge `shouldEqual` "edge1"
                ei.toNodes `shouldEqual` Set.fromFoldable [ "B", "C" ]
              Nothing -> pure unit
          Nothing -> pure unit

