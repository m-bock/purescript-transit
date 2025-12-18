-- | General purpose graph data structure for representing directed graphs with labeled edges.
-- |
-- | This module provides a simple graph representation where:
-- | - Edges are labeled (each edge has an associated label value)
-- | - Nodes are values themselves
-- | - Supports directed edges, cycles, and multiple edges between nodes
module Transit.Data.Graph
  ( Edge
  , Graph
  , fromEdges
  , getIncomingEdges
  , getNodes
  , getOutgoingEdges
  , hasEdge
  , getEdges
  , mapGraph
  , isUndirected
  ) where

import Prelude

import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set

-- | An edge in the graph connecting two nodes with a label.
-- |
-- | - `edgeLabel`: The type of edge labels
-- | - `node`: The type of node values
type Edge edgeLabel node =
  { fromNode :: node
  , edgeLabel :: edgeLabel
  , toNode :: node
  }

-- | A directed graph that supports cycles and multiple edges.
-- |
-- | Supports:
-- | - Directed edges (fromNode -> toNode)
-- | - Cycles (paths that return to a node, possibly through intermediate nodes)
-- | - Multiple edges (same nodes can be connected by different edges)
newtype Graph edgeLabel node = Graph (Set (Edge edgeLabel node))

instance (Show edgeLabel, Show node) => Show (Graph edgeLabel node) where
  show (Graph xs) = show xs

-- | Creates a graph from a set of edges.
fromEdges :: forall edgeLabel node. Set (Edge edgeLabel node) -> Graph edgeLabel node
fromEdges edges = Graph edges

-- | Extracts all edges from a graph.
getEdges :: forall edgeLabel node. Graph edgeLabel node -> Set (Edge edgeLabel node)
getEdges (Graph edges) = edges

-- | Checks if an edge exists in the graph.
hasEdge :: forall edgeLabel node. Ord edgeLabel => Ord node => Edge edgeLabel node -> Graph edgeLabel node -> Boolean
hasEdge edge (Graph edges) = Set.member edge edges

-- | Extracts all unique nodes from the graph.
getNodes :: forall edgeLabel node. Ord node => Graph edgeLabel node -> Set node
getNodes (Graph edges) =
  Set.fromFoldable $
    Array.concatMap (\{ fromNode, toNode } -> [ fromNode, toNode ])
      (Set.toUnfoldable edges)

-- | Gets all outgoing edges from a given node.
getOutgoingEdges :: forall edgeLabel node. Ord edgeLabel => Ord node => node -> Graph edgeLabel node -> Set (Edge edgeLabel node)
getOutgoingEdges node (Graph edges) =
  Set.filter (\{ fromNode } -> fromNode == node) edges

-- | Gets all incoming edges to a given node.
getIncomingEdges :: forall edgeLabel node. Ord edgeLabel => Ord node => node -> Graph edgeLabel node -> Set (Edge edgeLabel node)
getIncomingEdges node (Graph edges) =
  Set.filter (\{ toNode } -> toNode == node) edges

-- | Maps over both edge label and node types in the graph.
mapGraph
  :: forall edgeLabel node edgeLabel' node'
   . Ord edgeLabel'
  => Ord node'
  => (edgeLabel -> edgeLabel')
  -> (node -> node')
  -> Graph edgeLabel node
  -> Graph edgeLabel' node'
mapGraph mapEdgeLabel mapNode (Graph edges) =
  Graph $
    Set.map
      ( \edge ->
          { fromNode: mapNode edge.fromNode
          , edgeLabel: mapEdgeLabel edge.edgeLabel
          , toNode: mapNode edge.toNode
          }
      )
      edges

-- | Checks if the graph is undirected (all edges have symmetric counterparts).
-- |
-- | A graph is considered undirected if for every edge from A to B,
-- | there exists an edge from B to A with the same edge label.
isUndirected :: forall edgeLabel node. Ord node => Ord edgeLabel => Graph edgeLabel node -> Boolean
isUndirected graph =
  let
    edges = getEdges graph
  in
    Array.all
      ( \edge ->
          hasEdge
            { fromNode: edge.toNode
            , edgeLabel: edge.edgeLabel
            , toNode: edge.fromNode
            }
            graph
      )
      (Set.toUnfoldable edges)

