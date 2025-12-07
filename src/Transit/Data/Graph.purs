module Transit.Data.Graph
  ( Connection
  , Graph
  , NodeInfo
  , EdgeInfo
  , fromConnections
  , getGrouped
  , getIncomingEdges
  , getNodes
  , getOutgoingEdges
  , hasEdge
  , getConnections
  , isUndirected
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Set (Set)
import Data.Set as Set

type Connection e n = { fromNode :: n, edge :: e, toNode :: n }

type EdgeInfo e n = { edge :: e, toNodes :: Set n }

type NodeInfo e n = { fromNode :: n, edges :: Set (EdgeInfo e n) }

hasEdge :: forall e n. Ord e => Ord n => Connection e n -> Graph e n -> Boolean
hasEdge e (Graph xs) = Set.member e xs

getGrouped :: forall e n. Ord n => Ord e => Graph e n -> Set (NodeInfo e n)
getGrouped (Graph xs) = Set.fromFoldable $ map f $ Array.groupAllBy (\a b -> a.fromNode `compare` b.fromNode) $ Set.toUnfoldable xs
  where
  f :: NonEmptyArray (Connection e n) -> NodeInfo e n
  f conns = { fromNode: (NEA.head conns).fromNode, edges: Set.fromFoldable $ map g $ Array.groupAllBy (\a b -> a.edge `compare` b.edge) $ NEA.toArray conns }

  g :: NonEmptyArray (Connection e n) -> EdgeInfo e n
  g conns = { edge: (NEA.head conns).edge, toNodes: Set.fromFoldable $ map (\{ toNode } -> toNode) $ NEA.toArray conns }

newtype Graph e n = Graph (Set (Connection e n)) -- directed, cyclic, multiedge

instance (Show e, Show n) => Show (Graph e n) where
  show (Graph xs) = show xs

fromConnections :: forall e n. Set (Connection e n) -> Graph e n
fromConnections edges = Graph edges

getConnections :: forall e n. Graph e n -> Set (Connection e n)
getConnections (Graph xs) = xs

getNodes :: forall e n. Ord n => Graph e n -> Set n
getNodes (Graph xs) = Set.fromFoldable $ Array.concatMap (\{ fromNode, toNode } -> [ fromNode, toNode ]) (Set.toUnfoldable xs)

getOutgoingEdges :: forall e n. Ord e => Ord n => n -> Graph e n -> Set (Connection e n)
getOutgoingEdges node (Graph xs) = Set.filter (\{ fromNode } -> fromNode == node) xs

getIncomingEdges :: forall e n. Ord e => Ord n => n -> Graph e n -> Set (Connection e n)
getIncomingEdges node (Graph xs) = Set.filter (\{ toNode } -> toNode == node) xs

isUndirected :: forall e n. Ord n => Ord e => Graph e n -> Boolean
isUndirected g =
  let
    connections = getConnections g
  in
    Array.all (\conn -> hasEdge { fromNode: conn.toNode, edge: conn.edge, toNode: conn.fromNode } g) (Set.toUnfoldable connections)

