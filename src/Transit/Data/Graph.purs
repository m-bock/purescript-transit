module Transit.Data.Graph
  ( Connection
  , Graph
  , fromConnections
  , getIncomingEdges
  , getNodes
  , getOutgoingEdges
  , hasEdge
  , getConnections
  , mapGraph
  , isUndirected
  ) where

import Prelude

import Data.Array as Array
import Data.Set (Set)
import Data.Set as Set

type Connection e n = { fromNode :: n, edge :: e, toNode :: n }

hasEdge :: forall e n. Ord e => Ord n => Connection e n -> Graph e n -> Boolean
hasEdge e (Graph xs) = Set.member e xs

newtype Graph e n = Graph (Set (Connection e n)) -- directed, cyclic, multiedge

instance (Show e, Show n) => Show (Graph e n) where
  show (Graph xs) = show xs

mapGraph :: forall e n e' n'. Ord e' => Ord n' => (e -> e') -> (n -> n') -> Graph e n -> Graph e' n'
mapGraph f g (Graph xs) = Graph $ Set.map (\conn -> { fromNode: g conn.fromNode, edge: f conn.edge, toNode: g conn.toNode }) xs

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

