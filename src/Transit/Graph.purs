module Transit.Graph
  ( Edge
  , Graph
  , Grouped
  , fromEdges
  , getGrouped
  , getIncomingEdges
  , getNodes
  , getOutgoingEdges
  , hasEdge
  , hasEulerCircle
  , hasEulerTrail
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Int as Int
import Data.Set (Set)
import Data.Set as Set

type Edge e n = { fromNode :: n, edge :: e, toNode :: n }

type Grouped e n = Array { fromNode :: n, edges :: Array { edge :: e, toNodes :: Array n } }

hasEdge :: forall e n. Ord e => Ord n => Edge e n -> Graph e n -> Boolean
hasEdge e (Graph xs) = Set.member e xs

getGrouped :: forall e n. Eq n => Eq e => Graph e n -> Grouped e n
getGrouped (Graph xs) = map f $ Array.groupBy (\a b -> a.fromNode == b.fromNode) $ Set.toUnfoldable xs
  where
  f :: NonEmptyArray (Edge e n) -> { fromNode :: n, edges :: Array { edge :: e, toNodes :: Array n } }
  f xs = { fromNode: (NEA.head xs).fromNode, edges: map g $ Array.groupBy (\a b -> a.edge == b.edge) $ NEA.toArray xs }

  g :: NonEmptyArray (Edge e n) -> { edge :: e, toNodes :: Array n }
  g xs = { edge: (NEA.head xs).edge, toNodes: map (\{ toNode } -> toNode) $ NEA.toArray xs }

newtype Graph e n = Graph (Set (Edge e n)) -- directed, cyclic, multiedge

fromEdges :: forall e n. Set (Edge e n) -> Graph e n
fromEdges edges = Graph edges

getNodes :: forall e n. Ord n => Graph e n -> Set n
getNodes (Graph xs) = Set.fromFoldable $ Array.concatMap (\{ fromNode, toNode } -> [ fromNode, toNode ]) (Set.toUnfoldable xs)

getOutgoingEdges :: forall e n. Ord e => Ord n => n -> Graph e n -> Set (Edge e n)
getOutgoingEdges node (Graph xs) = Set.filter (\{ fromNode } -> fromNode == node) xs

getIncomingEdges :: forall e n. Ord e => Ord n => n -> Graph e n -> Set (Edge e n)
getIncomingEdges node (Graph xs) = Set.filter (\{ toNode } -> toNode == node) xs

isUndirected :: forall e n. Ord n => Ord e => Graph e n -> Boolean
isUndirected g =
  let
    nodes = getNodes g
  in
    Array.all (\node -> (getOutgoingEdges node g) == (getIncomingEdges node g)) (Set.toUnfoldable nodes)

hasEulerCircle :: forall e n. Ord n => Ord e => Graph e n -> Boolean
hasEulerCircle g = isUndirected g && countOddOutgoingEdges g == 0

hasEulerTrail :: forall e n. Ord n => Ord e => Graph e n -> Boolean
hasEulerTrail g = isUndirected g && (countOddOutgoingEdges g == 0 || countOddOutgoingEdges g == 2)

countOddOutgoingEdges :: forall e n. Ord n => Ord e => Graph e n -> Int
countOddOutgoingEdges g =
  let
    nodes = getNodes g
  in
    Array.length $ Array.filter (\node -> Int.odd $ Set.size (getOutgoingEdges node g)) (Set.toUnfoldable nodes)