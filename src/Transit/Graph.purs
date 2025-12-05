module Transit.Graph
  ( Graph
  , getNodes
  , getOutgoingEdges
  , getIncomingEdges
  , Edge
  ) where

import Prelude

import Data.Array as Array

type Edge e n = { fromNode :: n, toNode :: n, edge :: e }

newtype Graph e n = Graph (Array (Edge e n))

getNodes :: forall e n. Ord n => Graph e n -> Array n
getNodes (Graph xs) = Array.nub $ Array.concatMap (\{ fromNode, toNode } -> [ fromNode, toNode ]) xs

getOutgoingEdges :: forall e n. Eq n => n -> Graph e n -> Array (Edge e n)
getOutgoingEdges node (Graph xs) = Array.filter (\{ fromNode } -> fromNode == node) xs

getIncomingEdges :: forall e n. Eq n => n -> Graph e n -> Array (Edge e n)
getIncomingEdges node (Graph xs) = Array.filter (\{ toNode } -> toNode == node) xs