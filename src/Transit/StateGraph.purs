module Transit.StateGraph
  ( Edge
  , Node
  , StateGraph
  , mkStateGraph
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (mapWithIndex)
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Transit.Core (Return_(..), TransitCore_(..), Match_(..))
import Transit.Data.Graph (Graph, mapGraph)
import Transit.Data.Graph as Graph

type Edge = { msg :: String, guard :: Maybe String }

type Node = String

type StateGraph = Graph Edge Node

mkStateGraph :: TransitCore_ -> StateGraph
mkStateGraph (TransitCore transitions) =
  Graph.fromConnections
    $ Set.fromFoldable
    $ Array.concatMap
        ( \(Match from msg returns) -> map
            ( case _ of
                Return to ->
                  { fromNode: from
                  , toNode: to
                  , edge: { msg, guard: Nothing }
                  }
                ReturnVia guard to ->
                  { fromNode: from
                  , toNode: to
                  , edge: { msg, guard: Just guard }
                  }
            )
            returns
        )
        transitions
  where
  initState = Array.head transitions # map (\(Match from _ _) -> from)
