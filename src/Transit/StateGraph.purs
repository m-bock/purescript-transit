module Transit.StateGraph
  ( EdgeStateTransition
  , StateNode
  , StateGraph
  , mkStateGraph
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Transit.Core (Return(..), TransitCore(..), Match(..))
import Transit.Data.Graph (Graph)
import Transit.Data.Graph as Graph

type EdgeStateTransition = { msg :: String, guard :: Maybe String }

type StateNode = String

type StateGraph = Graph EdgeStateTransition StateNode

mkStateGraph :: TransitCore -> StateGraph
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
