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
import Transit.Core (GuardName, MsgName, Return(..), StateName, TransitCore(..), Match(..))
import Transit.Data.Graph (Graph)
import Transit.Data.Graph as Graph

type EdgeStateTransition = { msg :: MsgName, guard :: Maybe GuardName }

type StateNode = StateName

type StateGraph = Graph EdgeStateTransition StateNode

mkStateGraph :: TransitCore -> StateGraph
mkStateGraph (TransitCore transitions) =
  Graph.fromEdges
    $ Set.fromFoldable
    $ Array.concatMap
        ( \(Match from msg returns) -> map
            ( case _ of
                Return to ->
                  { fromNode: from
                  , toNode: to
                  , edgeLabel: { msg, guard: Nothing }
                  }
                ReturnVia guard to ->
                  { fromNode: from
                  , toNode: to
                  , edgeLabel: { msg, guard: Just guard }
                  }
            )
            returns
        )
        transitions
