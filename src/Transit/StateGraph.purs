-- | Conversion of transit specifications to graph data structures.
-- |
-- | This module provides functions to convert transit cores into graph
-- | representations, enabling graph-based analysis of state machines
-- | (e.g., finding paths, cycles, etc.).
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

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

-- | Edge label type for state transitions in the graph.
-- |
-- | Contains the message that triggers the transition and an optional
-- | guard condition name.
type EdgeStateTransition = { msg :: MsgName, guard :: Maybe GuardName }

-- | Node type for states in the graph.
type StateNode = StateName

-- | Graph representation of a state machine.
-- |
-- | Nodes represent states, and edges represent transitions with their
-- | associated messages and optional guard conditions.
type StateGraph = Graph EdgeStateTransition StateNode

--------------------------------------------------------------------------------
--- Functions
--------------------------------------------------------------------------------

-- | Converts a transit core specification into a graph representation.
-- |
-- | Each match in the transit core becomes one or more edges in the graph,
-- | with each return state creating a separate edge. Guard conditions are
-- | preserved in the edge labels.
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
