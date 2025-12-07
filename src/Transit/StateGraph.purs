module Transit.StateGraph
  ( Edge
  , Node
  , StateGraph
  , mkStateGraph
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Transit.Core (Return_(..), TransitCore_(..), Match_(..))
import Transit.Data.Graph (Graph)
import Transit.Data.Graph as Graph

type Edge = { msg :: String, guard :: Maybe String }

type Node = { state :: String }

type StateGraph = Graph Edge Node

mkStateGraph :: TransitCore_ -> StateGraph
mkStateGraph (TransitCore transitions) = Graph.fromConnections
  $ Set.fromFoldable
  $ Array.concatMap
      ( \(Match from msg returns) -> map
          ( case _ of
              Return to -> { fromNode: { state: from }, toNode: { state: to }, edge: { msg, guard: Nothing } }
              ReturnVia guard to -> { fromNode: { state: from }, toNode: { state: to }, edge: { msg, guard: Just guard } }
          )
          returns
      )
      transitions
