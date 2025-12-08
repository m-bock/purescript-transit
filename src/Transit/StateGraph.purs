module Transit.StateGraph
  ( Edge
  , Node
  , StateGraph(..)
  , mkStateGraph
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Data.Show.Generic (genericShow)
import Transit.Core (Return_(..), TransitCore_(..), Match_(..))
import Transit.Data.Graph (Graph, mapGraph)
import Transit.Data.Graph as Graph

type Edge = { msg :: String, guard :: Maybe String }

type Node = String

data StateGraph = StateGraph { entryPoints :: Array Node } (Graph Edge Node)

derive instance Generic StateGraph _

instance Show StateGraph where
  show = genericShow

mkStateGraph :: TransitCore_ -> StateGraph
mkStateGraph (TransitCore transitions) =
  let
    entryPoints = fromMaybe [] $ map (\(Match from _ _) -> [ from ]) $ Array.head transitions
  in
    StateGraph { entryPoints }
      $ Graph.fromConnections
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
