module Transit.Reflection where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Transit.Graph (Graph)
import Transit.Graph as Graph
import Transit.StateGraph (StateGraph)
import Data.Set (Set)
import Data.Set as Set

---

toGraph :: StateGraph_ -> StateGraph
toGraph (StateGraph transitions) = Graph.fromEdges
  $ Set.fromFoldable
  $ Array.concatMap
      ( \(Transition from msg returns) -> map
          ( case _ of
              Return to -> { fromNode: { state: from }, toNode: { state: to }, edge: { msg, guard: Nothing } }
              ReturnVia guard to -> { fromNode: { state: from }, toNode: { state: to }, edge: { msg, guard: Just guard } }
          )
          returns
      )
      transitions

----

type StateName_ = String
type MsgName_ = String
type GuardName_ = String

data Return_
  = Return StateName_
  | ReturnVia GuardName_ StateName_

data StateGraph_ = StateGraph (Array Transition_)

data Transition_ = Transition StateName_ MsgName_ (Array Return_)

instance Show Transition_ where
  show = genericShow

instance Show StateGraph_ where
  show = genericShow

instance Show Return_ where
  show = genericShow

derive instance Eq Transition_
derive instance Eq StateGraph_
derive instance Eq Return_

derive instance Generic Transition_ _
derive instance Generic StateGraph_ _
derive instance Generic Return_ _
