module Transit.Reflection where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe)
import Data.Show.Generic (genericShow)
import Transit.Graph (Graph)
import Unsafe.Coerce (unsafeCoerce)

---

type Edge = { msg :: String, guard :: Maybe String }

type Node = { state :: String }

toGraph :: StateGraph_ -> Graph Edge Node
toGraph = unsafeCoerce "todo"

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
