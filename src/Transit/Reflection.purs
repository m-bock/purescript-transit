module Transit.Reflection where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

type StateName_ = String
type MsgName_ = String
type GuardName_ = String

data Return_ = Return (Maybe GuardName_) StateName_

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

getStates :: StateGraph_ -> Array StateName_
getStates (StateGraph transitions) = Array.nub $ Array.concat [ fromStates, toStates ]
  where
  fromStates = map (\(Transition stateName _ _) -> stateName) transitions
  toStates = Array.concatMap (\(Transition _ _ returns) -> map (\(Return _ stateName) -> stateName) returns) transitions

getOutgoing :: StateName_ -> StateGraph_ -> Array Transition_
getOutgoing stateName (StateGraph transitions) =
  Array.filter (\(Transition from _ _) -> from == stateName) transitions

getIncoming :: StateName_ -> StateGraph_ -> Array Transition_
getIncoming stateName (StateGraph transitions) =
  Array.filter (\(Transition _ _ returns) -> Array.any (\(Return _ to) -> to == stateName) returns) transitions

