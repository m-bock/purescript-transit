module Transit.Reflection where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Show.Generic (genericShow)

type StateName_ = String
type MsgName_ = String
type GuardName_ = String

data Return_
  = Return StateName_
  | ReturnVia GuardName_ StateName_

newtype StateGraph_ = StateGraph (Array Transition_)

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
