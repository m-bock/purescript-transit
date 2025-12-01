module Transit.Core where

import Prelude

import Data.Newtype (class Newtype)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Symbol (reflectSymbol)
import Type.Data.List (type (:>), List', Nil')
import Type.Prelude (class IsSymbol, Proxy(..))

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

type StateName = Symbol
type MsgName = Symbol
type GuardName = Symbol

foreign import data StateGraph :: Type
foreign import data MkStateGraph :: List' Transition -> StateGraph

foreign import data Transition :: Type
foreign import data MkTransition :: StateName -> MsgName -> List' Return -> Transition

foreign import data Return :: Type
foreign import data MkReturn :: StateName -> Return
foreign import data MkReturnVia :: GuardName -> StateName -> Return

--------------------------------------------------------------------------------
--- Data types
--------------------------------------------------------------------------------

type StateName_ = String
type MsgName_ = String
type GuardName_ = String

newtype StateGraph_ = StateGraph (Array Transition_)

data Transition_ = Transition StateName_ MsgName_ (Array Return_)

data Return_
  = Return StateName_
  | ReturnVia GuardName_ StateName_

--------------------------------------------------------------------------------
--- Reflection instances
--------------------------------------------------------------------------------

instance Reflectable (MkStateGraph Nil') StateGraph_ where
  reflectType _ = StateGraph []

instance (Reflectable (MkStateGraph transitions) StateGraph_) => Reflectable (MkStateGraph (transition :> transitions)) StateGraph_ where

  reflectType _ = reflectType (Proxy @(MkStateGraph transitions))

instance
  ( IsSymbol stateName
  , IsSymbol msgName
  , Reflectable returns (Array Return_)
  ) =>
  Reflectable (MkTransition stateName msgName returns) Transition_ where
  reflectType _ = Transition (reflectSymbol (Proxy @stateName)) (reflectSymbol (Proxy @msgName)) (reflectType (Proxy @returns))

instance (IsSymbol stateName) => Reflectable (MkReturn stateName) Return_ where
  reflectType _ = Return (reflectSymbol (Proxy @stateName))

instance (IsSymbol guardName, IsSymbol stateName) => Reflectable (MkReturnVia guardName stateName) Return_ where
  reflectType _ = ReturnVia (reflectSymbol (Proxy @guardName)) (reflectSymbol (Proxy @stateName))

--------------------------------------------------------------------------------
--- Update implementation types
--------------------------------------------------------------------------------

newtype Match (symState :: Symbol) (symMsg :: Symbol) msgIn stateIn stateOut = Match (msgIn -> stateIn -> stateOut)

derive instance Newtype (Match symState symMsg msgIn stateIn stateOut) _

newtype ReturnStateVia (symGuard :: Symbol) a = ReturnStateVia a

derive instance Newtype (ReturnStateVia symGuard a) _

newtype ReturnState a = ReturnState a

derive instance Newtype (ReturnState a) _