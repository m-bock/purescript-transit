module Transit.Core where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Show.Generic (genericShow)
import Data.Symbol (reflectSymbol)
import Transit.Reflection (Return_(..))
import Transit.Reflection as R
import Type.Data.List (type (:>), Cons', List', Nil')
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

---

class IsStateSym (sym :: Symbol) (state :: StateGraph)

class IsMsgSym (sym :: Symbol) (state :: StateGraph)

class IsGuardSym (sym :: Symbol) (state :: StateGraph)

--------------------------------------------------------------------------------
--- Reflection instances
--------------------------------------------------------------------------------

instance Reflectable (MkStateGraph Nil') R.StateGraph_ where
  reflectType _ = R.StateGraph []

instance
  ( Reflectable (MkStateGraph transitions) R.StateGraph_
  , Reflectable transition R.Transition_
  ) =>
  Reflectable (MkStateGraph (transition :> transitions)) R.StateGraph_ where

  reflectType _ = R.StateGraph (Array.cons head tail)
    where
    head = reflectType (Proxy @transition)
    (R.StateGraph tail) = reflectType (Proxy @(MkStateGraph transitions))

instance
  ( IsSymbol stateName
  , IsSymbol msgName
  ) =>
  Reflectable (MkTransition stateName msgName Nil') R.Transition_ where
  reflectType _ = R.Transition
    (reflectSymbol (Proxy @stateName))
    (reflectSymbol (Proxy @msgName))
    []

instance
  ( IsSymbol stateName
  , IsSymbol msgName
  , Reflectable (MkTransition stateName msgName returns) R.Transition_
  , Reflectable ret R.Return_
  ) =>
  Reflectable (MkTransition stateName msgName (Cons' ret returns)) R.Transition_ where
  reflectType _ = R.Transition
    (reflectSymbol (Proxy @stateName))
    (reflectSymbol (Proxy @msgName))
    (Array.cons head tail)
    where
    head = reflectType (Proxy @ret)
    R.Transition _ _ tail = reflectType (Proxy @(MkTransition stateName msgName returns))

instance (IsSymbol stateName) => Reflectable (MkReturn stateName) R.Return_ where
  reflectType _ = Return (reflectSymbol (Proxy @stateName))

instance (IsSymbol guardName, IsSymbol stateName) => Reflectable (MkReturnVia guardName stateName) Return_ where
  reflectType _ = ReturnVia (reflectSymbol (Proxy @guardName)) (reflectSymbol (Proxy @stateName))

--------------------------------------------------------------------------------
--- Update implementation types
--------------------------------------------------------------------------------

newtype Match (symState :: Symbol) (symMsg :: Symbol) stateIn msgIn stateOut = Match (stateIn -> msgIn -> stateOut)

derive instance Newtype (Match symState symMsg msgIn stateIn stateOut) _

newtype ReturnStateVia (symGuard :: Symbol) a = ReturnStateVia a

derive instance Newtype (ReturnStateVia symGuard a) _

newtype ReturnState a = ReturnState a

derive instance Newtype (ReturnState a) _