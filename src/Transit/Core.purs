module Transit.Core where

import Prelude

import Data.Array as Array
import Data.Generic.Rep (class Generic)
import Data.Newtype (class Newtype)
import Data.Reflectable (class Reflectable, reflectType)
import Data.Show.Generic (genericShow)
import Data.Symbol (reflectSymbol)
import Type.Data.List (type (:>), Cons', List', Nil')
import Type.Prelude (class IsSymbol, Proxy(..))

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

type StateName = Symbol
type MsgName = Symbol
type GuardName = Symbol

foreign import data TransitCore :: Type
foreign import data MkTransitCore :: List' Match -> TransitCore

foreign import data Match :: Type
foreign import data MkMatch :: StateName -> MsgName -> List' Return -> Match

foreign import data Return :: Type
foreign import data MkReturn :: StateName -> Return
foreign import data MkReturnVia :: GuardName -> StateName -> Return

--------------------------------------------------------------------------------
--- Classes
--------------------------------------------------------------------------------

class IsStateSym (sym :: Symbol) (state :: TransitCore)

class IsMsgSym (sym :: Symbol) (state :: TransitCore)

class IsGuardSym (sym :: Symbol) (state :: TransitCore)

--------------------------------------------------------------------------------
--- Reflection types
--------------------------------------------------------------------------------

type StateName_ = String
type MsgName_ = String
type GuardName_ = String

data Return_
  = Return StateName_
  | ReturnVia GuardName_ StateName_

data TransitCore_ = TransitCore (Array Match_)

data Match_ = Match StateName_ MsgName_ (Array Return_)

instance Show Match_ where
  show = genericShow

instance Show TransitCore_ where
  show = genericShow

instance Show Return_ where
  show = genericShow

derive instance Eq Match_
derive instance Eq TransitCore_
derive instance Eq Return_

derive instance Generic Match_ _
derive instance Generic TransitCore_ _
derive instance Generic Return_ _

--------------------------------------------------------------------------------
--- Reflection instances
--------------------------------------------------------------------------------

instance Reflectable (MkTransitCore Nil') TransitCore_ where
  reflectType _ = TransitCore []

instance
  ( Reflectable (MkTransitCore transitions) TransitCore_
  , Reflectable transition Match_
  ) =>
  Reflectable (MkTransitCore (transition :> transitions)) TransitCore_ where

  reflectType _ = TransitCore (Array.cons head tail)
    where
    head = reflectType (Proxy @transition)
    (TransitCore tail) = reflectType (Proxy @(MkTransitCore transitions))

instance
  ( IsSymbol stateName
  , IsSymbol msgName
  ) =>
  Reflectable (MkMatch stateName msgName Nil') Match_ where
  reflectType _ = Match
    (reflectSymbol (Proxy @stateName))
    (reflectSymbol (Proxy @msgName))
    []

instance
  ( IsSymbol stateName
  , IsSymbol msgName
  , Reflectable (MkMatch stateName msgName returns) Match_
  , Reflectable ret Return_
  ) =>
  Reflectable (MkMatch stateName msgName (Cons' ret returns)) Match_ where
  reflectType _ = Match
    (reflectSymbol (Proxy @stateName))
    (reflectSymbol (Proxy @msgName))
    (Array.cons head tail)
    where
    head = reflectType (Proxy @ret)
    Match _ _ tail = reflectType (Proxy @(MkMatch stateName msgName returns))

instance (IsSymbol stateName) => Reflectable (MkReturn stateName) Return_ where
  reflectType _ = Return (reflectSymbol (Proxy @stateName))

instance (IsSymbol guardName, IsSymbol stateName) => Reflectable (MkReturnVia guardName stateName) Return_ where
  reflectType _ = ReturnVia (reflectSymbol (Proxy @guardName)) (reflectSymbol (Proxy @stateName))

--------------------------------------------------------------------------------
--- Update implementation types
--------------------------------------------------------------------------------

newtype MatchImpl (symState :: Symbol) (symMsg :: Symbol) stateIn msgIn stateOut = MatchImpl (stateIn -> msgIn -> stateOut)

derive instance Newtype (MatchImpl symState symMsg msgIn stateIn stateOut) _

newtype ReturnStateVia (symGuard :: Symbol) a = ReturnStateVia a

derive instance Newtype (ReturnStateVia symGuard a) _

newtype ReturnState a = ReturnState a

derive instance Newtype (ReturnState a) _