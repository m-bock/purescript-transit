module Transit.Core
  ( GuardName
  , Match(..)
  , MatchImpl(..)
  , MatchTL
  , MkMatchTL
  , MkReturnTL
  , MkReturnViaTL
  , MkTransitCoreTL
  , MsgName
  , Return(..)
  , ReturnState(..)
  , ReturnStateVia(..)
  , ReturnTL
  , StateName
  , TransitCore(..)
  , TransitCoreTL
  , class IsGuardSym
  , class IsMsgSym
  , class IsStateSym
  , class IsTransitSpec
  , getMatchesForState
  , getStateNames
  ) where

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

foreign import data TransitCoreTL :: Type
foreign import data MkTransitCoreTL :: List' MatchTL -> TransitCoreTL

foreign import data MatchTL :: Type

foreign import data MkMatchTL :: StateName -> MsgName -> List' ReturnTL -> MatchTL

foreign import data ReturnTL :: Type
foreign import data MkReturnTL :: StateName -> ReturnTL
foreign import data MkReturnViaTL :: GuardName -> StateName -> ReturnTL

--------------------------------------------------------------------------------
--- Classes
--------------------------------------------------------------------------------

class IsStateSym (sym :: Symbol) (state :: TransitCoreTL)

class IsMsgSym (sym :: Symbol) (state :: TransitCoreTL)

class IsGuardSym (sym :: Symbol) (state :: TransitCoreTL)

--------------------------------------------------------------------------------
--- Reflection types
--------------------------------------------------------------------------------

data Return
  = Return String
  | ReturnVia String String

data TransitCore = TransitCore (Array Match)

data Match = Match String String (Array Return)

instance Show Match where
  show = genericShow

instance Show TransitCore where
  show = genericShow

instance Show Return where
  show = genericShow

derive instance Eq Match
derive instance Eq TransitCore
derive instance Eq Return

derive instance Generic Match _
derive instance Generic TransitCore _
derive instance Generic Return _

--------------------------------------------------------------------------------
--- Reflection instances
--------------------------------------------------------------------------------

instance Reflectable (MkTransitCoreTL Nil') TransitCore where
  reflectType _ = TransitCore []

instance
  ( Reflectable (MkTransitCoreTL transitions) TransitCore
  , Reflectable transition Match
  ) =>
  Reflectable (MkTransitCoreTL (transition :> transitions)) TransitCore where

  reflectType _ = TransitCore (Array.cons head tail)
    where
    head = reflectType (Proxy @transition)
    (TransitCore tail) = reflectType (Proxy @(MkTransitCoreTL transitions))

instance
  ( IsSymbol stateName
  , IsSymbol msgName
  ) =>
  Reflectable (MkMatchTL stateName msgName Nil') Match where
  reflectType _ = Match
    (reflectSymbol (Proxy @stateName))
    (reflectSymbol (Proxy @msgName))
    []

instance
  ( IsSymbol stateName
  , IsSymbol msgName
  , Reflectable (MkMatchTL stateName msgName returns) Match
  , Reflectable ret Return
  ) =>
  Reflectable (MkMatchTL stateName msgName (Cons' ret returns)) Match where
  reflectType _ = Match
    (reflectSymbol (Proxy @stateName))
    (reflectSymbol (Proxy @msgName))
    (Array.cons head tail)
    where
    head = reflectType (Proxy @ret)
    Match _ _ tail = reflectType (Proxy @(MkMatchTL stateName msgName returns))

instance (IsSymbol stateName) => Reflectable (MkReturnTL stateName) Return where
  reflectType _ = Return (reflectSymbol (Proxy @stateName))

instance (IsSymbol guardName, IsSymbol stateName) => Reflectable (MkReturnViaTL guardName stateName) Return where
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

---

class IsTransitSpec :: forall spec. spec -> TransitCoreTL -> Constraint
class IsTransitSpec spec core | spec -> core

instance IsTransitSpec (MkTransitCoreTL xs) (MkTransitCoreTL xs)

---

getStateNames :: TransitCore -> Array String
getStateNames (TransitCore matches) = Array.nub $ Array.concatMap
  ( \(Match from _ returns) -> [ from ] <> map
      ( case _ of
          Return to -> to
          ReturnVia _ to -> to
      )
      returns
  )
  matches

getMatchesForState :: String -> TransitCore -> Array Match
getMatchesForState stateName (TransitCore matches) = Array.filter (\(Match from _ _) -> from == stateName) matches