-- | Core types and functions for transit specifications.
-- |
-- | This module provides the foundational types for representing state machine
-- | specifications at both the type level and value level. It includes:
-- |
-- | - Type-level constructors for building transit specifications
-- | - Runtime reflection of type-level specifications to values
-- | - Core data types for matches, returns, and transit cores
-- | - Utility functions for querying transit specifications
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
  , Ret(..)
  , RetVia(..)
  , ReturnTL
  , StateName
  , TransitCore(..)
  , TransitCoreTL
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
--- Type-level types
--------------------------------------------------------------------------------

-- | Type alias for state name symbols (type-level).
type StateNameTL = Symbol

-- | Type alias for message name symbols (type-level).
type MsgNameTL = Symbol

-- | Type alias for guard name symbols (type-level).
type GuardNameTL = Symbol

-- | Type-level representation of a transit core specification.
foreign import data TransitCoreTL :: Type

-- | Constructs a type-level transit core from a list of matches.
foreign import data MkTransitCoreTL :: List' MatchTL -> TransitCoreTL

-- | Type-level representation of a match (state, message, returns).
foreign import data MatchTL :: Type

-- | Constructs a type-level match from a state name, message name, and list of returns.
foreign import data MkMatchTL :: StateNameTL -> MsgNameTL -> List' ReturnTL -> MatchTL

-- | Type-level representation of a return specification.
foreign import data ReturnTL :: Type

-- | Constructs a type-level return to a state (no guard).
foreign import data MkReturnTL :: StateNameTL -> ReturnTL

-- | Constructs a type-level return via a guard to a state.
foreign import data MkReturnViaTL :: GuardNameTL -> StateNameTL -> ReturnTL

--------------------------------------------------------------------------------
--- Runtime types
--------------------------------------------------------------------------------

-- | Type alias for state names (runtime).
type StateName = String

-- | Type alias for message names (runtime).
type MsgName = String

-- | Type alias for guard names (runtime).
type GuardName = String

-- | A return specification indicating the next state after a transition.
-- |
-- | - `Return stateName`: Direct transition to a state
-- | - `ReturnVia guardName stateName`: Conditional transition via a guard
data Return
  = Return StateName
  | ReturnVia GuardName StateName

-- | A transit core containing all matches (transitions) in a state machine.
newtype TransitCore = TransitCore (Array Match)

-- | A match represents a transition from a state triggered by a message.
-- |
-- | - First `StateName`: Source state name
-- | - Second `MsgName`: Message name
-- | - `Array Return`: Possible return states (may include guards)
data Match = Match StateName MsgName (Array Return)

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

-- | Type for match implementations in the update function builder.
newtype MatchImpl (symStateIn :: Symbol) (symMsgIn :: Symbol) stateIn msgIn (m :: Type -> Type) stateOut =
  MatchImpl (stateIn -> msgIn -> m stateOut)

derive instance Newtype (MatchImpl symStateIn symMsgIn stateIn msgIn m stateOut) _

-- | Wrapper for return values via a guard condition.
newtype RetVia (symGuard :: Symbol) a = RetVia a

derive instance Newtype (RetVia symGuard a) _

-- | Wrapper for direct return values.
newtype Ret a = Ret a

derive instance Newtype (Ret a) _

--------------------------------------------------------------------------------
--- Type class for transit specifications
--------------------------------------------------------------------------------

-- | Type class that identifies a type as a valid transit specification.
class IsTransitSpec :: forall spec. spec -> TransitCoreTL -> Constraint
class IsTransitSpec spec core | spec -> core

instance IsTransitSpec (MkTransitCoreTL xs) (MkTransitCoreTL xs)

--------------------------------------------------------------------------------
--- Utility functions
--------------------------------------------------------------------------------

-- | Extracts all unique state names from a transit core.
-- |
-- | Returns all states that appear as either source states or target states
-- | in any match, with duplicates removed.
getStateNames :: TransitCore -> Array StateName
getStateNames (TransitCore matches) = Array.nub $ Array.concatMap
  ( \(Match from _ returns) -> [ from ] <> map
      ( case _ of
          Return to -> to
          ReturnVia _ to -> to
      )
      returns
  )
  matches

-- | Gets all matches (transitions) that originate from a given state.
-- |
-- | Returns an array of all matches where the source state matches the
-- | provided state name.
getMatchesForState :: StateName -> TransitCore -> Array Match
getMatchesForState stateName (TransitCore matches) = Array.filter (\(Match from _ _) -> from == stateName) matches