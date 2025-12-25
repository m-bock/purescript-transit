-- | Main API for building type-safe state machines.
-- |
-- | This module provides the core functions for creating state machine update
-- | functions from type-level specifications.
-- | ```
module Transit
  ( class Return
  , class ReturnVia
  , match
  , matchM
  , mkUpdate
  , mkUpdateEither
  , mkUpdateEitherM
  , mkUpdateM
  , module ExportCore
  , module ExportDSL
  , module ExportMkUpdate
  , module ExportStateGraph
  , return
  , returnVia
  ) where

import Prelude

import Data.Either (Either, fromRight)
import Data.Identity (Identity(..))
import Data.Symbol (class IsSymbol)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Coerce (class Coercible)
import Prim.Row as Row
import Safe.Coerce as Safe
import Transit.Class.CurryN (class CurryN, curryN)
import Transit.Class.MkUpdate (class MkUpdate, TransitError, mkUpdateCore)
import Transit.Class.MkUpdateV2 as UV2
import Transit.Class.MkUpdate (TransitError(..)) as ExportMkUpdate
import Transit.Class.MkUpdate as MkUpdate
import Transit.Core (class IsTransitSpec, MatchImpl(..), Ret(..), RetVia(..))
import Transit.Core (GuardName, Match(..), MsgName, StateName, TransitCore(..), getMatchesForState, getStateNames) as ExportCore
import Transit.DSL (type (|<), AddIn, class ToMatch, class ToReturn, class ToTransitCore, type (:*), type (:?), type (:@), type (>|), Transit) as ExportDSL
import Transit.StateGraph (mkStateGraph, StateGraph) as ExportStateGraph
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- Update Function Builders
--------------------------------------------------------------------------------

-- | Creates a monadic update function with error handling.
-- |
-- | Returns `m (Either TransitError state)`, allowing you to distinguish
-- | between successful transitions and illegal transition requests.
-- |
-- | Example:
-- | ```purescript
-- | update :: State -> Msg -> Effect (Either TransitError State)
-- | update = mkUpdateEitherM @MyTransit ...
-- | ```
mkUpdateEitherM
  :: forall @spec tcore msg state args m a
   . (IsTransitSpec spec tcore)
  => (CurryN args (state -> msg -> m (Either TransitError state)) a)
  => (MkUpdate.MkUpdate tcore m args msg state)
  => a
mkUpdateEitherM = curryN @args f
  where
  f :: args -> state -> msg -> m (Either TransitError state)
  f impl state msg =
    mkUpdateCore @tcore impl state msg

-- | Creates a pure update function with error handling.
-- |
-- | Returns `Either TransitError state`, allowing you to distinguish
-- | between successful transitions and illegal transition requests.
-- |
-- | Example:
-- | ```purescript
-- | update :: State -> Msg -> Either TransitError State
-- | update = mkUpdateEither @MyTransit ...
-- | ```
mkUpdateEither
  :: forall @spec tcore msg state args a
   . (IsTransitSpec spec tcore)
  => (CurryN args (state -> msg -> Either TransitError state) a)
  => (MkUpdate tcore Identity args msg state)
  => a
mkUpdateEither = curryN @args f
  where
  f :: args -> state -> msg -> Either TransitError state
  f impl state msg =
    safeUnwrap @Identity $
      (mkUpdateCore @tcore impl state msg)

-- | Creates a monadic update function.
-- |
-- | Returns `m state`, silently returning the unchanged state on illegal
-- | transitions. Use `mkUpdateEitherM` if you need error handling.
-- |
-- | Example:
-- | ```purescript
-- | update :: State -> Msg -> Effect State
-- | update = mkUpdateM @MyTransit ...
-- | ```
mkUpdateM
  :: forall @spec tcore msg state args m a
   . (IsTransitSpec spec tcore)
  => (CurryN args (state -> msg -> m state) a)
  => (MkUpdate.MkUpdate tcore m args msg state)
  => Functor m
  => a
mkUpdateM = curryN @args f
  where
  f :: args -> state -> msg -> m state
  f impl state msg =
    map (fromRight state)
      (mkUpdateCore @tcore impl state msg)

-- | Creates a pure update function.
-- |
-- | Returns `state`, silently returning the unchanged state on illegal
-- | transitions. Use `mkUpdateEither` if you need error handling.
-- |
-- | Example:
-- | ```purescript
-- | update :: State -> Msg -> State
-- | update = mkUpdate @MyTransit ...
-- | ```
-- mkUpdate
--   :: forall @spec tcore msg state args a
--    . (IsTransitSpec spec tcore)
--   => (CurryN args (state -> msg -> state) a)
--   => (MkUpdate tcore Identity args msg state)
--   => a
-- mkUpdate = curryN @args f
--   where
--   f :: args -> state -> msg -> state
--   f impl state msg =
--     fromRight state $ safeUnwrap @Identity $ (mkUpdateCore @tcore impl state msg)

mkUpdate
  :: forall @spec tcore msg state args a
   . (IsTransitSpec spec tcore)
  => (CurryN args (Variant state -> Variant msg -> Variant state) a)
  => (UV2.MkUpdate tcore Identity args (Variant msg) (Variant state))
  => a
mkUpdate = curryN @args f
  where
  f :: args -> Variant state -> Variant msg -> Variant state
  f impl =
    let
      f' = UV2.mkUpdateCore @tcore impl
    in
      \state msg -> fromRight state $ safeUnwrap @Identity $ f' state msg

-- | Internal helper for unwrapping Identity.
safeUnwrap :: forall @f a. Coercible (f a) a => f a -> a
safeUnwrap = Safe.coerce

--------------------------------------------------------------------------------
--- Match Handlers
--------------------------------------------------------------------------------

-- | Creates a pure match handler for a state transition.
-- |
-- | The handler function receives the current state and message, and returns
-- | the new state. Use `matchM` for monadic handlers.
-- |
-- | Example:
-- | ```purescript
-- | match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed"
-- | ```
match
  :: forall @symStateIn @symMsgIn stateIn msgIn stateOut
   . (stateIn -> msgIn -> stateOut)
  -> MatchImpl symStateIn symMsgIn stateIn msgIn Identity stateOut
match f = MatchImpl (\state msg -> pure $ f state msg)

-- | Creates a monadic match handler for a state transition.
-- |
-- | The handler function receives the current state and message, and returns
-- | the new state in a monadic context. Use `match` for pure handlers.
-- |
-- | Example:
-- | ```purescript
-- | matchM @"DoorOpen" @"Close" \_ _ -> do
-- |   Console.log "Closing door"
-- |   pure $ return @"DoorClosed"
-- | ```
matchM
  :: forall @symStateIn @symMsgIn m stateIn msgIn stateOut
   . (stateIn -> msgIn -> m stateOut)
  -> MatchImpl symStateIn symMsgIn stateIn msgIn m stateOut
matchM f = MatchImpl (\state msg -> f state msg)

--------------------------------------------------------------------------------
--- Return Functions
--------------------------------------------------------------------------------

-- | Type class for returning to a state in a transition handler.
-- |
-- | Used with `return @"StateName"` to specify the target state after a transition.
class Return (sym :: Symbol) a where
  -- | Returns to the specified state.
  -- |
  -- | For states with payloads:
  -- | ```purescript
  -- | return @"StateName" { field: value }
  -- | ```
  -- |
  -- | For states with empty payloads:
  -- | ```purescript
  -- | return @"StateName"
  -- | ```
  return :: a

instance (Row.Cons sym (Ret a) r1 r2, IsSymbol sym) => Return sym (a -> Variant r2) where
  return v = V.inj (Proxy :: _ sym) (Ret v)

instance (Row.Cons sym (Ret {}) r1 r2, IsSymbol sym) => Return sym (Variant r2) where
  return = V.inj (Proxy :: _ sym) (Ret {})

-- | Type class for returning to a state via a guard condition.
-- |
-- | Used with `returnVia @"GuardName" @"StateName"` to specify a conditional
-- | transition with a guard.
class ReturnVia (symGuard :: Symbol) (sym :: Symbol) a where
  -- | Returns to the specified state via a guard condition.
  -- |
  -- | For states with payloads:
  -- | ```purescript
  -- | returnVia @"GuardName" @"StateName" { field: value }
  -- | ```
  -- |
  -- | For states with empty payloads:
  -- | ```purescript
  -- | returnVia @"GuardName" @"StateName"
  -- | ```
  returnVia :: a

instance (Row.Cons sym (RetVia symGuard a) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (a -> Variant r2) where
  returnVia v = V.inj (Proxy :: _ sym) (RetVia @symGuard v)

instance (Row.Cons sym (RetVia symGuard {}) r1 r2, IsSymbol sym) => ReturnVia symGuard sym (Variant r2) where
  returnVia = V.inj (Proxy :: _ sym) (RetVia @symGuard {})
