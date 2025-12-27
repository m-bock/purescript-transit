-- @inline export mkUpdateInst arity=2

module Transit.Class.MkUpdateV2 where

-- | Type class for building state update functions from transit specifications.
-- |
-- | This module provides the core functionality for executing state transitions
-- | based on a type-level specification of the state machine.

import Prelude

import Data.Function.Uncurried (runFn4)
import Data.Maybe (Maybe)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Prim.Row as Row
import Transit.Class.ExpandReturn (class RemoveWrappers, removeWrappers)
import Transit.Core (MatchImpl(..), MatchTL, MkMatchTL, MkTransitCoreTL, TransitCoreTL)
import Transit.Data.MaybeChurch (MaybeChurch)
import Transit.HandlerLookup (HandlerLookupBuilder, addHandler, build, initBuilder, runIMaybe, runImpl)
import Type.Data.List (type (:>), List', Nil')

class
  MkUpdate (spec :: TransitCoreTL) m matches msg state
  | spec msg state m -> matches
  where
  mkUpdateCore :: matches -> state -> msg -> m (MaybeChurch state)

instance mkUpdateInst ::
  ( MkLookup m spec matches rowState rowMsg
  , Applicative m
  ) =>
  MkUpdate (MkTransitCoreTL spec) m matches (Variant rowMsg) (Variant rowState) where
  mkUpdateCore matches =
    let
      handerLookupBuilder = mkLookup @m @spec matches
      handlerLookup = build handerLookupBuilder
    in
      \state msg -> runFn4 runImpl runIMaybe handlerLookup state msg

class
  MkLookup
    (m :: Type -> Type)
    (spec :: List' MatchTL)
    matches
    (rowState :: Row Type)
    (rowMsg :: Row Type)
  | spec rowState rowMsg m -> matches where
  mkLookup :: matches -> HandlerLookupBuilder m rowState rowMsg

instance mkLookupNil :: MkLookup m (Nil') Unit rowState rowMsg where
  mkLookup _ = initBuilder @rowState @rowMsg

instance mkLookupCons ::
  ( IsSymbol symStateIn
  , IsSymbol symMsg
  , RemoveWrappers returns rowStateOut rowStateOut'
  , Row.Cons symStateIn stateIn _x1 rowState
  , Row.Cons symMsg msgIn _x2 rowMsg
  , Row.Union rowStateOut' _x3 rowState
  , Functor m
  , MkLookup m (rest1) rest2 rowState rowMsg
  ) =>
  MkLookup m
    (((MkMatchTL symStateIn symMsg returns) :> rest1))
    (MatchImpl symStateIn symMsg stateIn msgIn m (Variant rowStateOut) /\ rest2)
    rowState
    rowMsg
  where
  mkLookup (MatchImpl fn /\ rest) = addHandler @symStateIn @symMsg fn' builder
    where
    builder = mkLookup @m @(rest1) rest

    fn' :: stateIn -> msgIn -> m (Variant rowStateOut')
    fn' s m = fn s m # map (removeWrappers @returns)

---