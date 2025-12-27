-- @inline export mkUpdateInst arity=2

module Transit.Class.MkUpdate where

-- | Type class for building state update functions from transit specifications.
-- |
-- | This module provides the core functionality for executing state transitions
-- | based on a type-level specification of the state machine.

import Prelude

import Control.Alternative (class Alternative)
import Data.Function.Uncurried (runFn4)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Prim.Row as Row
import Transit.Class.ExpandReturn (class RemoveWrappers, removeWrappersFast)
import Transit.Core (MatchImpl(..), MatchTL, MkMatchTL, MkTransitCoreTL, TransitCoreTL)
import Transit.HandlerLookup (HandlerLookupBuilder, addHandler, build, initBuilder, runI, runImpl)
import Type.Data.List (type (:>), List', Nil')

class
  MkUpdate (spec :: TransitCoreTL) (m :: Type -> Type) (may :: Type -> Type) matches msg state
  | spec msg state m -> matches
  where
  mkUpdateCore :: matches -> state -> msg -> m (may state)

instance mkUpdateInst ::
  ( MkLookup m spec matches rowState rowMsg
  , Applicative m
  , Alternative may
  ) =>
  MkUpdate (MkTransitCoreTL spec) m may matches (Variant rowMsg) (Variant rowState) where
  mkUpdateCore matches =
    let
      handerLookupBuilder = mkLookup @m @spec matches
      handlerLookup = build handerLookupBuilder
    in
      \state msg -> runFn4 runImpl runI handlerLookup state msg

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
    ((MkMatchTL symStateIn symMsg returns) :> rest1)
    (MatchImpl symStateIn symMsg stateIn msgIn m (Variant rowStateOut) /\ rest2)
    rowState
    rowMsg
  where
  mkLookup (MatchImpl fn /\ rest) = out
    where
    out = addHandler @symStateIn @symMsg fn' builder
    builder = mkLookup @m @(rest1) rest

    fn' :: stateIn -> msgIn -> m (Variant rowStateOut')
    fn' state msg = map (removeWrappersFast @returns) (fn state msg)