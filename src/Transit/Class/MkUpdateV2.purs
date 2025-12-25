-- @inline export mkUpdateInst arity=2

module Transit.Class.MkUpdateV2 where

-- | Type class for building state update functions from transit specifications.
-- |
-- | This module provides the core functionality for executing state transitions
-- | based on a type-level specification of the state machine.

import Prelude

import Data.Either (Either, note)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Prim.Row as Row
import Transit.Class.ExpandReturn (class RemoveWrappers, removeWrappers)
import Transit.Core (MatchImpl(..), MkMatchTL, MkTransitCoreTL, TransitCoreTL)
import Transit.HandlerLookup (HandlerLookupBuilder, addHandler, build, initBuilder, run)
import Type.Data.List (type (:>), Nil')

-- | Error type for illegal state transitions.
-- |
-- | `IllegalTransitionRequest` is returned when a transition is attempted that
-- | is not defined in the state machine specification.
data TransitError = IllegalTransitionRequest

derive instance Generic TransitError _

derive instance Eq TransitError

instance Show TransitError where
  show = genericShow

class
  MkUpdate (spec :: TransitCoreTL) m matches msg state
  | spec msg state m -> matches
  where
  mkUpdateCore :: matches -> state -> msg -> m (Either TransitError state)

instance mkUpdateInst ::
  ( MkLookup m spec matches rowState rowMsg
  , Applicative m
  ) =>
  MkUpdate (MkTransitCoreTL spec) m matches (Variant rowMsg) (Variant rowState) where
  mkUpdateCore matches =
    let
      handerLookup = mkLookup @m @spec matches
      h = build handerLookup
      run' = run h
    in
      \state msg -> map (note IllegalTransitionRequest) $ run' state msg

-- mkUpdateCore
--   :: forall m spec matches rowState rowMsg
--    . (MkLookup m spec matches rowState rowMsg)
--   => Applicative m
--   => matches
--   -> Variant rowState
--   -> Variant rowMsg
--   -> m (Maybe (Variant rowState))
-- mkUpdateCore matches =
--   let
--     handerLookup = mkLookup @m @spec matches
--     h = build handerLookup
--   in
--     \state msg -> run h state msg

class MkLookup (m :: Type -> Type) spec matches (rowState :: Row Type) (rowMsg :: Row Type) | spec rowState rowMsg m -> matches where
  mkLookup :: matches -> HandlerLookupBuilder m rowState rowMsg

instance MkLookup m (Nil') Unit rowState rowMsg where
  mkLookup _ = initBuilder @rowState @rowMsg

instance
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