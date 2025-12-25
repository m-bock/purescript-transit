-- @inline export mkUpdateInst arity=2

module Transit.Class.MkUpdateV3 where

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
import Unsafe.Coerce (unsafeCoerce)

-- | Error type for illegal state transitions.
-- |
-- | `IllegalTransitionRequest` is returned when a transition is attempted that
-- | is not defined in the state machine specification.
data TransitError = IllegalTransitionRequest

derive instance Generic TransitError _

derive instance Eq TransitError

instance Show TransitError where
  show = genericShow

class MkUpdate (spec :: TransitCoreTL) m matches msg state | spec msg state m -> matches where
  mkUpdateCore :: matches -> state -> msg -> m (Either TransitError state)

-- instance mkUpdateInst ::
--   ( MkLookup m spec matches rowState rowMsg
--   , Applicative m
--   ) =>
--   MkUpdate spec m matches (Variant rowMsg) (Variant rowState) where
--   mkUpdateCore matches =
--     let
--       handerLookup = mkLookup @m @spec matches
--       h = build handerLookup
--       run' = run h
--     in
--       \state msg -> map (note IllegalTransitionRequest) $ run' state msg

class TransformMatches (prevStateSym :: MaybeTL Symbol) matches (out :: Row Type) (out' :: Row Type) | matches -> out out' where
  transformMatches :: matches -> Record out

instance TransformMatches s Unit () () where
  transformMatches _ = {}

-- instance TransformMatches (JustTL symStateIn) (MatchImpl symStateIn symMsg stateIn msgIn m (Variant rowStateOut) /\ rest) r () where
--   transformMatches _ = unsafeCoerce ""

-- else instance TransformMatches (JustTL symStateIn') (MatchImpl symStateIn symMsg stateIn msgIn m (Variant rowStateOut) /\ rest) r () where
--   transformMatches _ = unsafeCoerce ""

-- instance
--   ( TransformMatches (JustTL symStateIn) rest
--   ) =>
--   TransformMatches NothingTL (MatchImpl symStateIn symMsg stateIn msgIn m (Variant rowStateOut) /\ rest) r () where
--   transformMatches _ = unsafeCoerce ""

foreign import data MaybeTL :: forall k. k -> Type

foreign import data JustTL :: forall k. k -> MaybeTL k

foreign import data NothingTL :: forall k. MaybeTL k