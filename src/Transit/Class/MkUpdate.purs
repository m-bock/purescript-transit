-- | Type class for building state update functions from transit specifications.
-- |
-- | This module provides the core functionality for executing state transitions
-- | based on a type-level specification of the state machine.

module Transit.Class.MkUpdate
  ( class MkUpdate
  , mkUpdateCore
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Tuple.Nested (type (/\), (/\))
import Transit.Class.ExpandReturn (class ExpandReturn, expandReturn)
import Transit.Class.MatchBySym (class MatchBySym, matchBySym2)
import Transit.Core (MatchImpl(..), MkMatchTL, MkTransitCoreTL, TransitCoreTL)
import Type.Data.List (type (:>), Nil')

-- | Builds a state update function from a transit specification.
-- |
-- | The functional dependency `spec msg state m -> matches` ensures that given
-- | the specification, message type, state type, and monad, the matches type
-- | is uniquely determined.
-- |
-- | - `spec`: The transit core type-level specification
-- | - `m`: The monad for effectful updates
-- | - `matches`: The matches type (nested tuple of match implementations)
-- | - `msg`: The message variant type
-- | - `state`: The state variant type
class MkUpdate (spec :: TransitCoreTL) m matches msg state | spec msg state m -> matches where
  mkUpdateCore :: matches -> state -> msg -> m (Maybe state)

-- | Base case: empty specification always returns Nothing.
instance mkUpdateNil :: (Applicative m) => MkUpdate (MkTransitCoreTL Nil') m Unit msg state where
  mkUpdateCore _ _ _ = pure Nothing

-- | Recursive case: matches state and message, executes transition if found.
instance mkUpdateCons ::
  ( MatchBySym symStateIn state stateIn
  , ExpandReturn returns state stateOut
  , MatchBySym symMsg msg msgIn
  , MkUpdate (MkTransitCoreTL rest1) m rest2 msg state
  , Applicative m
  ) =>
  MkUpdate
    (MkTransitCoreTL ((MkMatchTL symStateIn symMsg returns) :> rest1))
    m
    (MatchImpl symStateIn symMsg stateIn msgIn m stateOut /\ rest2)
    msg
    state
  where
  mkUpdateCore (MatchImpl fn /\ rest) state msg = result
    where
    result :: m (Maybe state)
    result = matchBySym2 @symStateIn @symMsg handleMatch handleRest state msg

    handleMatch :: stateIn -> msgIn -> m (Maybe state)
    handleMatch stateIn msgIn = map (Just <<< expandReturn @returns) (fn stateIn msgIn)

    handleRest :: Unit -> m (Maybe state)
    handleRest _ = mkUpdateCore @(MkTransitCoreTL rest1) rest state msg

