module Transit.Class.MkUpdate
  ( class MkUpdate
  , mkUpdate
  , TransitError(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested (type (/\), (/\))
import Transit.Class.GetSubset (class GetSubset, getSubset)
import Transit.Class.MatchBySym (class MatchBySym, matchBySym2)
import Transit.Core (MatchImpl(..), MkMatchTL, MkTransitCoreTL, TransitCoreTL)
import Type.Data.List (type (:>), Nil')
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data TransitError = IllegalTransitionRequest

derive instance Generic TransitError _

derive instance Eq TransitError

instance Show TransitError where
  show = genericShow

class MkUpdate (spec :: TransitCoreTL) m impl msg state | spec msg state m -> impl where
  mkUpdate :: impl -> state -> msg -> m (Either TransitError state)

instance (Applicative m) => MkUpdate (MkTransitCoreTL Nil') m Unit msg state where
  mkUpdate _ _ _ = pure
    (Left IllegalTransitionRequest)

instance
  ( MatchBySym symStateIn state stateIn
  , GetSubset returns state stateOut
  , MatchBySym symMsg msg msgIn
  , MkUpdate (MkTransitCoreTL rest1) m rest2 msg state
  , Applicative m
  ) =>
  MkUpdate
    (MkTransitCoreTL ((MkMatchTL symStateIn symMsg returns) :> rest1))
    m
    (MatchImpl symStateIn symMsg m stateIn msgIn stateOut /\ rest2)
    msg
    state
  where
  mkUpdate (MatchImpl fn /\ rest) state msg =
    matchBySym2 @symStateIn @symMsg
      (\s m -> Right <$> (getSubset @returns <$> fn s m))
      (\_ -> mkUpdate @(MkTransitCoreTL rest1) rest state msg)
      state
      msg

