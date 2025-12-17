module Transit.Class.MkUpdate
  ( class MkUpdate
  , mkUpdateCore
  , TransitError(..)
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Transit.Class.ExpandReturn (class ExpandReturn, expandReturn)
import Transit.Class.MatchBySym (class MatchBySym, matchBySym2)
import Transit.Core (MatchImpl(..), MkMatchTL, MkTransitCoreTL, TransitCoreTL)
import Type.Data.List (type (:>), Nil')

data TransitError = IllegalTransitionRequest

derive instance Generic TransitError _

derive instance Eq TransitError

instance Show TransitError where
  show = genericShow

class MkUpdate (spec :: TransitCoreTL) m impl msg state | spec msg state m -> impl where
  mkUpdateCore :: impl -> state -> msg -> m (Either TransitError state)

instance mkUpdateNil :: (Applicative m) => MkUpdate (MkTransitCoreTL Nil') m Unit msg state where
  mkUpdateCore _ _ _ = pure
    (Left IllegalTransitionRequest)

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
  mkUpdateCore (MatchImpl fn /\ rest) state msg =
    matchBySym2 @symStateIn @symMsg
      (\s m -> Right <$> (expandReturn @returns <$> fn s m))
      (\_ -> mkUpdateCore @(MkTransitCoreTL rest1) rest state msg)
      state
      msg
