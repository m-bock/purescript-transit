module Transit.Class.MkUpdate
  ( class MkUpdate
  , mkUpdate
  ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Transit.Class.MatchBySym (class MatchBySym, matchBySym2)
import Transit.Core (MatchImpl(..), MkMatchTL, MkTransitCoreTL, TransitCoreTL)
import Transit.Class.GetSubset (class GetSubset, getSubset)
import Type.Data.List (type (:>), Nil')

class MkUpdate (spec :: TransitCoreTL) m impl msg state | spec msg state m -> impl where
  mkUpdate :: impl -> state -> msg -> m state

data No = No

instance (Monad m) => MkUpdate (MkTransitCoreTL Nil') m Unit msg state where
  mkUpdate _ state _ = pure state

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
      (\s m -> getSubset @returns <$> fn s m)
      (\_ -> mkUpdate @(MkTransitCoreTL rest1) rest state msg)
      state
      msg

