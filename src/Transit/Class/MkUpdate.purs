module Transit.MkUpdate
  ( class MkUpdate
  , mkUpdate
  ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Transit.Class.MatchBySym (class MatchBySym, matchBySym2)
import Transit.Core (MatchImpl(..), MkMatch, MkTransitCore, TransitCore)
import Transit.GetSubset (class GetSubset, getSubset)
import Type.Data.List (type (:>), Nil')

class MkUpdate (spec :: TransitCore) m impl msg state | spec msg state m -> impl where
  mkUpdate :: impl -> state -> msg -> m state

instance (Monad m) => MkUpdate (MkTransitCore Nil') m Unit msg state where
  mkUpdate _ state _ = pure state

instance
  ( MatchBySym symStateIn state stateIn
  , GetSubset returns state stateOut
  , MatchBySym symMsg msg msgIn
  , MkUpdate (MkTransitCore rest1) m rest2 msg state
  , Applicative m
  ) =>
  MkUpdate
    (MkTransitCore ((MkMatch symStateIn symMsg returns) :> rest1))
    m
    (MatchImpl symStateIn symMsg stateIn msgIn stateOut /\ rest2)
    msg
    state
  where
  mkUpdate (MatchImpl fn /\ rest) state msg =
    matchBySym2 @symStateIn @symMsg
      (\s m -> pure $ getSubset @returns $ fn s m)
      (\_ -> mkUpdate @(MkTransitCore rest1) rest state msg)
      state
      msg

