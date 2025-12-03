module Transit.MkUpdate where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Transit.Class.MatchBySym (class MatchBySym, matchBySym2)
import Transit.Core (Match(..), MkStateGraph, MkTransition, StateGraph)
import Transit.GetSubset (class GetSubset, getSubset)
import Transit.Tmp (class Revert)
import Transit.Util (type (:<))
import Type.Data.List (type (:>), Nil')

class MkUpdate (spec :: StateGraph) m impl msg state | spec msg state m -> impl where
  mkUpdate :: impl -> msg -> state -> m state

instance (Monad m) => MkUpdate (MkStateGraph Nil') m Unit msg state where
  mkUpdate _ _ state = pure state

instance
  ( MatchBySym symStateIn state stateIn
  , GetSubset returns state stateOut
  , MatchBySym symMsg msg msgIn
  , MkUpdate (MkStateGraph rest1) m rest2 msg state
  , Applicative m
  ) =>
  MkUpdate
    (MkStateGraph ((MkTransition symStateIn symMsg returns) :> rest1))
    m
    (Match symStateIn symMsg msgIn stateIn stateOut /\ rest2)
    msg
    state
  where
  mkUpdate (Match fn /\ rest) msg state =
    matchBySym2 @symMsg @symStateIn
      (\m s -> pure $ getSubset @returns $ fn m s)
      (\_ -> mkUpdate @(MkStateGraph rest1) rest msg state)
      msg
      state

