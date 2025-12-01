module Transit.MkUpdate where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Transit.Class.MatchBySym (class MatchBySym, matchBySym2)
import Transit.Core (Match(..), MkStateGraph, MkTransition, StateGraph)
import Transit.GetSubset (class GetSubset, getSubset)
import Transit.Util (type (:<))
import Type.Data.List (Nil')

class MkUpdate (spec :: StateGraph) impl msg state | spec msg state -> impl where
  mkUpdate :: impl -> msg -> state -> state

instance MkUpdate (MkStateGraph Nil') Unit msg state where
  mkUpdate _ _ state = state

instance
  ( MatchBySym symStateIn state stateIn
  , GetSubset returns state stateOut
  , MatchBySym symMsg msg msgIn
  , MkUpdate (MkStateGraph rest1) rest2 msg state
  ) =>
  MkUpdate
    (MkStateGraph (rest1 :< (MkTransition symStateIn symMsg returns)))
    (rest2 /\ Match symStateIn symMsg msgIn stateIn stateOut)
    msg
    state
  where
  mkUpdate (rest /\ Match fn) msg state =
    matchBySym2 @symMsg @symStateIn
      (\m s -> getSubset @returns $ fn m s)
      (\_ -> mkUpdate @(MkStateGraph rest1) rest msg state)
      msg
      state

