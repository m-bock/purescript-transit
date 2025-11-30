module Transit.MkUpdate where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Safe.Coerce (coerce)
import Transit.Class.MatchBySym (class MatchBySym, matchBySym2)
import Transit.Core (Match(..), MkStateGraph, MkTransition, StateGraph)
import Transit.GetSubset (class GetSubset, getSubset)
import Transit.Util (type (:<), Generically(..))
import Type.Data.List (type (:>), List', Nil')
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

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
