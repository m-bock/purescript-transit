module Transit.Core where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Transit.MatchSub (class GetSubset, class MatchSub, integrate, matchSub2)
import Transit.Util (type (:<))
import Type.Data.List (List', Nil')

type StateName = Symbol

type MsgName = Symbol

---

foreign import data StateGraph :: Type

foreign import data MkStateGraph :: List' Transition -> StateGraph

---

foreign import data Transition :: Type

foreign import data MkTransition :: StateName -> MsgName -> List' StateName -> Transition

---

class MkUpdate :: forall k. k -> Type -> Type -> Type -> Constraint
class MkUpdate spec impl msg state | spec msg state -> impl where
  mkUpdate :: impl -> msg -> state -> state

instance MkUpdate (MkStateGraph Nil') Unit msg state where
  mkUpdate _ _ state = state

instance
  ( MatchSub symStateIn state stateIn
  , GetSubset symsStateOut state stateOut
  , MatchSub symMsg msg msgIn
  , MkUpdate (MkStateGraph rest1) rest2 msg state
  ) =>
  MkUpdate
    (MkStateGraph (rest1 :< (MkTransition symStateIn symMsg symsStateOut)))
    (rest2 /\ Match symStateIn symMsg msgIn stateIn stateOut)
    msg
    state
  where
  mkUpdate (rest /\ Match fn) msg state =
    matchSub2 @symMsg @symStateIn
      (\m s -> integrate @symsStateOut $ fn m s)
      (\_ -> mkUpdate @(MkStateGraph rest1) rest msg state)
      msg
      state

---
newtype Match (symState :: Symbol) (symMsg :: Symbol) msgIn stateIn stateOut = Match (msgIn -> stateIn -> stateOut)

---

