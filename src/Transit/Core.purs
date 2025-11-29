module Transit.Core where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Transit.MatchSub (class GetSubset, class MatchSub, integrate, matchSub2)
import Transit.Util (type (:<))
import Type.Data.List (type (:>), List', Nil')
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

type StateName = Symbol

type MsgName = Symbol

---

foreign import data StateGraph :: Type

foreign import data MkStateGraph :: List' Transition -> StateGraph

---

foreign import data Transition :: Type

foreign import data MkTransition :: StateName -> MsgName -> List' Return -> Transition

---

foreign import data Return :: Type

foreign import data MkReturn :: StateName -> Return

---

class UnwrapReturns (xs :: List' Return) (ys :: List' StateName) | xs -> ys

instance UnwrapReturns Nil' Nil'
instance (UnwrapReturns xs ys) => UnwrapReturns (MkReturn a :> xs) (a :> ys)

---

class MkUpdate :: StateGraph -> Type -> Type -> Type -> Constraint
class MkUpdate spec impl msg state | spec msg state -> impl where
  mkUpdate :: impl -> msg -> state -> state

instance MkUpdate (MkStateGraph Nil') Unit msg state where
  mkUpdate _ _ state = state

instance
  ( MatchSub symStateIn state stateIn
  , GetSubset symsStateOut state stateOut
  , MatchSub symMsg msg msgIn
  , MkUpdate (MkStateGraph rest1) rest2 msg state
  , UnwrapReturns returns symsStateOut
  ) =>
  MkUpdate
    (MkStateGraph (rest1 :< (MkTransition symStateIn symMsg returns)))
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

match :: forall @symState @symMsg msgIn stateIn stateOut. (msgIn -> stateIn -> stateOut) -> Match symState symMsg msgIn stateIn stateOut
match f = Match f

---

return :: forall (@sym ∷ Symbol) (a ∷ Type) (r1 ∷ Row Type) (r2 ∷ Row Type). Row.Cons sym a r1 r2 ⇒ IsSymbol sym ⇒ a → Variant r2
return v = V.inj (Proxy :: _ sym) v

