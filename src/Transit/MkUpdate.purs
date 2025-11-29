module Transit.MkUpdate where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Safe.Coerce (coerce)
import Transit.Core (MkStateGraph, MkTransition, StateGraph)
import Transit.MatchSub (class GetSubset, class MatchSub, integrate, matchSub2)
import Transit.Util (type (:<), Generically(..))
import Type.Data.List (type (:>), List', Nil')
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

mkUpdateG :: forall @spec impl msg state. (MkUpdate spec impl (Generically msg) (Generically state)) => impl -> msg -> state -> state
mkUpdateG impl msg state = coerce $ mkUpdate @spec impl (Generically msg) (Generically state)

class MkUpdate :: StateGraph -> Type -> Type -> Type -> Constraint
class MkUpdate spec impl msg state | spec msg state -> impl where
  mkUpdate :: impl -> msg -> state -> state

instance MkUpdate (MkStateGraph Nil') Unit msg state where
  mkUpdate _ _ state = state

instance
  ( MatchSub symStateIn state stateIn
  , GetSubset returns state stateOut
  , MatchSub symMsg msg msgIn
  , MkUpdate (MkStateGraph rest1) rest2 msg state
  ) =>
  MkUpdate
    (MkStateGraph (rest1 :< (MkTransition symStateIn symMsg returns)))
    (rest2 /\ Match symStateIn symMsg msgIn stateIn stateOut)
    msg
    state
  where
  mkUpdate (rest /\ Match fn) msg state =
    matchSub2 @symMsg @symStateIn
      (\m s -> integrate @returns $ fn m s)
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

