module Test.Example where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\))
import Data.Unit (unit)
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row (class Cons)
import Transit.Core (Match(..), MkMsgName, MkStateGraph, MkStateName, MkStateNameList, MkTransition, StateGraph, mkUpdate)
import Transit.DSL (type (:*), type (:->), type (:@), type (:|), AddTransition, MkStateGraphDSL, StateGraphDSL, TransitionBuilderAddExtraRet, TransitionBuilderAddRet, TransitionBuilderInit)
import Transit.Util (type (:<), Generically)
import Type.Data.List (Nil')
import Type.Function (type (#))
import Type.Proxy (Proxy(..))

data Msg = Msg1 { foo :: Int } | Msg2 { bar :: String }

derive instance Generic Msg _

data State = State1 { foo :: Int } | State2 { bar :: String } | State3 { baz :: Boolean }

derive instance Generic State _

type MyStateGraph :: StateGraph
type MyStateGraph = MkStateGraph
  ( Nil'
      :< (MkTransition (MkStateName "State1") (MkMsgName "Msg1") (MkStateNameList (Nil' :< "State2")))
      :< (MkTransition (MkStateName "State2") (MkMsgName "Msg2") (MkStateNameList (Nil' :< "State3" :< "State1")))
  )

type MyStateGraphDSLRep :: StateGraphDSL
type MyStateGraphDSLRep = MkStateGraphDSL
  # AddTransition (TransitionBuilderInit "State1" "Msg1" # TransitionBuilderAddRet "State2")
  # AddTransition (TransitionBuilderInit "State2" "Msg2" # TransitionBuilderAddRet "State3" # TransitionBuilderAddExtraRet "State1")

type MyStateGraphDSL :: StateGraphDSL
type MyStateGraphDSL = MkStateGraphDSL
  :* ("State1" :@ "Msg1" :-> "State2")
  :* ("State2" :@ "Msg2" :-> "State3" :| "State1")

update :: Generically Msg -> Generically State -> Generically State
update = mkUpdate @MyStateGraph
  ( unit
      :* _match @"State1" @"Msg1" (\msg state -> _return @"State2" { bar: "" })
      :* _match @"State2" @"Msg2" (\msg state -> _return @"State3" { baz: false })
  )

_match :: forall t0 t1 t2 @t3 @t4. (t0 -> t1 -> t2) -> Match t3 t4 t0 t1 t2
_match f = Match f

_return :: forall (@sym ∷ Symbol) (a ∷ Type) (r1 ∷ Row Type) (r2 ∷ Row Type). Cons sym a r1 r2 ⇒ IsSymbol sym ⇒ a → Variant r2
_return v = V.inj (Proxy :: _ sym) v

infixl 5 Tuple as :*