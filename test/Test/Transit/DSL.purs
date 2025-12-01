module Test.Transit.DSL where

import Data.Reflectable (class Reflectable)
import Data.Unit (Unit, unit)
import Transit.Core (MkReturn, MkStateGraph, MkTransition, StateGraph, StateGraph_(..))
import Transit.Core as C
import Transit.DSL (class FromDSL, type (:*), type (:>), type (:@), type (:|), AddTransition, MkStateSpec, TransitionBuilderAddExtraRet, TransitionBuilderAddRet, TransitionBuilderInit)
import Transit.Util (type (:<))
import Type.Data.List (Cons', Nil')
import Type.Proxy (Proxy(..))

check :: forall @a @b. (FromDSL a b) => Unit
check = unit

test1 :: Unit
test1 = check @MkStateSpec @(MkStateGraph Nil')

type Test2DSL = MkStateSpec
  :* ("State1" :@ "Msg1" :> "State2")
  :* ("State2" :@ "Msg2" :> "State3" :| "State1")
  :* ("State3" :@ "Msg3" :> "State1" :| "State2" :| "State4")

type Test2StateGraph = MkStateGraph
  ( Nil'
      :<
        ( MkTransition "State1" "Msg1"
            (Nil' :< MkReturn "State2")
        )
      :<
        ( MkTransition "State2" "Msg2"
            (Nil' :< MkReturn "State3" :< MkReturn "State1")
        )
      :<
        ( MkTransition "State3" "Msg3"
            (Nil' :< MkReturn "State1" :< MkReturn "State2" :< MkReturn "State4")
        )
  )

test2 :: Unit
test2 = check @Test2DSL @Test2StateGraph
