module Test.Example where

import Transit.Core (MkMsgName, MkStateGraph, MkStateName, MkTransition, StateGraph)
import Transit.DSL (type (:*), type (:->), type (:@), type (:|), AddTransition, MkSG, SG, TransitionBuilderAddExtraRet, TransitionBuilderAddRet, TransitionBuilderInit)
import Transit.Util (type (:<))
import Type.Data.List (Nil')
import Type.Function (type (#))

type MyStateGraph :: StateGraph
type MyStateGraph = MkStateGraph
  ( Nil'
      :< (MkTransition (MkStateName "State1") (MkMsgName "Msg1") (Nil' :< (MkStateName "State2")))
      :< (MkTransition (MkStateName "State2") (MkMsgName "Msg2") (Nil' :< (MkStateName "State3") :< (MkStateName "State1")))
  )

type MyStateGraph2' :: SG
type MyStateGraph2' = MkSG
  # AddTransition (TransitionBuilderInit "State1" "Msg1" # TransitionBuilderAddRet "State2")
  # AddTransition (TransitionBuilderInit "State2" "Msg2" # TransitionBuilderAddRet "State3" # TransitionBuilderAddExtraRet "State1")

type MyStateGraph3 :: SG
type MyStateGraph3 = MkSG
  :* ("State1" :@ "Msg1" :-> "State2")
  :* ("State2" :@ "Msg2" :-> "State3" :| "State1")
