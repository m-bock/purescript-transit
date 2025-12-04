module Test.Transit.Core where

import Prelude

import Data.Reflectable (reflectType)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Core (MkReturn, MkStateGraph, MkTransition, StateGraph)
import Transit.Reflection (Return_(..))
import Transit.Reflection as R
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))

type MyStateGraph :: StateGraph
type MyStateGraph = MkStateGraph
  ( (MkTransition "TestState1" "TestMsg1" (MkReturn "TestState2" :> Nil'))
      :> (MkTransition "TestState2" "TestMsg2" (MkReturn "TestState3" :> Nil'))
      :> Nil'

  )

spec :: Spec Unit
spec = do
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let ret = reflectType (Proxy @(MkStateGraph Nil'))
        ret `shouldEqual` (R.StateGraph [])

    describe "MyStateGraph" do
      it "should be equal to the reflected type" do
        let ret = reflectType (Proxy @MyStateGraph)
        ret `shouldEqual`
          ( R.StateGraph
              [ (R.Transition "TestState1" "TestMsg1" [ (R.Return "TestState2") ])
              , (R.Transition "TestState2" "TestMsg2" [ (R.Return "TestState3") ])
              ]
          )
