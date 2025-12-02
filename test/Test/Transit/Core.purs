module Test.Transit.Core where

import Prelude

import Data.Reflectable (reflectType)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Transit.Core (MkReturn, MkReturnVia, MkStateGraph, MkTransition, Return, Return_(..), StateGraph, StateGraph_(..), Transition_(..))
import Transit.Util (type (:<))
import Type.Data.List (Nil')
import Type.Proxy (Proxy(..))

type MyStateGraph :: StateGraph
type MyStateGraph = MkStateGraph
  ( Nil'
      :< (MkTransition "TestState1" "TestMsg1" (Nil' :< MkReturn "TestState2"))
      :< (MkTransition "TestState2" "TestMsg2" (Nil' :< MkReturn "TestState3"))

  -- :< (MkTransition "TestState2" "TestMsg2" (Nil' :< MkReturnVia "foo" "TestState3" :< MkReturn "TestState1"))
  )

spec :: Spec Unit
spec = do
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let ret = reflectType (Proxy @(MkStateGraph Nil'))
        ret `shouldEqual` (StateGraph [])

    describe "MyStateGraph" do
      it "should be equal to the reflected type" do
        let ret = reflectType (Proxy @MyStateGraph)
        ret `shouldEqual`
          ( StateGraph
              [ (Transition "TestState1" "TestMsg1" [ (Return "TestState2") ])
              , (Transition "TestState2" "TestMsg2" [ (Return "TestState3") ])
              ]
          )
