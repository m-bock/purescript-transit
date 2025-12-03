module Test.Transit.Core where

import Prelude

import Data.Reflectable (reflectType)
import Data.Time.Duration (Milliseconds(..))
import Effect.Aff (delay)
import Test.Spec (Spec, describe, it, pending)
import Test.Spec.Assertions (shouldEqual)
import Transit.Core (MkReturn, MkReturnVia, MkStateGraph, MkTransition, Return, Return_(..), StateGraph, StateGraph_(..), Transition_(..))
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
