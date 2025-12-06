module Test.Transit.Core where

import Prelude

import Data.Reflectable (reflectType)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Core (Match_(..), MkMatch, MkReturn, MkTransitCore, Return_(..), TransitCore, TransitCore_(..))
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))

type MyStateGraph :: TransitCore
type MyStateGraph = MkTransitCore
  ( (MkMatch "TestState1" "TestMsg1" (MkReturn "TestState2" :> Nil'))
      :> (MkMatch "TestState2" "TestMsg2" (MkReturn "TestState3" :> Nil'))
      :> Nil'

  )

spec :: Spec Unit
spec = do
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let ret = reflectType (Proxy @(MkTransitCore Nil'))
        ret `shouldEqual` (TransitCore [])

    describe "MyStateGraph" do
      it "should be equal to the reflected type" do
        let ret = reflectType (Proxy @MyStateGraph)
        ret `shouldEqual`
          ( TransitCore
              [ (Match "TestState1" "TestMsg1" [ (Return "TestState2") ])
              , (Match "TestState2" "TestMsg2" [ (Return "TestState3") ])
              ]
          )
