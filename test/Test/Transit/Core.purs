module Test.Transit.Core
  ( spec
  ) where

import Prelude

import Data.Reflectable (reflectType)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Core (Match(..), MkMatchTL, MkReturnTL, MkTransitCoreTL, Return(..), TransitCore, TransitCore(..), TransitCoreTL)
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))

type MyStateGraph :: TransitCoreTL
type MyStateGraph = MkTransitCoreTL
  ( (MkMatchTL "TestState1" "TestMsg1" (MkReturnTL "TestState2" :> Nil'))
      :> (MkMatchTL "TestState2" "TestMsg2" (MkReturnTL "TestState3" :> Nil'))
      :> Nil'

  )

spec :: Spec Unit
spec = do
  describe "purescript-spec" do
    describe "Attributes" do
      it "awesome" do
        let ret = reflectType (Proxy @(MkTransitCoreTL Nil'))
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
