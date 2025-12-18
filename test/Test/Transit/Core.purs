module Test.Transit.Core
  ( spec
  ) where

import Prelude

import Data.Array as Array
import Data.Reflectable (reflectType)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Core (Match(..), MkMatchTL, MkReturnTL, MkReturnViaTL, MkTransitCoreTL, Return(..), TransitCore(..), TransitCoreTL, getMatchesForState, getStateNames)
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..))

type TestStateGraph :: TransitCoreTL
type TestStateGraph = MkTransitCoreTL
  ( (MkMatchTL "State1" "Msg1" (MkReturnTL "State2" :> Nil'))
      :> (MkMatchTL "State2" "Msg2" (MkReturnTL "State3" :> Nil'))
      :> Nil'
  )

type TestStateGraphWithGuards :: TransitCoreTL
type TestStateGraphWithGuards = MkTransitCoreTL
  ( ( MkMatchTL "State1" "Msg1"
        ( MkReturnTL "State2"
            :> MkReturnViaTL "Guard1" "State3"
            :> Nil'
        )
    )
      :> Nil'
  )

spec :: Spec Unit
spec = do
  describe "Transit.Core" do
    describe "MkTransitCoreTL reflection" do
      it "reflects empty transit core" do
        let result = reflectType (Proxy @(MkTransitCoreTL Nil'))
        result `shouldEqual` (TransitCore [])

      it "reflects transit core with multiple matches" do
        let result = reflectType (Proxy @TestStateGraph)
        result `shouldEqual`
          ( TransitCore
              [ Match "State1" "Msg1" [ Return "State2" ]
              , Match "State2" "Msg2" [ Return "State3" ]
              ]
          )

      it "reflects transit core with guards (ReturnVia)" do
        let result = reflectType (Proxy @TestStateGraphWithGuards)
        result `shouldEqual`
          ( TransitCore
              [ Match "State1" "Msg1"
                  [ Return "State2"
                  , ReturnVia "Guard1" "State3"
                  ]
              ]
          )

    describe "getStateNames" do
      it "returns empty array for empty transit core" do
        let transitCore = TransitCore []
        getStateNames transitCore `shouldEqual` []

      it "returns all unique state names from matches" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            , Match "State2" "Msg2" [ Return "State3" ]
            , Match "State1" "Msg3" [ Return "State1" ]
            ]
        let stateNames = getStateNames transitCore
        Array.length stateNames `shouldEqual` 3
        Array.elem "State1" stateNames `shouldEqual` true
        Array.elem "State2" stateNames `shouldEqual` true
        Array.elem "State3" stateNames `shouldEqual` true

      it "includes states from ReturnVia returns" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1"
                [ Return "State2"
                , ReturnVia "Guard1" "State3"
                ]
            ]
        let stateNames = getStateNames transitCore
        Array.length stateNames `shouldEqual` 3
        Array.elem "State1" stateNames `shouldEqual` true
        Array.elem "State2" stateNames `shouldEqual` true
        Array.elem "State3" stateNames `shouldEqual` true

    describe "getMatchesForState" do
      it "returns empty array when state has no matches" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            ]
        getMatchesForState "State3" transitCore `shouldEqual` []

      it "returns all matches for a given state" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            , Match "State1" "Msg2" [ Return "State3" ]
            , Match "State2" "Msg3" [ Return "State1" ]
            ]
        let matches = getMatchesForState "State1" transitCore
        Array.length matches `shouldEqual` 2
        Array.elem (Match "State1" "Msg1" [ Return "State2" ]) matches `shouldEqual` true
        Array.elem (Match "State1" "Msg2" [ Return "State3" ]) matches `shouldEqual` true

      it "returns matches with guards" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1"
                [ Return "State2"
                , ReturnVia "Guard1" "State3"
                ]
            ]
        let matches = getMatchesForState "State1" transitCore
        Array.length matches `shouldEqual` 1
        case Array.head matches of
          Just (Match "State1" "Msg1" returns) -> do
            Array.length returns `shouldEqual` 2
            Array.elem (Return "State2") returns `shouldEqual` true
            Array.elem (ReturnVia "Guard1" "State3") returns `shouldEqual` true
          _ -> false `shouldEqual` true
