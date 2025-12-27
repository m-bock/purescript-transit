module Test.Transit
  ( spec
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Variant (Variant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Transit, match, mkUpdate, mkUpdateMaybe, mkUpdateMaybeM, mkUpdateM, return)
import Transit.VariantUtils (v)

type TestState = Variant
  ( "State1" :: {}
  , "State2" :: {}
  , "State3" :: {}
  )

type TestMsg = Variant
  ( "Msg1" :: {}
  , "Msg2" :: {}
  )

type TestTransit =
  Transit
    :* ("State1" :@ "Msg1" >| "State2")
    :* ("State2" :@ "Msg2" >| "State3")

spec :: Spec Unit
spec = do
  describe "Transit" do
    describe "mkUpdate" do
      let
        update :: TestState -> TestMsg -> TestState
        update = mkUpdate @TestTransit
          (match @"State1" @"Msg1" \_ _ -> return @"State2")
          (match @"State2" @"Msg2" \_ _ -> return @"State3")

      it "performs valid state transitions" do
        update (v @"State1") (v @"Msg1") `shouldEqual` (v @"State2")

      it "returns unchanged state on invalid transition" do
        update (v @"State1") (v @"Msg2") `shouldEqual` (v @"State1")

    describe "mkUpdateMaybe" do
      let
        update :: TestState -> TestMsg -> Maybe TestState
        update = mkUpdateMaybe @TestTransit
          (match @"State1" @"Msg1" \_ _ -> return @"State2")
          (match @"State2" @"Msg2" \_ _ -> return @"State3")

      it "returns Just for valid transitions" do
        update (v @"State1") (v @"Msg1") `shouldEqual` Just (v @"State2")

      it "returns Nothing for invalid transitions" do
        update (v @"State1") (v @"Msg2") `shouldEqual` Nothing

    describe "mkUpdateM" do
      let
        update :: TestState -> TestMsg -> Identity TestState
        update = mkUpdateM @TestTransit
          (match @"State1" @"Msg1" \_ _ -> return @"State2")
          (match @"State2" @"Msg2" \_ _ -> return @"State3")

      it "performs valid state transitions" do
        update (v @"State1") (v @"Msg1") `shouldEqual` Identity (v @"State2")

      it "returns unchanged state on invalid transition" do
        update (v @"State1") (v @"Msg2") `shouldEqual` Identity (v @"State1")

    describe "mkUpdateEitherM" do
      let
        update :: TestState -> TestMsg -> Identity (Maybe TestState)
        update = mkUpdateMaybeM @TestTransit
          (match @"State1" @"Msg1" \_ _ -> return @"State2")
          (match @"State2" @"Msg2" \_ _ -> return @"State3")

      it "returns Just for valid transitions in Identity monad" do
        update (v @"State1") (v @"Msg1") `shouldEqual` Identity (Just (v @"State2"))

      it "returns Nothing for invalid transitions in Identity monad" do
        update (v @"State1") (v @"Msg2") `shouldEqual` Identity Nothing

