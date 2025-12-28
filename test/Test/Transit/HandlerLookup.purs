module Test.Transit.HandlerLookup
  ( spec
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (runFn4)
import Data.Variant (Variant)
import Data.Variant as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.HandlerLookup (HandlerLookup(..), HandlerLookupBuilder(..), addHandler, build, initBuilder, runI, runImpl)
import Transit.VariantUtils (v)
import Type.Proxy (Proxy(..))

type StateRow =
  ( "State1" :: Int
  , "State2" :: String
  )

type MsgRow =
  ( "Msg1" :: Int
  , "Msg2" :: String
  )

type State = Variant StateRow

type Msg = Variant MsgRow

spec :: Spec Unit
spec = do
  describe "Transit.HandlerLookup" do
    describe "initBuilder" do
      it "creates an empty builder" do
        let
          builder :: HandlerLookupBuilder Identity StateRow MsgRow
          builder = initBuilder
          lookup = build builder
          result = runFn4 runImpl runI lookup (v @"State1" 1) (v @"Msg1" 2)
        result `shouldEqual` Identity Nothing

    describe "addHandler and build" do
      it "builds a lookup with a single handler" do
        let
          builder :: HandlerLookupBuilder Identity StateRow MsgRow
          builder = initBuilder

          handler :: Int -> Int -> Identity State
          handler = \_ _ -> pure $ v @"State2" "result"
          builder' = addHandler @"State1" @"Msg1" handler builder
          lookup = build builder'
          result = runFn4 runImpl runI lookup (v @"State1" 1) (v @"Msg1" 2)
        result `shouldEqual` Identity (Just (v @"State2" "result"))

      it "builds a lookup with multiple handlers" do
        let
          builder :: HandlerLookupBuilder Identity StateRow MsgRow
          builder = initBuilder

          handler1 :: Int -> Int -> Identity State
          handler1 = \_ _ -> pure $ v @"State2" "from-state1"

          handler2 :: String -> String -> Identity State
          handler2 = \_ _ -> pure $ v @"State1" 99
          builder' = addHandler @"State1" @"Msg1" handler1
            $ addHandler @"State2" @"Msg2" handler2
            $ builder
          lookup = build builder'
        runFn4 runImpl runI lookup (v @"State1" 1) (v @"Msg1" 2)
          `shouldEqual` Identity (Just (v @"State2" "from-state1"))
        runFn4 runImpl runI lookup (v @"State2" "foo") (v @"Msg2" "bar")
          `shouldEqual` Identity (Just (v @"State1" 99))

    describe "runImpl" do
      let
        handler :: Int -> Int -> Identity State
        handler = \_ _ -> pure $ v @"State2" "success"

        builder :: HandlerLookupBuilder Identity StateRow MsgRow
        builder = addHandler @"State1" @"Msg1" handler initBuilder
        lookup = build builder

      it "returns handler result when handler exists" do
        runFn4 runImpl runI lookup (v @"State1" 1) (v @"Msg1" 2)
          `shouldEqual` Identity (Just (v @"State2" "success"))

      it "returns Nothing when state not found" do
        runFn4 runImpl runI lookup (v @"State2" "foo") (v @"Msg1" 2)
          `shouldEqual` Identity Nothing

      it "returns Nothing when message not found" do
        runFn4 runImpl runI lookup (v @"State1" 1) (v @"Msg2" "bar")
          `shouldEqual` Identity Nothing

