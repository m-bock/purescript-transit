module Test.Transit.Class.CurryN
  ( spec
  ) where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.CurryN (curryN)

spec :: Spec Unit
spec = do
  describe "Transit.Class.CurryN" do
    describe "curryN with Unit" do
      it "curries function with no arguments" do
        let
          f :: Unit -> Int
          f _ = 42

          result :: Int
          result = curryN f
        result `shouldEqual` 42

    describe "curryN with single argument" do
      it "curries function with one argument" do
        let
          f :: Int /\ Unit -> Int
          f (x /\ _) = x * 2

          curried :: Int -> Int
          curried = curryN f
        curried 5 `shouldEqual` 10

    describe "curryN with two arguments" do
      it "curries function with two arguments" do
        let
          f :: Int /\ String /\ Unit -> String
          f (x /\ s /\ _) = s <> show x

          curried :: Int -> String -> String
          curried = curryN f
        curried 42 "answer: " `shouldEqual` "answer: 42"

    describe "curryN with three arguments" do
      it "curries function with three arguments" do
        let
          f :: Int /\ String /\ Boolean /\ Unit -> String
          f (x /\ s /\ b /\ _) = s <> show x <> " " <> show b

          curried :: Int -> String -> Boolean -> String
          curried = curryN f
        curried 10 "value: " true `shouldEqual` "value: 10 true"

