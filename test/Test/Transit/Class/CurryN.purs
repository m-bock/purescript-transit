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
    it "curries function with no arguments" do
      let
        fn :: Unit -> Int
        fn _ = 42

        curriedFn :: Int
        curriedFn = curryN fn
      curriedFn `shouldEqual` 42

    it "curries function with one argument" do
      let
        fn :: Int /\ Unit -> Int
        fn (x /\ _) = x * 2

        curriedFn :: Int -> Int
        curriedFn = curryN fn
      curriedFn 5 `shouldEqual` 10

    it "curries function with two arguments" do
      let
        fn :: Int /\ Number /\ Unit -> String
        fn (number /\ prefix /\ _) = show number <> " " <> show prefix

        curriedFn :: Int -> Number -> String
        curriedFn = curryN fn
      curriedFn 42 3.14 `shouldEqual` "42 3.14"

    it "curries function with three arguments" do
      let
        fn :: Int /\ Number /\ Boolean /\ Unit -> String
        fn (arg1 /\ arg2 /\ arg3 /\ _) = show arg1 <> " " <> show arg2 <> " " <> show arg3

        curriedFn :: Int -> Number -> Boolean -> String
        curriedFn = curryN fn
      curriedFn 10 3.14 true `shouldEqual` "10 3.14 true"
