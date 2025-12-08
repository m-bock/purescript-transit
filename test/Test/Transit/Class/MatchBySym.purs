module Test.Transit.Class.MatchBySym
  ( test1
  , test2
  , test3
  , spec
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.MatchBySym (class MatchBySym, matchBySym)
import Transit.Util (Generically(..))

check :: forall @sym @a @b. MatchBySym sym a b => Unit
check = unit

data Test = Foo Int | Bar String | Baz

derive instance Generic Test _

--------------------------------------------------------------------------------
-- Test 1
--------------------------------------------------------------------------------

test1 :: Unit
test1 = check @"Foo" @(Generically Test) @Int

test2 :: Unit
test2 = check @"Bar" @(Generically Test) @String

test3 :: Unit
test3 = check @"Baz" @(Generically Test) @Unit

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "Transit.Class.MatchBySym" do
    describe "matchBySym" do
      it "should match by symbol" do
        matchBySym @"Foo" (\n -> Int.toNumber n) (\_ -> 99.0) (Generically (Foo 42)) `shouldEqual` 42.0
      it "should match by symbol with default" do
        matchBySym @"Foo" (\n -> Int.toNumber n) (\_ -> 99.0) (Generically (Bar "")) `shouldEqual` 99.0
      it "should match by symbol with unit payload" do
        matchBySym @"Baz" (\u -> u) (\_ -> unit) (Generically Baz) `shouldEqual` unit
