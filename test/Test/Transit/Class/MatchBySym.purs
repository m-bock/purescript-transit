module Test.Transit.Class.MatchBySym
  ( test1
  , test2
  , test3
  , spec
  ) where

import Prelude

import Data.Int as Int
import Data.Variant (Variant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.MatchBySym (class MatchBySym, matchBySym)
import Transit.VariantUtils (v)

check :: forall @sym @a @b. MatchBySym sym a b => Unit
check = unit

--------------------------------------------------------------------------------
-- Test 1
--------------------------------------------------------------------------------

type Test = Variant
  ( "Foo" :: Int
  , "Bar" :: String
  , "Baz" :: Unit
  )

test1 :: Unit
test1 = check @"Foo" @Test @Int

test2 :: Unit
test2 = check @"Bar" @Test @String

test3 :: Unit
test3 = check @"Baz" @Test @Unit

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "Transit.Class.MatchBySym" do
    describe "matchBySym" do
      it "should match by symbol" do
        matchBySym @"Foo" (\n -> Int.toNumber n) (\_ -> 99.0) (v @"Foo" 42 :: Test) `shouldEqual` 42.0
      it "should match by symbol with default" do
        matchBySym @"Foo" (\n -> Int.toNumber n) (\_ -> 99.0) (v @"Bar" "" :: Test) `shouldEqual` 99.0
      it "should match by symbol with unit payload" do
        matchBySym @"Baz" (\u -> u) (\_ -> unit) (v @"Baz" unit :: Test) `shouldEqual` unit
