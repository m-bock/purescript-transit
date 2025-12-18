module Test.Transit.Class.MatchBySym
  ( spec
  ) where

import Prelude

import Data.Int as Int
import Data.Variant (Variant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.MatchBySym (matchBySym)
import Transit.VariantUtils (v)

type TestVariant = Variant
  ( "Foo" :: Int
  , "Bar" :: String
  , "Baz" :: Unit
  )

spec :: Spec Unit
spec = do
  describe "Transit.Class.MatchBySym" do
    describe "matchBySym" do
      it "matches variant by symbol and extracts payload" do
        matchBySym @"Foo" (\number -> Int.toNumber number) (\_ -> 99.0) (v @"Foo" 42 :: TestVariant)
          `shouldEqual` 42.0

      it "uses default handler when symbol doesn't match" do
        matchBySym @"Foo" (\number -> Int.toNumber number) (\_ -> 99.0) (v @"Bar" "" :: TestVariant)
          `shouldEqual` 99.0

      it "matches variant with unit payload" do
        matchBySym @"Baz" identity (\_ -> unit) (v @"Baz" unit :: TestVariant)
          `shouldEqual` unit
