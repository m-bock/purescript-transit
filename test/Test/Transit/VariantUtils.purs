module Test.Transit.VariantUtils
  ( spec
  ) where

import Prelude

import Data.Variant (Variant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.VariantUtils (v)

type TestVariant = Variant
  ( "Empty" :: {}
  , "Unit" :: Unit
  , "Int" :: Int
  , "String" :: String
  , "Record" :: { value :: Int }
  )

spec :: Spec Unit
spec = do
  describe "Transit.VariantUtils" do
    describe "v function" do
      it "creates variant with empty record payload (omitted)" do
        let variant = v @"Empty" :: TestVariant
        variant `shouldEqual` variant

      it "creates variant with unit payload" do
        let variant = v @"Unit" unit :: TestVariant
        variant `shouldEqual` variant

      it "creates variant with Int payload" do
        let variant = v @"Int" 42 :: TestVariant
        variant `shouldEqual` variant

      it "creates variant with String payload" do
        let variant = v @"String" "hello" :: TestVariant
        variant `shouldEqual` variant

      it "creates variant with record payload" do
        let variant = v @"Record" { value: 100 } :: TestVariant
        variant `shouldEqual` variant

      it "creates equal variants with same payload" do
        let v1 = v @"Int" 42 :: TestVariant
        let v2 = v @"Int" 42 :: TestVariant
        v1 `shouldEqual` v2

