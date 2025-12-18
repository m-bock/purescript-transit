module Test.Transit.Class.ExpandReturn
  ( spec
  ) where

import Prelude

import Data.Variant (Variant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.ExpandReturn (expandReturn)
import Transit.Core (MkReturnTL, MkReturnViaTL, Ret(..), RetVia(..))
import Transit.VariantUtils (v)
import Type.Data.List (type (:>), Nil')

type TestVariant = Variant
  ( "Foo" :: Int
  , "Bar" :: String
  , "Baz" :: Unit
  , "Qux" :: Int
  )

type TestReturnList =
  MkReturnTL "Foo"
    :> MkReturnTL "Baz"
    :> MkReturnViaTL "Guard1" "Qux"
    :> Nil'

spec :: Spec Unit
spec = do
  describe "Transit.Class.ExpandReturn" do
    describe "expandReturn" do
      it "expands variant with Ret wrapper and payload" do
        expandReturn @TestReturnList (v @"Foo" (Ret 1))
          `shouldEqual` (v @"Foo" 1 :: TestVariant)

      it "expands variant with RetVia wrapper, guard, and payload" do
        expandReturn @TestReturnList (v @"Qux" (RetVia @"Guard1" 1))
          `shouldEqual` (v @"Qux" 1 :: TestVariant)

      it "expands variant with Ret wrapper and unit payload" do
        expandReturn @TestReturnList (v @"Baz" (Ret unit))
          `shouldEqual` (v @"Baz" unit :: TestVariant)

