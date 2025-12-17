module Test.Transit.Class.GetSubset
  ( test1
  , test2
  , spec
  ) where

import Prelude

import Data.Variant (Variant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.GetSubset (class GetSubset, getSubset)
import Transit.Core (MkReturnTL, MkReturnViaTL, ReturnTL, ReturnState(..), ReturnStateVia(..))
import Transit.VariantUtils (v)
import Type.Data.List (type (:>), List', Nil')

check :: forall @syms @t @a. (GetSubset syms t a) => Unit
check = unit

type D = Variant
  ( "Foo" :: Int
  , "Bar" :: String
  , "Baz" :: Unit
  , "Qux" :: Int
  )

--------------------------------------------------------------------------------
-- TL Test 1
--------------------------------------------------------------------------------

type Test1A = Nil' :: List' ReturnTL
type Test1B = D
type Test1C = Variant ()

test1 :: Unit
test1 = check @Test1A @Test1B @Test1C

--------------------------------------------------------------------------------
-- TL Test 2
--------------------------------------------------------------------------------

type Test2A = MkReturnTL "Foo" :> MkReturnViaTL "Tansition" "Bar" :> Nil'
type Test2B = D
type Test2C = Variant
  ( "Foo" :: ReturnState Int
  , "Bar" :: ReturnStateVia "Tansition" String
  )

test2 :: Unit
test2 = check @Test2A @Test2B @Test2C

--------------------------------------------------------------------------------
-- Spec
--------------------------------------------------------------------------------

type L1 = MkReturnTL "Foo" :> MkReturnTL "Baz" :> MkReturnViaTL "Guard1" "Qux" :> Nil'

spec :: Spec Unit
spec = do
  describe "Transit.Class.GetSubset" do
    describe "getSubset" do
      it "should inject whitelisted case with payload" do
        getSubset @L1 (v @"Foo" (ReturnState 1))
          `shouldEqual` (v @"Foo" 1 :: D)

      it "should inject whitelisted case with payload and guard" do
        getSubset @L1 (v @"Qux" (ReturnStateVia @"Guard1" 1))
          `shouldEqual` (v @"Qux" 1 :: D)

      it "should inject whitelisted case with unit payload" do
        getSubset @L1 (v @"Baz" (ReturnState unit))
          `shouldEqual` (v @"Baz" unit :: D)
