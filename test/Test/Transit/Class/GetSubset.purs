module Test.Transit.Class.GetSubset
  ( test1
  , test2
  , spec
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import Data.Variant as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.GetSubset (class GetSubset, getSubset)
import Transit.Core (MkReturnTL, MkReturnViaTL, ReturnTL, ReturnState(..), ReturnStateVia(..))
import Transit.Util (Generically(..))
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))

check :: forall @syms @t @a. (GetSubset syms t a) => Unit
check = unit

data D
  = Foo Int
  | Bar String
  | Baz
  | Qux Int

derive instance Generic D _

derive instance Eq D

--------------------------------------------------------------------------------
-- TL Test 1
--------------------------------------------------------------------------------

instance Show D where
  show = genericShow

type Test1A = Nil' :: List' ReturnTL
type Test1B = Generically D
type Test1C = Variant ()

test1 :: Unit
test1 = check @Test1A @Test1B @Test1C

--------------------------------------------------------------------------------
-- TL Test 2
--------------------------------------------------------------------------------

type Test2A = MkReturnTL "Foo" :> MkReturnViaTL "Tansition" "Bar" :> Nil'
type Test2B = Generically D
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
        getSubset @L1 (V.inj (Proxy @"Foo") (ReturnState 1))
          `shouldEqual` (Generically (Foo 1))

      it "should inject whitelisted case with payload and guard" do
        getSubset @L1 (V.inj (Proxy @"Qux") (ReturnStateVia @"Guard1" 1))
          `shouldEqual` (Generically (Qux 1))

      it "should inject whitelisted case with unit payload" do
        getSubset @L1 (V.inj (Proxy @"Baz") (ReturnState unit))
          `shouldEqual` (Generically Baz)
