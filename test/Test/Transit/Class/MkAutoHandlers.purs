module Test.Transit.Class.MkAutoHandlers (spec) where

import Prelude

import Data.Identity (Identity(..))
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.MkAutoHandlers (mkAutoHandlers)
import Transit.Core (MatchImpl(..))
import Type.Proxy (Proxy(..))

data A = A
data B = B
data C = C

derive instance Eq A
derive instance Eq B
derive instance Eq C

instance showA :: Show A where
  show A = "A"

instance showB :: Show B where
  show B = "B"

instance showC :: Show C where
  show C = "C"

type T1 =
  MatchImpl
    "State1"
    "Msg1"
    A
    B
    Identity
    (Variant ("State2" :: A))

type T2 =
  MatchImpl
    "StateA"
    "MsgA"
    C
    A
    Identity
    (Variant ("StateB" :: C))

spec :: Spec Unit
spec = do
  describe "Transit.Class.MkAutoHandlers" do
    describe "mkAutoHandlers" do
      it "builds a lookup from a single match" do
        let
          handlers :: T1 /\ T2 /\ Unit
          handlers = mkAutoHandlers

          (MatchImpl fn1 /\ MatchImpl fn2 /\ end) = handlers

          result1 = fn1 A B
          result2 = fn2 C A

        result1 `shouldEqual` Identity (V.inj (Proxy @"State2") A)
        result2 `shouldEqual` Identity (V.inj (Proxy @"StateB") C)
        end `shouldEqual` unit