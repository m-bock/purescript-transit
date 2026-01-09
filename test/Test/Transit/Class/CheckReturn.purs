module Test.Transit.Class.CheckReturn
  ( spec
  ) where

import Prelude

import Data.Variant (Variant)
import Data.Variant as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.CheckReturn (checkReturn)
import Transit.Core (MkReturnTL, MkReturnViaTL, ViaGuard(..), ReturnTL)
import Transit.VariantUtils (v)
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))

type ReturnsRet :: List' ReturnTL
type ReturnsRet = MkReturnTL "State1" :> MkReturnTL "State2" :> Nil'

type ReturnsViaGuard :: List' ReturnTL
type ReturnsViaGuard = MkReturnViaTL "Guard1" "State1" :> MkReturnViaTL "Guard2" "State2" :> Nil'

type ReturnsMixed :: List' ReturnTL
type ReturnsMixed = MkReturnTL "State1" :> MkReturnViaTL "Guard1" "State2" :> Nil'

type StateInRet = Variant
  ( "State1" :: Int
  , "State2" :: String
  )

type StateOutRet = Variant
  ( "State1" :: Int
  , "State2" :: String
  )

type StateInViaGuard = Variant
  ( "State1" :: ViaGuard "Guard1" Int
  , "State2" :: ViaGuard "Guard2" String
  )

type StateOutViaGuard = Variant
  ( "State1" :: Int
  , "State2" :: String
  )

type StateInMixed = Variant
  ( "State1" :: Int
  , "State2" :: ViaGuard "Guard1" String
  )

type StateOutMixed = Variant
  ( "State1" :: Int
  , "State2" :: String
  )

spec :: Spec Unit
spec = do
  describe "Transit.Class.CheckReturn" do
    it "unwraps Ret wrappers" do
      let
        checkRet :: StateInRet -> StateOutRet
        checkRet = checkReturn @ReturnsRet
        input = V.inj (Proxy @"State1") 42
      checkRet input `shouldEqual` (v @"State1" 42)

    it "unwraps ViaGuard wrappers" do
      let
        checkViaGuard :: StateInViaGuard -> StateOutViaGuard
        checkViaGuard = checkReturn @ReturnsViaGuard
        input = V.inj (Proxy @"State1") (ViaGuard @"Guard1" 42)
      checkViaGuard input `shouldEqual` (v @"State1" 42)

    it "unwraps mixed Ret and ViaGuard wrappers" do
      let
        checkMixed :: StateInMixed -> StateOutMixed
        checkMixed = checkReturn @ReturnsMixed
        input1 = V.inj (Proxy @"State1") 42
        input2 = V.inj (Proxy @"State2") (ViaGuard @"Guard1" "hello")
      checkMixed input1 `shouldEqual` (v @"State1" 42)
      checkMixed input2 `shouldEqual` (v @"State2" "hello")