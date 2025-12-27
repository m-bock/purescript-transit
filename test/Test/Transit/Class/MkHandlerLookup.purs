module Test.Transit.Class.MkHandlerLookup
  ( spec
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Function.Uncurried (runFn4)
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.MkHandlerLookup (mkHandlerLookup)
import Transit.Core (MatchImpl(..), MkMatchTL, MkReturnTL, Ret(..))
import Transit.HandlerLookup (HandlerLookupBuilder, build, runI, runImpl)
import Transit.VariantUtils (v)
import Type.Data.List (type (:>), List', Nil')
import Type.Proxy (Proxy(..))

type StateRow =
  ( "State1" :: Int
  , "State2" :: String
  )

type MsgRow =
  ( "Msg1" :: Int
  , "Msg2" :: String
  )

type State = Variant StateRow

type Msg = Variant MsgRow

type SingleMatchSpec = MkMatchTL "State1" "Msg1" (MkReturnTL "State2" :> Nil') :> Nil'

type MultipleMatchSpec = MkMatchTL "State1" "Msg1" (MkReturnTL "State2" :> Nil')
  :> MkMatchTL "State2" "Msg2" (MkReturnTL "State1" :> Nil')
  :> Nil'

spec :: Spec Unit
spec = do
  describe "Transit.Class.MkHandlerLookup" do
    describe "mkHandlerLookup" do
      it "builds lookup from single match" do
        let
          handler = MatchImpl @"State1" @"Msg1" \_ _ -> Identity $ V.inj (Proxy @"State2") (Ret "result")
          builder :: HandlerLookupBuilder Identity StateRow MsgRow
          builder = mkHandlerLookup @Identity @SingleMatchSpec (handler /\ unit)
          lookup = build builder
          result = runFn4 runImpl runI lookup (v @"State1" 1) (v @"Msg1" 2)
        result `shouldEqual` Identity (Just (v @"State2" "result"))

      it "builds lookup from multiple matches" do
        let
          handler1 = MatchImpl @"State1" @"Msg1" \_ _ -> Identity $ V.inj (Proxy @"State2") (Ret "from-state1")
          handler2 = MatchImpl @"State2" @"Msg2" \_ _ -> Identity $ V.inj (Proxy @"State1") (Ret 99)
          builder :: HandlerLookupBuilder Identity StateRow MsgRow
          builder = mkHandlerLookup @Identity @MultipleMatchSpec (handler1 /\ handler2 /\ unit)
          lookup = build builder
        runFn4 runImpl runI lookup (v @"State1" 1) (v @"Msg1" 2)
          `shouldEqual` Identity (Just (v @"State2" "from-state1"))
        runFn4 runImpl runI lookup (v @"State2" "foo") (v @"Msg2" "bar")
          `shouldEqual` Identity (Just (v @"State1" 99))

