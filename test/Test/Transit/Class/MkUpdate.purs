module Test.Transit.Class.MkUpdate
  ( spec
  ) where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.MkUpdate (mkUpdateCore)
import Transit.Core (MatchImpl(..), MkMatchTL, MkReturnTL, MkTransitCoreTL, Ret(..), TransitCoreTL)
import Transit.VariantUtils (v)
import Type.Data.List (type (:>), Nil')
import Type.Proxy (Proxy(..))

type State = Variant
  ( "State1" :: Int
  , "State2" :: String
  , "State3" :: {}
  )

type Msg = Variant
  ( "Msg1" :: Int
  , "Msg2" :: String
  , "Msg3" :: {}
  )

type TestStateGraph :: TransitCoreTL
type TestStateGraph = MkTransitCoreTL
  ( (MkMatchTL "State1" "Msg1" (MkReturnTL "State2" :> Nil'))
      :> (MkMatchTL "State2" "Msg2" (MkReturnTL "State1" :> Nil'))
      :> Nil'
  )

spec :: Spec Unit
spec = do
  describe "Transit.Class.MkUpdate" do
    describe "mkUpdateCore" do
      let
        update :: State -> Msg -> Identity (Maybe State)
        update = mkUpdateCore @TestStateGraph @Identity
          ( (MatchImpl @"State1" @"Msg1" \_ _ -> pure $ V.inj (Proxy @"State2") (Ret "42"))
              /\ (MatchImpl @"State2" @"Msg2" \_ _ -> pure $ V.inj (Proxy @"State1") (Ret 99))
              /\ unit
          )

      it "performs state updates on legal transitions" do
        update (v @"State1" 1) (v @"Msg1" 2)
          `shouldEqual` Identity (Just (v @"State2" "42"))

        update (v @"State2" "foo") (v @"Msg2" "bar")
          `shouldEqual` Identity (Just (v @"State1" 99))

      it "returns Nothing on illegal transitions" do
        update (v @"State3" {}) (v @"Msg3" {})
          `shouldEqual` Identity Nothing

        update (v @"State1" 1) (v @"Msg3" {})
          `shouldEqual` Identity Nothing

        update (v @"State2" "foo") (v @"Msg1" 2)
          `shouldEqual` Identity Nothing

        update (v @"State3" {}) (v @"Msg1" 2)
          `shouldEqual` Identity Nothing

        update (v @"State3" {}) (v @"Msg2" "bar")
          `shouldEqual` Identity Nothing