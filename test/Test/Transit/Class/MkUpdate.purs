module Test.Transit.Class.MkUpdate
  ( spec
  --, test1
  --, test2
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Data.Tuple.Nested ((/\))
import Data.Variant (Variant)
import Data.Variant as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.MkUpdate (class MkUpdate, TransitError, mkUpdate)
import Transit.Core (MatchImpl(..), MkMatchTL, MkReturnTL, MkTransitCoreTL, ReturnState(..), TransitCoreTL)
import Transit.VariantUtils (inj)
import Type.Data.List (type (:>), Nil')
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

check :: forall @spec @m @impl @msg @state. (MkUpdate spec m impl msg state) => Unit
check = unit

--------------------------------------------------------------------------------
-- Test 1
--------------------------------------------------------------------------------

-- data Test1State
--   = Test1StateA Int
--   | Test1StateB String
--   | Test1StateC Boolean

-- data Test1Msg = Test1MsgA Int | Test1MsgB String

-- derive instance Generic Test1State _
-- derive instance Generic Test1Msg _

-- test1 :: Unit
-- test1 = check
--   @(MkTransitCore Nil')
--   @Identity
--   @Unit
--   @(Generically Test1Msg)
--   @(Generically Test1State)

--------------------------------------------------------------------------------
-- Test 2
--------------------------------------------------------------------------------

-- type MyStateGraph :: TransitCore
-- type MyStateGraph = MkTransitCore
--   ( (MkMatch "TestState1" "TestMsg1" (MkReturn "TestState2" :> Nil'))
--       :> (MkMatch "TestState2" "TestMsg2" (MkReturnVia "foo" "TestState3" :> MkReturn "TestState1" :> Nil'))
--       :> Nil'
--   )

-- type T1 = MatchImpl "TestState1" "TestMsg1" Int Int
--   ( Variant
--       ( "TestState2" :: ReturnState String
--       )
--   )

-- type T2 = MatchImpl "TestState2" "TestMsg2" String String
--   ( Variant
--       ( "TestState1" :: ReturnState Int
--       , "TestState3" :: ReturnStateVia "foo" Boolean
--       )
--   )

-- type T = T1 /\ T2 /\ Unit

-- test2 :: Unit
-- test2 = check
--   @MyStateGraph
--   @Identity
--   @T
--   @(Generically TestMsg)
--   @(Generically TestState)

------------------------------------------------------------------------------
-- Spec
------------------------------------------------------------------------------

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

type MyStateGraph :: TransitCoreTL
type MyStateGraph = MkTransitCoreTL
  ( Id $ (MkMatchTL "State1" "Msg1" (MkReturnTL "State2" :> Nil'))
      :> (MkMatchTL "State2" "Msg2" (MkReturnTL "State1" :> Nil'))
      :> Nil'
  )

spec :: Spec Unit
spec = do
  describe "Transit.Class.MkUpdate" do
    describe "mkUpdate" do
      let
        update :: State -> Msg -> Identity (Either (TransitError State Msg) State)
        update = mkUpdate @MyStateGraph @Identity
          ( iden (MatchImpl @"State1" @"Msg1" \_ _ -> pure $ V.inj (Proxy @"State2") (ReturnState "42"))
              /\ (MatchImpl @"State2" @"Msg2" \_ _ -> pure $ V.inj (Proxy @"State1") (ReturnState 99))
              /\ unit
          )

      it "perform state updates on legal transitions" do

        update (inj @"State1" 1) (inj @"Msg1" 2)
          `shouldEqual` Identity (Right (inj @"State2" "foo"))

        update (inj @"State2" "foo") (inj @"Msg2" "bar")
          `shouldEqual` Identity (Right (inj @"State1" 99))

        update (inj @"State3" {}) (inj @"Msg3" {})
          `shouldEqual` Identity (Right (inj @"State3" {}))

      it "should leave the state unchanged on illegal transitions" do
        update (inj @"State1" 1) (inj @"Msg3" {})
          `shouldEqual` Identity (Right (inj @"State1" 1))

        update (inj @"State2" "foo") (inj @"Msg1" 2)
          `shouldEqual` Identity (Right (inj @"State2" "foo"))

        update (inj @"State3" {}) (inj @"Msg1" 2)
          `shouldEqual` Identity (Right (inj @"State3" {}))

        update (inj @"State3" {}) (inj @"Msg2" "bar")
          `shouldEqual` Identity (Right (inj @"State3" {}))

type Id :: forall k. k -> k
type Id a = a

iden :: forall a. a -> a
iden = identity