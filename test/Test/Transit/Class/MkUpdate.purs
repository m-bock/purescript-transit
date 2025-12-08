module Test.Transit.Class.MkUpdate
  ( spec
  --, test1
  --, test2
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Identity (Identity(..))
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.MkUpdate (class MkUpdate, mkUpdate)
import Transit.Core (MatchImpl(..), MkMatchTL, MkReturnTL, MkReturnViaTL, MkTransitCoreTL, ReturnState(..), ReturnStateVia, TransitCoreTL)
import Transit.Util (Generically(..))
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

data State = State1 Int | State2 String | State3

data Msg = Msg1 Int | Msg2 String | Msg3

derive instance Generic State _
derive instance Generic Msg _

derive instance Eq State
derive instance Eq Msg

instance Show State where
  show = genericShow

instance Show Msg where
  show = genericShow

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
        update :: Generically State -> Generically Msg -> Identity (Generically State)
        update = mkUpdate @MyStateGraph @Identity
          ( iden (MatchImpl @"State1" @"Msg1" \_ _ -> V.inj (Proxy @"State2") (ReturnState "42"))
              /\ (MatchImpl @"State2" @"Msg2" \_ _ -> V.inj (Proxy @"State1") (ReturnState 99))
              /\ unit
          )

      it "perform state updates on legal transitions" do

        update (Generically (State1 1)) (Generically (Msg1 2))
          `shouldEqual` Identity (Generically (State2 "42"))

        update (Generically (State2 "foo")) (Generically (Msg2 "bar"))
          `shouldEqual` Identity (Generically (State1 99))

        update (Generically State3) (Generically Msg3)
          `shouldEqual` Identity (Generically State3)

      it "should leave the state unchanged on illegal transitions" do
        update (Generically (State1 1)) (Generically Msg3)
          `shouldEqual` Identity (Generically (State1 1))

        update (Generically (State2 "foo")) (Generically (Msg1 2))
          `shouldEqual` Identity (Generically (State2 "foo"))

        update (Generically State3) (Generically (Msg1 2))
          `shouldEqual` Identity (Generically State3)

        update (Generically State3) (Generically (Msg2 "bar"))
          `shouldEqual` Identity (Generically State3)

type Id a = a

iden :: forall a. a -> a
iden = identity