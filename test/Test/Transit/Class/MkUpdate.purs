module Test.Transit.Class.MkUpdate
  ( check
  , test1
  --, test2
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Class.MkUpdate (class MkUpdate, mkUpdate)
import Transit.Core (MatchImpl(..), MkMatch, MkReturn, MkReturnVia, MkTransitCore, ReturnState(..), ReturnStateVia, TransitCore)
import Transit.Util (Generically)
import Type.Data.List (type (:>), Nil')

check :: forall @spec @m @impl @msg @state. (MkUpdate spec m impl msg state) => Unit
check = unit

--------------------------------------------------------------------------------
-- Test 1
--------------------------------------------------------------------------------

data Test1State = Test1StateA Int | Test1StateB String | Test1StateC Boolean
data Test1Msg = Test1MsgA Int | Test1MsgB String

derive instance Generic Test1State _
derive instance Generic Test1Msg _

test1 :: Unit
test1 = check
  @(MkTransitCore Nil')
  @Identity
  @Unit
  @(Generically Test1Msg)
  @(Generically Test1State)

--------------------------------------------------------------------------------
-- Test 2
--------------------------------------------------------------------------------

type MyStateGraph :: TransitCore
type MyStateGraph = MkTransitCore
  ( (MkMatch "TestState1" "TestMsg1" (MkReturn "TestState2" :> Nil'))
      :> (MkMatch "TestState2" "TestMsg2" (MkReturnVia "foo" "TestState3" :> MkReturn "TestState1" :> Nil'))
      :> Nil'
  )

type T1 = MatchImpl "TestState1" "TestMsg1" Int Int
  ( Variant
      ( "TestState2" :: ReturnState String
      )
  )

type T2 = MatchImpl "TestState2" "TestMsg2" String String
  ( Variant
      ( "TestState1" :: ReturnState Int
      , "TestState3" :: ReturnStateVia "foo" Boolean
      )
  )

type T = T1 /\ T2 /\ Unit

-- test2 :: Unit
-- test2 = check
--   @MyStateGraph
--   @Identity
--   @T
--   @(Generically TestMsg)
--   @(Generically TestState)

-- data Msg = Msg1 | Msg2

-- data State = State1 | State2

-- derive instance Generic Msg _
-- derive instance Generic State _

-- spec :: Spec Unit
-- spec = do
--   describe "Transit.Class.MkUpdate" do
--     describe "mkUpdate" do
--       it "should return the correct state" do
--         let
--           update :: Generically State -> Generically Msg -> Identity (Generically State)
--           update = mkUpdate @MyStateGraph @Identity
--             ( (MatchImpl @"State1" @"Msg1" \_ _ -> ReturnState "State2")
--                 /\ (MatchImpl @"State2" @"Msg2" \_ _ -> ReturnState "State1")
--                 /\ unit
--             )
--         1 `shouldEqual` 2