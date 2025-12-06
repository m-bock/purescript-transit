module Test.Transit.Class.MkUpdate where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Transit.Core (MatchImpl, MkMatch, MkReturn, MkReturnVia, MkTransitCore, ReturnState, ReturnStateVia, TransitCore)
import Transit.MkUpdate (class MkUpdate)
import Transit.Util (Generically)
import Type.Data.List (type (:>), Nil')

check :: forall @spec @m @impl @msg @state. (MkUpdate spec m impl msg state) => Unit
check = unit

data TestState = TestState1 Int | TestState2 String | TestState3 Boolean

derive instance Generic TestState _

data TestMsg = TestMsg1 Int | TestMsg2 String

derive instance Generic TestMsg _

test1 :: Unit
test1 = check
  @(MkTransitCore Nil')
  @Identity
  @Unit
  @(Generically TestMsg)
  @(Generically TestState)

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

test2 :: Unit
test2 = check
  @MyStateGraph
  @Identity
  @T
  @(Generically TestMsg)
  @(Generically TestState)