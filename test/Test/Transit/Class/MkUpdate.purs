module Test.Transit.Class.MkUpdate where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Tuple (Tuple)
import Data.Variant (Variant)
import Transit.Core (Match, MkReturn, MkReturnVia, MkStateGraph, MkTransition, ReturnState, ReturnStateVia, StateGraph)
import Transit.MkUpdate (class MkUpdate)
import Transit.Util (type (:<), Generically)
import Type.Data.List (Nil')

check :: forall @spec @m @impl @msg @state. (MkUpdate spec m impl msg state) => Unit
check = unit

data TestState = TestState1 Int | TestState2 String | TestState3 Boolean

derive instance Generic TestState _

data TestMsg = TestMsg1 Int | TestMsg2 String

derive instance Generic TestMsg _

test1 :: Unit
test1 = check
  @(MkStateGraph Nil')
  @Identity
  @Unit
  @(Generically TestMsg)
  @(Generically TestState)

type MyStateGraph :: StateGraph
type MyStateGraph = MkStateGraph
  ( Nil'
      :< (MkTransition "TestState1" "TestMsg1" (Nil' :< MkReturn "TestState2"))
      :< (MkTransition "TestState2" "TestMsg2" (Nil' :< MkReturnVia "foo" "TestState3" :< MkReturn "TestState1"))
  )

test2 :: Unit
test2 = check
  @MyStateGraph
  @Identity
  @( Tuple
      ( Tuple Unit
          ( Match "TestState1" "TestMsg1" Int Int
              ( Variant
                  ( "TestState2" :: ReturnState String
                  )
              )
          )
      )
      ( Match "TestState2" "TestMsg2" String String
          ( Variant
              ( "TestState1" :: ReturnState Int
              , "TestState3" :: ReturnStateVia "foo" Boolean
              )
          )
      )

  )
  @(Generically TestMsg)
  @(Generically TestState)