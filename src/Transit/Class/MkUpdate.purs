module Transit.MkUpdate where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Symbol (class IsSymbol)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Safe.Coerce (coerce)
import Transit.Class.MatchBySym (class MatchBySym, matchBySym2)
import Transit.Core (Match(..), MkReturn, MkReturnVia, MkStateGraph, MkTransition, ReturnState, ReturnStateVia, StateGraph)
import Transit.GetSubset (class GetSubset, getSubset)
import Transit.Util (type (:<), Generically(..))
import Type.Data.List (type (:>), List', Nil')
import Type.Prelude (Proxy(..))
import Type.Proxy (Proxy)

class MkUpdate (spec :: StateGraph) impl msg state | spec msg state -> impl where
  mkUpdate :: impl -> msg -> state -> state

instance MkUpdate (MkStateGraph Nil') Unit msg state where
  mkUpdate _ _ state = state

instance
  ( MatchBySym symStateIn state stateIn
  , GetSubset returns state stateOut
  , MatchBySym symMsg msg msgIn
  , MkUpdate (MkStateGraph rest1) rest2 msg state
  ) =>
  MkUpdate
    (MkStateGraph (rest1 :< (MkTransition symStateIn symMsg returns)))
    (rest2 /\ Match symStateIn symMsg msgIn stateIn stateOut)
    msg
    state
  where
  mkUpdate (rest /\ Match fn) msg state =
    matchBySym2 @symMsg @symStateIn
      (\m s -> getSubset @returns $ fn m s)
      (\_ -> mkUpdate @(MkStateGraph rest1) rest msg state)
      msg
      state

checkMkUpdate :: forall spec impl msg state. MkUpdate spec impl msg state => Proxy spec -> Proxy impl -> Proxy msg -> Proxy state -> Unit
checkMkUpdate _ _ _ _ = unit

data TestState = TestState1 Int | TestState2 String | TestState3 Boolean

derive instance Generic TestState _

data TestMsg = TestMsg1 Int | TestMsg2 String

derive instance Generic TestMsg _

test1 :: Unit
test1 = checkMkUpdate
  (Proxy :: _ (MkStateGraph Nil'))
  (Proxy :: _ Unit)
  (Proxy :: _ (Generically TestMsg))
  (Proxy :: _ (Generically TestState))

type MyStateGraph :: StateGraph
type MyStateGraph = MkStateGraph
  ( Nil'
      :< (MkTransition "TestState1" "TestMsg1" (Nil' :< MkReturn "TestState2"))
      :< (MkTransition "TestState2" "TestMsg2" (Nil' :< MkReturnVia "foo" "TestState3" :< MkReturn "TestState1"))
  )

test2 :: Unit
test2 = checkMkUpdate
  (Proxy :: _ MyStateGraph)
  ( Proxy
      :: _
           ( Tuple
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
  )
  (Proxy :: _ (Generically TestMsg))
  (Proxy :: _ (Generically TestState))