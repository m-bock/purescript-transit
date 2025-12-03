module Test.Example where

import Prelude

import Effect (Effect)
import Transit (type (:*), type (:@), Empty, Wrap)
import Transit.DSL (type (>|))
import Transit.Gen.Graphviz as TransitGraphviz
import Type.Function (type ($))

type ColorTest = Wrap $ Empty
  :* ("StateA" :@ "MsgA" >| "StateB")
  :* ("StateB" :@ "MsgB" >| "StateC")
  :* ("StateC" :@ "MsgC" >| "StateD")
  :* ("StateD" :@ "MsgD" >| "StateE")
  :* ("StateE" :@ "MsgE" >| "StateF")
  :* ("StateF" :@ "MsgF" >| "StateG")
  :* ("StateG" :@ "MsgG" >| "StateH")
  :* ("StateH" :@ "MsgH" >| "StateI")
  :* ("StateI" :@ "MsgI" >| "StateJ")
  :* ("StateJ" :@ "MsgJ" >| "StateK")
  :* ("StateK" :@ "MsgK" >| "StateL")
  :* ("StateL" :@ "MsgL" >| "StateM")
  :* ("StateM" :@ "MsgM" >| "StateN")
  :* ("StateN" :@ "MsgN" >| "StateO")
  :* ("StateO" :@ "MsgO" >| "StateP")
  :* ("StateP" :@ "MsgP" >| "StateQ")
  :* ("StateQ" :@ "MsgQ" >| "StateR")
  :* ("StateR" :@ "MsgR" >| "StateS")
  :* ("StateS" :@ "MsgS" >| "StateT")
  :* ("StateT" :@ "MsgT" >| "StateU")
  :* ("StateU" :@ "MsgU" >| "StateV")
  :* ("StateV" :@ "MsgV" >| "StateW")
  :* ("StateW" :@ "MsgW" >| "StateX")
  :* ("StateX" :@ "MsgX" >| "StateY")
  :* ("StateY" :@ "MsgY" >| "StateZ")

type DoorSpec = Wrap $ Empty
  :* ("DoorIsOpen" :@ "CloseTheDoor" >| "DoorIsClosed")
  :* ("DoorIsClosed" :@ "OpenTheDoor" >| "DoorIsOpen")

main :: Effect Unit
main = do
  TransitGraphviz.writeToFile_ @ColorTest "graphs/color-test.dot"
  TransitGraphviz.writeToFile_ @DoorSpec "graphs/door-graph.dot"
  pure unit
-- data Msg = Msg1 { foo :: Int } | Msg2 { bar :: String }

-- derive instance Generic Msg _

-- data State = State1 { foo :: Int } | State2 { bar :: String } | State3 { baz :: Boolean }

-- derive instance Generic State _

-- type MyStateGraph :: StateGraph
-- type MyStateGraph = MkStateGraph
--   ( Nil'
--       :< (MkTransition "State1" "Msg1" (Nil' :< MkReturn "State2"))
--       :< (MkTransition "State2" "Msg2" (Nil' :< MkReturnVia "foo" "State3" :< MkReturn "State1"))
--   )

-- type MyStateGraphDSLRep :: StateGraphDSL
-- type MyStateGraphDSLRep = MkStateGraphDSL
--   # AddTransition (TransitionBuilderInit "State1" "Msg1" # TransitionBuilderAddRet "State2")
--   # AddTransition (TransitionBuilderInit "State2" "Msg2" # TransitionBuilderAddRet "State3" # TransitionBuilderAddExtraRet "State1")

-- type MyStateGraphDSL :: StateGraphDSL
-- type MyStateGraphDSL = MkStateGraphDSL
--   :* ("State1" :@ "Msg1" :-> Out "State2")
--   :* ("State2" :@ "Msg2" :-> Out "State3" :| Out "State1")

-- type MyStateGraphDSL2 :: StateGraphDSL
-- type MyStateGraphDSL2 = MkStateGraphDSL
--   :* ("State1" :@ "Msg1" :-> "State2")
--   :* ("State2" :@ "Msg2" :-> ("Guard1" : "State3", "Guard2" : "State1"))

-- type MyStateGraphDSL3 :: StateGraphDSL
-- type MyStateGraphDSL3 = MkStateGraphDSL
--   :* ("State1" :@ "Msg1" :-> "State2")
--   :* ("State2" :@ "Msg2" :-> ("Guard1" :? "State3") :| ("Guard2" :? "State1"))

-- type MyStateGraphDSL4 = MkStateGraphDSL
--   :* ("State1" :@ "Msg1" :> "State2")
--   :*
--     ( "State2" :@ "Msg2"
--         :>> "Guard1" :? "State3"
--         :|| "Guard2" :? "State1"
--         :|| "Guard3" :? "State4"
--     )
--   :*
--     ( "State3" :@ "Msg3"
--         :>> "Guard1" :? "State1"
--         :|| "Guard2" :? "State2"
--         :|| "Guard3" :? "State4"
--     )

--data T a b
-- data G a b

-- data S a b

-- infixl 9 type T as :?
-- infixl 7 type G as :||
-- infixl 7 type S as :>>

-- infixl 5 type T as :>

-- update :: Msg -> State -> State
-- update = mkUpdateG @MyStateGraph $
--   unit
--     & match @"State1" @"Msg1" (\msg state -> return @"State2" { bar: "" })
--     & match @"State2" @"Msg2"
--         ( \msg state ->
--             if true then
--               return @"State3" (RetVia @"foo" { baz: false })
--             else
--               return @"State1" { foo: 0 }
--         )

-- infixl 5 Tuple as &
