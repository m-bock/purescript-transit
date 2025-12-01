module Test.Transit.DSL where

import Data.Reflectable (class Reflectable)
import Data.Unit (Unit, unit)
import Transit.Core (MkStateGraph, StateGraph, StateGraph_(..))
import Transit.Core as C
import Transit.DSL (class FromDSL, MkStateSpec)
import Transit.Util (type (:<))
import Type.Data.List (Nil')
import Type.Proxy (Proxy(..))

check :: forall @a @b. (FromDSL a b) => Unit
check = unit

test1 :: Unit
test1 = check @MkStateSpec @(MkStateGraph Nil')

-- test1 :: Unit
-- test1 = checkFromDSL
--   ( Proxy
--       :: Proxy
--            ( MkStateGraphDSL
--                :* ("State1" :@ "Msg1" :> "State2")
--                :* ("State2" :@ "Msg2" :> "State3" :| "State1")
--                :* ("State3" :@ "Msg3" :> "State1" :| "State2" :| "State4")
--            )
--   )
--   ( Proxy
--       :: Proxy
--            ( C.MkStateGraph
--                ( Nil'
--                    :< (C.MkTransition "State1" "Msg1" (Nil' :< "State2"))
--                    :< (C.MkTransition "State2" "Msg2" (Nil' :< "State3" :< "State1"))
--                    :< (C.MkTransition "State3" "Msg3" (Nil' :< "State1" :< "State2" :< "State4"))
--                )
--            )
--   )
