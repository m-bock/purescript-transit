module Test.Transit.DSL where

import Data.Unit (Unit, unit)
import Transit.Core (MkTransitCore)
import Transit.Core as C
import Transit.DSL (class FromDSL, type (:*), type (:?), type (:@), type (>|), Empty, Wrap)
import Type.Data.List (type (:>), Nil')
import Type.Function (type ($), APPLY)

check :: forall @a @b. (FromDSL a b) => Unit
check = unit

-- test1 :: Unit
-- test1 = check @C @(C.MkStateGraph Nil')

type In =
  Wrap $ Empty
    :*
      ( "State1" :@ "Msg1"
          >| ("guard" :? "State2")
          >| "State3"
      )
    :* ("State2" :@ "Msg2" >| "State3")
    :* ("State2" :@ "Msg2" >| "State3")

--  :* ("State2" :@ "Msg2")

infixr 1 type APPLY as $$

--:* ("State3" :@ "Msg3")

--:* ("State2" :@ "Msg2" >| "State3")
--:* ("State3" :@ "Msg3" >| "State1")

type Out = MkTransitCore
  ( (C.MkMatch "State1" "Msg1" (C.MkReturn "State3" :> C.MkReturnVia "guard" "State2" :> Nil'))
      :> (C.MkMatch "State2" "Msg2" (C.MkReturn "State3" :> Nil'))
      :> (C.MkMatch "State2" "Msg2" (C.MkReturn "State3" :> Nil'))
      :> Nil'
  )

test2 :: Unit
test2 = check @In @Out

-- test1 :: Unit
-- test1 = check @MkStateSpec @(MkStateGraph Nil')

-- type Test2DSL = MkStateSpec
--   :* ("State1" :@ "Msg1" :> "State2")
--   :* ("State2" :@ "Msg2" :> "State3" :| "State1")
--   :* ("State3" :@ "Msg3" :> "State1" :| "State2" :| "State4")

-- type Test2StateGraph = MkStateGraph
--   ( Nil'
--       :<
--         ( MkTransition "State1" "Msg1"
--             (Nil' :< MkReturn "State2")
--         )
--       :<
--         ( MkTransition "State2" "Msg2"
--             (Nil' :< MkReturn "State3" :< MkReturn "State1")
--         )
--       :<
--         ( MkTransition "State3" "Msg3"
--             (Nil' :< MkReturn "State1" :< MkReturn "State2" :< MkReturn "State4")
--         )
--   )

-- test2 :: Unit
-- test2 = check @Test2DSL @Test2StateGraph
