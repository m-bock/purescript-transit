module Test.Transit.DSL
  ( check
  , test1
  , test2
  , test3
  , test4
  , test5
  ) where

import Data.Unit (Unit, unit)
import Transit.Core (class IsTransitSpec, MkTransitCoreTL)
import Transit.Core as C
import Transit.DSL (type (:*), type (:?), type (:@), type (>|), Empty)
import Type.Data.List (type (:>), Nil')
import Type.Function (type ($))

check :: forall @a @b. (IsTransitSpec a b) => Unit
check = unit

--------------------------------------------------------------------------------
-- Test 1: Empty DSL
--------------------------------------------------------------------------------

test1 :: Unit
test1 = check @Test1In @Test1Out

type Test1In = Empty

type Test1Out = MkTransitCoreTL Nil'

--------------------------------------------------------------------------------
-- Test 2: Multiple transitions
--------------------------------------------------------------------------------

test2 :: Unit
test2 = check @Test2In @Test2Out

type Test2In =
  Empty
    :* ("State3" :@ "Msg3" >| "State3")
    :* ("State2" :@ "Msg2" >| "State3")
    :* ("State3" :@ "Msg3" >| "State3")

type Test2Out = MkTransitCoreTL
  ( (C.MkMatchTL "State3" "Msg3" (C.MkReturnTL "State3" :> Nil'))
      :> (C.MkMatchTL "State2" "Msg2" (C.MkReturnTL "State3" :> Nil'))
      :> (C.MkMatchTL "State3" "Msg3" (C.MkReturnTL "State3" :> Nil'))
      :> Nil'
  )

--------------------------------------------------------------------------------
-- Test 3: Transition with multiple return states
--------------------------------------------------------------------------------

test3 :: Unit
test3 = check @Test3In @Test3Out

type Test3In =
  Empty
    :*
      ( "State1" :@ "Msg1"
          >| "State3"
          >| "State2"
          >| "State1"
      )

type Test3Out = MkTransitCoreTL
  ( (C.MkMatchTL "State1" "Msg1" (C.MkReturnTL "State1" :> C.MkReturnTL "State2" :> C.MkReturnTL "State3" :> Nil'))
      :> Nil'
  )

--------------------------------------------------------------------------------
-- Test 4: Transition with guards
--------------------------------------------------------------------------------

test4 :: Unit
test4 = check @Test4In @Test4Out

type Test4In =
  Empty
    :* ("State1" :@ "Msg1" >| ("guard" :? "State2"))

type Test4Out = MkTransitCoreTL
  ( (C.MkMatchTL "State1" "Msg1" (C.MkReturnViaTL "guard" "State2" :> Nil'))
      :> Nil'
  )

--------------------------------------------------------------------------------
-- Test 5: Full complex test
--------------------------------------------------------------------------------

test5 :: Unit
test5 = check @Test5In @Test5Out

type Test5In =
  Empty
    :*
      ( "State1" :@ "Msg1"
          >| ("guard" :? "State3")
          >| "State2"
          >| "State1"
      )

    :*
      ( "State2" :@ "Msg2"
          >| "State3"
          >| "State2"
          >| "State1"
      )
    :*
      ( "State3" :@ "Msg3"
          >| ("guardC" :? "State3")
          >| ("guardB" :? "State2")
          >| ("guardA" :? "State1")
      )

    :*
      ("State3" :@ "Msg3" >| "State1")

    :*
      ("State3" :@ "Msg3" >| ("guard" :? "State1"))

type Test5Out = MkTransitCoreTL
  ( ( C.MkMatchTL "State1" "Msg1"
        ( C.MkReturnTL "State1"
            :> C.MkReturnTL "State2"
            :> C.MkReturnViaTL "guard" "State3"
            :> Nil'
        )
    )
      :>
        ( C.MkMatchTL "State2" "Msg2"
            ( C.MkReturnTL "State1"
                :> C.MkReturnTL "State2"
                :> C.MkReturnTL "State3"
                :> Nil'
            )
        )
      :>
        ( C.MkMatchTL "State3" "Msg3"
            ( C.MkReturnViaTL "guardA" "State1"
                :> C.MkReturnViaTL "guardB" "State2"
                :> C.MkReturnViaTL "guardC" "State3"
                :> Nil'
            )
        )
      :>
        ( C.MkMatchTL "State3" "Msg3" (C.MkReturnTL "State1" :> Nil')
        )
      :>
        ( C.MkMatchTL "State3" "Msg3" (C.MkReturnViaTL "guard" "State1" :> Nil')
        )
      :> Nil'
  )
