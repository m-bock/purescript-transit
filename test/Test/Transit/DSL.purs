module Test.Transit.DSL
  ( spec
  ) where

import Prelude

import Test.Spec (Spec, describe, it)
import Transit.Core (class IsTransitSpec, MkTransitCoreTL)
import Transit.Core as C
import Transit.DSL (type (:*), type (:?), type (:@), type (>|), type (|<), Transit)
import Type.Data.List (type (:>), Nil')

-- | Type-level test helper that verifies a DSL specification matches expected output.
check :: forall @a @b. (IsTransitSpec a b) => Unit
check = unit

spec :: Spec Unit
spec = do
  describe "Transit.DSL" do
    describe "empty specification" do
      it "creates empty transit core" do
        let _ = check @EmptyTransitIn @EmptyTransitOut
        pure unit

    describe "single transition" do
      it "creates single match" do
        let _ = check @SingleTransitionIn @SingleTransitionOut
        pure unit

    describe "multiple transitions" do
      it "creates multiple matches in order" do
        let _ = check @MultipleTransitionsIn @MultipleTransitionsOut
        pure unit

    describe "multiple return states" do
      it "creates match with multiple returns" do
        let _ = check @MultipleReturnsIn @MultipleReturnsOut
        pure unit

    describe "guards" do
      it "creates return with guard condition" do
        let _ = check @GuardReturnIn @GuardReturnOut
        pure unit

      it "creates match with multiple guarded returns" do
        let _ = check @MultipleGuardsIn @MultipleGuardsOut
        pure unit

    describe "bidirectional edges" do
      it "creates bidirectional transition" do
        let _ = check @BidirectionalIn @BidirectionalOut
        pure unit

    describe "complex specification" do
      it "handles complex multi-state specification" do
        let _ = check @ComplexSpecIn @ComplexSpecOut
        pure unit

--------------------------------------------------------------------------------
--- Test Type Definitions
--------------------------------------------------------------------------------

type EmptyTransitIn = Transit

type EmptyTransitOut = MkTransitCoreTL Nil'

type SingleTransitionIn = Transit :* ("State1" :@ "Msg1" >| "State2")

type SingleTransitionOut = MkTransitCoreTL
  ( (C.MkMatchTL "State1" "Msg1" (C.MkReturnTL "State2" :> Nil'))
      :> Nil'
  )

type MultipleTransitionsIn =
  Transit
    :* ("State3" :@ "Msg3" >| "State3")
    :* ("State2" :@ "Msg2" >| "State3")
    :* ("State1" :@ "Msg1" >| "State2")

type MultipleTransitionsOut = MkTransitCoreTL
  ( (C.MkMatchTL "State3" "Msg3" (C.MkReturnTL "State3" :> Nil'))
      :> (C.MkMatchTL "State2" "Msg2" (C.MkReturnTL "State3" :> Nil'))
      :> (C.MkMatchTL "State1" "Msg1" (C.MkReturnTL "State2" :> Nil'))
      :> Nil'
  )

type MultipleReturnsIn =
  Transit
    :*
      ( "State1" :@ "Msg1"
          >| "State3"
          >| "State2"
          >| "State1"
      )

type MultipleReturnsOut = MkTransitCoreTL
  ( (C.MkMatchTL "State1" "Msg1" (C.MkReturnTL "State1" :> C.MkReturnTL "State2" :> C.MkReturnTL "State3" :> Nil'))
      :> Nil'
  )

type GuardReturnIn =
  Transit
    :* ("State1" :@ "Msg1" >| ("guard" :? "State2"))

type GuardReturnOut = MkTransitCoreTL
  ( (C.MkMatchTL "State1" "Msg1" (C.MkReturnViaTL "guard" "State2" :> Nil'))
      :> Nil'
  )

type MultipleGuardsIn =
  Transit
    :*
      ( "State1" :@ "Msg1"
          >| ("guardA" :? "State2")
          >| ("guardB" :? "State3")
      )

type MultipleGuardsOut = MkTransitCoreTL
  ( ( C.MkMatchTL "State1" "Msg1"
        ( C.MkReturnViaTL "guardB" "State3"
            :> C.MkReturnViaTL "guardA" "State2"
            :> Nil'
        )
    )
      :> Nil'
  )

type BidirectionalIn =
  Transit
    :* ("State1" |< "Msg" >| "State2")

type BidirectionalOut = MkTransitCoreTL
  ( (C.MkMatchTL "State1" "Msg" (C.MkReturnTL "State2" :> Nil'))
      :> (C.MkMatchTL "State2" "Msg" (C.MkReturnTL "State1" :> Nil'))
      :> Nil'
  )

type ComplexSpecIn =
  Transit
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
    :* ("State3" :@ "Msg3" >| "State1")
    :* ("State3" :@ "Msg3" >| ("guard" :? "State1"))

type ComplexSpecOut = MkTransitCoreTL
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
