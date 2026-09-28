-- | `mkApply` against a real machine: what comes back is the states
-- | this message can produce, and `stuck` when it produced none.
module Test.Transit.Apply (spec) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe, fromMaybe)
import Data.Variant (Variant, inj, match)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Transit, mkUpdateMaybe, return)
import Transit (match) as Transit
import Transit.Apply (mkApply)
import Type.Proxy (Proxy(..))

type StateRow =
  ( "Idle" :: {}
  , "Asking" :: {}
  , "Showing" :: { readings :: Int }
  , "Refused" :: { why :: String }
  )

type MsgRow =
  ( "Asked" :: {}
  , "Got" :: { readings :: Int }
  , "Broke" :: { why :: String }
  )

type MachineSpec =
  Transit
    :* ("Idle" :@ "Asked" >| "Asking")
    :* ("Asking" :@ "Got" >| "Showing")
    :* ("Asking" :@ "Broke" >| "Refused")
    :* ("Showing" :@ "Asked" >| "Asking")
    :* ("Refused" :@ "Asked" >| "Asking")

update :: Variant StateRow -> Variant MsgRow -> Maybe (Variant StateRow)
update = mkUpdateMaybe @MachineSpec
  (Transit.match @"Idle" @"Asked" \_ _ -> return @"Asking" {})
  (Transit.match @"Asking" @"Got" \_ msg -> return @"Showing" { readings: msg.readings })
  (Transit.match @"Asking" @"Broke" \_ msg -> return @"Refused" { why: msg.why })
  (Transit.match @"Showing" @"Asked" \_ _ -> return @"Asking" {})
  (Transit.match @"Refused" @"Asked" \_ _ -> return @"Asking" {})

-- | A machine that holds no state: the state it starts from is handed
-- | in, and a message the table has no arrow for leaves it as it was.
-- | That is what `apply` is - a message in, the state it produced out.
applyFrom :: Variant StateRow -> Variant MsgRow -> Aff (Variant StateRow)
applyFrom was msg = pure (fromMaybe was (update was msg))

spec :: Spec Unit
spec = describe "Transit.Apply" do
  it "answers with the states one message can produce, and the whole state otherwise" do
    fromIdle <- mkApply @MachineSpec @"Asked"
      (applyFrom (inj (Proxy @"Idle") {}))
      (inj (Proxy @"Asked") {})

    drawAsked fromIdle `shouldEqual` "asking"

    fromShowing <- mkApply @MachineSpec @"Asked"
      (applyFrom (inj (Proxy @"Showing") { readings: 3 }))
      (inj (Proxy @"Asked") {})

    drawAsked fromShowing `shouldEqual` "asking"

    -- `Asking` has no `Asked` arrow, so nothing moved - and what comes
    -- back is still `Asking`, which is a landing of `Asked`. `stuck` is
    -- about where the machine ended up, not about whether an arrow
    -- fired, and a caller that needs the difference compares states.
    unmoved <- mkApply @MachineSpec @"Asked"
      (applyFrom (inj (Proxy @"Asking") {}))
      (inj (Proxy @"Asked") {})

    drawAsked unmoved `shouldEqual` "asking"

    -- A message with one arrow: one landing, and its payload with it.
    got <- mkApply @MachineSpec @"Got"
      (applyFrom (inj (Proxy @"Asking") {}))
      (inj (Proxy @"Got") { readings: 7 })

    case got of
      Right landed -> landed # match
        { "Showing": \one -> one.readings `shouldEqual` 7 }
      Left _ -> fail "expected Got to land in Showing"

    -- `Broke` from a state that cannot send it: no landing, and what
    -- comes back on the left is the whole state.
    broke <- mkApply @MachineSpec @"Broke"
      (applyFrom (inj (Proxy @"Idle") {}))
      (inj (Proxy @"Broke") { why: "no" })

    case broke of
      Right landed -> landed # match
        { "Refused": \one -> fail ("expected no landing, got Refused " <> one.why) }
      Left was -> draw was `shouldEqual` "idle"

-- | Where `Asked` can land, drawn. The record is exhaustive over what
-- | `mkApply` hands back on the right and there is no other case to
-- | write: `Idle`, `Showing` and `Refused` are not in its type.
drawAsked :: Either (Variant StateRow) (Variant ("Asking" :: {})) -> String
drawAsked = case _ of
  Right landed -> landed # match { "Asking": \_ -> "asking" }
  Left was -> "nowhere, at " <> draw was

draw :: Variant StateRow -> String
draw = match
  { "Idle": \_ -> "idle"
  , "Asking": \_ -> "asking"
  , "Showing": \one -> "showing " <> show one.readings
  , "Refused": \one -> "refused, " <> one.why
  }

fail :: String -> Aff Unit
fail why = why `shouldEqual` "no failure"

-- ## Context
--
-- One case with six assertions, because what is being tested is a walk
-- over a table: a version that worked for the first arrow and stopped
-- would pass three separate cases and fail here.
--
-- `applyFrom` is the machine without the machine - the state is handed
-- in rather than held, so there is no ref and nothing to reset between
-- assertions, and `apply` still has the shape `mkApply` takes.
--
-- **What the left side means.** The narrowing is by state: the right
-- holds the states this message can land in, the left every other
-- state. A message that fired no arrow and left the machine in a state
-- that happens to be one of its landings comes back on the right,
-- which the third assertion is there to say out loud.
