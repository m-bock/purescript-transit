-- | The landings a message can have, asserted by the compiler.
-- |
-- | Each signature below is the assertion: if `Landings` computed any
-- | other row, the file would not compile. There is nothing to run.
module Test.Transit.Class.Landings where

import Prelude (Unit, unit)

import Transit (type (:*), type (:@), type (>|), Transit)
import Transit.Class.Landings (class Landings)

type StateRow =
  ( "Idle" :: {}
  , "Asking" :: {}
  , "Showing" :: { readings :: Int }
  , "Refused" :: { why :: String }
  )

type Spec =
  Transit
    :* ("Idle" :@ "Asked" >| "Asking")
    :* ("Asking" :@ "Got" >| "Showing")
    :* ("Asking" :@ "Broke" >| "Refused")
    :* ("Showing" :@ "Asked" >| "Asking")
    :* ("Refused" :@ "Asked" >| "Asking")

-- | Three arrows are labelled `Asked` and all land in `Asking`, so the
-- | row has one field however many states can send it.
-- |
-- | Each pair below is one assertion. The constraint is not solved
-- | where it is written - a constraint in a signature waits for a use
-- | - so every claim is forced by a second declaration that uses it at
-- | no constraint of its own. Remove the second and the first proves
-- | nothing.
askedLandsInAsking :: Landings Spec "Asked" StateRow ("Asking" :: {}) => Unit
askedLandsInAsking = unit

asked :: Unit
asked = askedLandsInAsking

-- | A message with one arrow says one state, and says its payload too.
gotLandsInShowing :: Landings Spec "Got" StateRow ("Showing" :: { readings :: Int }) => Unit
gotLandsInShowing = unit

got :: Unit
got = gotLandsInShowing

brokeLandsInRefused :: Landings Spec "Broke" StateRow ("Refused" :: { why :: String }) => Unit
brokeLandsInRefused = unit

broke :: Unit
broke = brokeLandsInRefused

-- | A message the table never mentions lands nowhere, which is a row
-- | with no fields - and a caller given that has nothing to match.
unknownLandsNowhere :: Landings Spec "NeverSent" StateRow () => Unit
unknownLandsNowhere = unit

neverSent :: Unit
neverSent = unknownLandsNowhere
