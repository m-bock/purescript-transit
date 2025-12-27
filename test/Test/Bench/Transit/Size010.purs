module Test.Bench.Transit.Size010 where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Transit (type (:*), type (:@), type (>|), Transit, match, mkUpdate, return)
import Transit.VariantUtils (v)
import Unsafe.Coerce (unsafeCoerce)

type State = Variant
  ( "State001" :: {}
  , "State002" :: {}
  , "State003" :: {}
  , "State004" :: {}
  , "State005" :: {}
  , "State006" :: {}
  , "State007" :: {}
  , "State008" :: {}
  , "State009" :: {}
  , "State010" :: {}
  )

printState :: State -> String
printState v = t
  where
  VariantRep { type: t } = unsafeCoerce v

init :: State
init = v @"State001"

type Msg = Variant
  ( "Msg001" :: {}
  , "Msg002" :: {}
  , "Msg003" :: {}
  , "Msg004" :: {}
  , "Msg005" :: {}
  , "Msg006" :: {}
  , "Msg007" :: {}
  , "Msg008" :: {}
  , "Msg009" :: {}
  , "Msg010" :: {}
  )

printMsg :: Msg -> String
printMsg v = t
  where
  VariantRep { type: t } = unsafeCoerce v

type BenchTransit =
  Transit
    :* ("State001" :@ "Msg001" >| "State002")
    :* ("State002" :@ "Msg002" >| "State003")
    :* ("State003" :@ "Msg003" >| "State004")
    :* ("State004" :@ "Msg004" >| "State005")
    :* ("State005" :@ "Msg005" >| "State006")
    :* ("State006" :@ "Msg006" >| "State007")
    :* ("State007" :@ "Msg007" >| "State008")
    :* ("State008" :@ "Msg008" >| "State009")
    :* ("State009" :@ "Msg009" >| "State010")
    :* ("State010" :@ "Msg010" >| "State001")

update :: State -> Msg -> State
update = mkUpdate @BenchTransit
  (match @"State001" @"Msg001" \_ _ -> return @"State002")
  (match @"State002" @"Msg002" \_ _ -> return @"State003")
  (match @"State003" @"Msg003" \_ _ -> return @"State004")
  (match @"State004" @"Msg004" \_ _ -> return @"State005")
  (match @"State005" @"Msg005" \_ _ -> return @"State006")
  (match @"State006" @"Msg006" \_ _ -> return @"State007")
  (match @"State007" @"Msg007" \_ _ -> return @"State008")
  (match @"State008" @"Msg008" \_ _ -> return @"State009")
  (match @"State009" @"Msg009" \_ _ -> return @"State010")
  (match @"State010" @"Msg010" \_ _ -> return @"State001")

walk :: Array (Msg /\ State)
walk =
  [ v @"Msg001" /\ v @"State002"
  , v @"Msg002" /\ v @"State003"
  , v @"Msg003" /\ v @"State004"
  , v @"Msg004" /\ v @"State005"
  , v @"Msg005" /\ v @"State006"
  , v @"Msg006" /\ v @"State007"
  , v @"Msg007" /\ v @"State008"
  , v @"Msg008" /\ v @"State009"
  , v @"Msg009" /\ v @"State010"
  , v @"Msg010" /\ v @"State001"
  ]
