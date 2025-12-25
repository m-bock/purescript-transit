module Test.BenchDef.TransitSize10 where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Transit (type (:*), type (:@), type (>|), Transit, match, mkUpdate, return)
import Transit.VariantUtils (v)
import Unsafe.Coerce (unsafeCoerce)

type State = Variant
  (
  "State01" :: {},
  "State02" :: {},
  "State03" :: {},
  "State04" :: {},
  "State05" :: {},
  "State06" :: {},
  "State07" :: {},
  "State08" :: {},
  "State09" :: {},
  "State10" :: {}
  )

printState :: State -> String
printState v = t
  where
  VariantRep { type: t } = unsafeCoerce v

type Msg = Variant
  (
  "Msg01" :: {},
  "Msg02" :: {},
  "Msg03" :: {},
  "Msg04" :: {},
  "Msg05" :: {},
  "Msg06" :: {},
  "Msg07" :: {},
  "Msg08" :: {},
  "Msg09" :: {},
  "Msg10" :: {}
  )

printMsg :: Msg -> String
printMsg v = t
  where
  VariantRep { type: t } = unsafeCoerce v

type BenchTransit =
  Transit
    :* ("State01" :@ "Msg01" >| "State02")
    :* ("State02" :@ "Msg02" >| "State03")
    :* ("State03" :@ "Msg03" >| "State04")
    :* ("State04" :@ "Msg04" >| "State05")
    :* ("State05" :@ "Msg05" >| "State06")
    :* ("State06" :@ "Msg06" >| "State07")
    :* ("State07" :@ "Msg07" >| "State08")
    :* ("State08" :@ "Msg08" >| "State09")
    :* ("State09" :@ "Msg09" >| "State10")
    :* ("State10" :@ "Msg10" >| "State01")

update :: State -> Msg -> State
update = mkUpdate @BenchTransit
  (match @"State01" @"Msg01" \_ _ -> return @"State02")
  (match @"State02" @"Msg02" \_ _ -> return @"State03")
  (match @"State03" @"Msg03" \_ _ -> return @"State04")
  (match @"State04" @"Msg04" \_ _ -> return @"State05")
  (match @"State05" @"Msg05" \_ _ -> return @"State06")
  (match @"State06" @"Msg06" \_ _ -> return @"State07")
  (match @"State07" @"Msg07" \_ _ -> return @"State08")
  (match @"State08" @"Msg08" \_ _ -> return @"State09")
  (match @"State09" @"Msg09" \_ _ -> return @"State10")
  (match @"State10" @"Msg10" \_ _ -> return @"State01")

walk :: Array (Msg /\ State)
walk =
  [
  v @"Msg01" /\ v @"State02",
  v @"Msg02" /\ v @"State03",
  v @"Msg03" /\ v @"State04",
  v @"Msg04" /\ v @"State05",
  v @"Msg05" /\ v @"State06",
  v @"Msg06" /\ v @"State07",
  v @"Msg07" /\ v @"State08",
  v @"Msg08" /\ v @"State09",
  v @"Msg09" /\ v @"State10",
  v @"Msg10" /\ v @"State01"
  ]

