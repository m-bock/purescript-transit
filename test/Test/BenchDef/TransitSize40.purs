module Test.BenchDef.TransitSize40 where

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
  "State10" :: {},
  "State11" :: {},
  "State12" :: {},
  "State13" :: {},
  "State14" :: {},
  "State15" :: {},
  "State16" :: {},
  "State17" :: {},
  "State18" :: {},
  "State19" :: {},
  "State20" :: {},
  "State21" :: {},
  "State22" :: {},
  "State23" :: {},
  "State24" :: {},
  "State25" :: {},
  "State26" :: {},
  "State27" :: {},
  "State28" :: {},
  "State29" :: {},
  "State30" :: {},
  "State31" :: {},
  "State32" :: {},
  "State33" :: {},
  "State34" :: {},
  "State35" :: {},
  "State36" :: {},
  "State37" :: {},
  "State38" :: {},
  "State39" :: {},
  "State40" :: {}
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
  "Msg10" :: {},
  "Msg11" :: {},
  "Msg12" :: {},
  "Msg13" :: {},
  "Msg14" :: {},
  "Msg15" :: {},
  "Msg16" :: {},
  "Msg17" :: {},
  "Msg18" :: {},
  "Msg19" :: {},
  "Msg20" :: {},
  "Msg21" :: {},
  "Msg22" :: {},
  "Msg23" :: {},
  "Msg24" :: {},
  "Msg25" :: {},
  "Msg26" :: {},
  "Msg27" :: {},
  "Msg28" :: {},
  "Msg29" :: {},
  "Msg30" :: {},
  "Msg31" :: {},
  "Msg32" :: {},
  "Msg33" :: {},
  "Msg34" :: {},
  "Msg35" :: {},
  "Msg36" :: {},
  "Msg37" :: {},
  "Msg38" :: {},
  "Msg39" :: {},
  "Msg40" :: {}
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
    :* ("State10" :@ "Msg10" >| "State11")
    :* ("State11" :@ "Msg11" >| "State12")
    :* ("State12" :@ "Msg12" >| "State13")
    :* ("State13" :@ "Msg13" >| "State14")
    :* ("State14" :@ "Msg14" >| "State15")
    :* ("State15" :@ "Msg15" >| "State16")
    :* ("State16" :@ "Msg16" >| "State17")
    :* ("State17" :@ "Msg17" >| "State18")
    :* ("State18" :@ "Msg18" >| "State19")
    :* ("State19" :@ "Msg19" >| "State20")
    :* ("State20" :@ "Msg20" >| "State21")
    :* ("State21" :@ "Msg21" >| "State22")
    :* ("State22" :@ "Msg22" >| "State23")
    :* ("State23" :@ "Msg23" >| "State24")
    :* ("State24" :@ "Msg24" >| "State25")
    :* ("State25" :@ "Msg25" >| "State26")
    :* ("State26" :@ "Msg26" >| "State27")
    :* ("State27" :@ "Msg27" >| "State28")
    :* ("State28" :@ "Msg28" >| "State29")
    :* ("State29" :@ "Msg29" >| "State30")
    :* ("State30" :@ "Msg30" >| "State31")
    :* ("State31" :@ "Msg31" >| "State32")
    :* ("State32" :@ "Msg32" >| "State33")
    :* ("State33" :@ "Msg33" >| "State34")
    :* ("State34" :@ "Msg34" >| "State35")
    :* ("State35" :@ "Msg35" >| "State36")
    :* ("State36" :@ "Msg36" >| "State37")
    :* ("State37" :@ "Msg37" >| "State38")
    :* ("State38" :@ "Msg38" >| "State39")
    :* ("State39" :@ "Msg39" >| "State40")
    :* ("State40" :@ "Msg40" >| "State01")

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
  (match @"State10" @"Msg10" \_ _ -> return @"State11")
  (match @"State11" @"Msg11" \_ _ -> return @"State12")
  (match @"State12" @"Msg12" \_ _ -> return @"State13")
  (match @"State13" @"Msg13" \_ _ -> return @"State14")
  (match @"State14" @"Msg14" \_ _ -> return @"State15")
  (match @"State15" @"Msg15" \_ _ -> return @"State16")
  (match @"State16" @"Msg16" \_ _ -> return @"State17")
  (match @"State17" @"Msg17" \_ _ -> return @"State18")
  (match @"State18" @"Msg18" \_ _ -> return @"State19")
  (match @"State19" @"Msg19" \_ _ -> return @"State20")
  (match @"State20" @"Msg20" \_ _ -> return @"State21")
  (match @"State21" @"Msg21" \_ _ -> return @"State22")
  (match @"State22" @"Msg22" \_ _ -> return @"State23")
  (match @"State23" @"Msg23" \_ _ -> return @"State24")
  (match @"State24" @"Msg24" \_ _ -> return @"State25")
  (match @"State25" @"Msg25" \_ _ -> return @"State26")
  (match @"State26" @"Msg26" \_ _ -> return @"State27")
  (match @"State27" @"Msg27" \_ _ -> return @"State28")
  (match @"State28" @"Msg28" \_ _ -> return @"State29")
  (match @"State29" @"Msg29" \_ _ -> return @"State30")
  (match @"State30" @"Msg30" \_ _ -> return @"State31")
  (match @"State31" @"Msg31" \_ _ -> return @"State32")
  (match @"State32" @"Msg32" \_ _ -> return @"State33")
  (match @"State33" @"Msg33" \_ _ -> return @"State34")
  (match @"State34" @"Msg34" \_ _ -> return @"State35")
  (match @"State35" @"Msg35" \_ _ -> return @"State36")
  (match @"State36" @"Msg36" \_ _ -> return @"State37")
  (match @"State37" @"Msg37" \_ _ -> return @"State38")
  (match @"State38" @"Msg38" \_ _ -> return @"State39")
  (match @"State39" @"Msg39" \_ _ -> return @"State40")
  (match @"State40" @"Msg40" \_ _ -> return @"State01")

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
  v @"Msg10" /\ v @"State11",
  v @"Msg11" /\ v @"State12",
  v @"Msg12" /\ v @"State13",
  v @"Msg13" /\ v @"State14",
  v @"Msg14" /\ v @"State15",
  v @"Msg15" /\ v @"State16",
  v @"Msg16" /\ v @"State17",
  v @"Msg17" /\ v @"State18",
  v @"Msg18" /\ v @"State19",
  v @"Msg19" /\ v @"State20",
  v @"Msg20" /\ v @"State21",
  v @"Msg21" /\ v @"State22",
  v @"Msg22" /\ v @"State23",
  v @"Msg23" /\ v @"State24",
  v @"Msg24" /\ v @"State25",
  v @"Msg25" /\ v @"State26",
  v @"Msg26" /\ v @"State27",
  v @"Msg27" /\ v @"State28",
  v @"Msg28" /\ v @"State29",
  v @"Msg29" /\ v @"State30",
  v @"Msg30" /\ v @"State31",
  v @"Msg31" /\ v @"State32",
  v @"Msg32" /\ v @"State33",
  v @"Msg33" /\ v @"State34",
  v @"Msg34" /\ v @"State35",
  v @"Msg35" /\ v @"State36",
  v @"Msg36" /\ v @"State37",
  v @"Msg37" /\ v @"State38",
  v @"Msg38" /\ v @"State39",
  v @"Msg39" /\ v @"State40",
  v @"Msg40" /\ v @"State01"
  ]

