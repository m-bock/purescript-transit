module Test.BenchDef.TransitSize90 where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))
import Data.Variant (Variant)
import Data.Variant.Internal (VariantRep(..))
import Transit (type (:*), type (:@), type (>|), Transit, match, mkUpdate, return)
import Transit.VariantUtils (v)
import Unsafe.Coerce (unsafeCoerce)

type State = Variant
  ( "State01" :: {}
  , "State02" :: {}
  , "State03" :: {}
  , "State04" :: {}
  , "State05" :: {}
  , "State06" :: {}
  , "State07" :: {}
  , "State08" :: {}
  , "State09" :: {}
  , "State10" :: {}
  , "State11" :: {}
  , "State12" :: {}
  , "State13" :: {}
  , "State14" :: {}
  , "State15" :: {}
  , "State16" :: {}
  , "State17" :: {}
  , "State18" :: {}
  , "State19" :: {}
  , "State20" :: {}
  , "State21" :: {}
  , "State22" :: {}
  , "State23" :: {}
  , "State24" :: {}
  , "State25" :: {}
  , "State26" :: {}
  , "State27" :: {}
  , "State28" :: {}
  , "State29" :: {}
  , "State30" :: {}
  , "State31" :: {}
  , "State32" :: {}
  , "State33" :: {}
  , "State34" :: {}
  , "State35" :: {}
  , "State36" :: {}
  , "State37" :: {}
  , "State38" :: {}
  , "State39" :: {}
  , "State40" :: {}
  , "State41" :: {}
  , "State42" :: {}
  , "State43" :: {}
  , "State44" :: {}
  , "State45" :: {}
  , "State46" :: {}
  , "State47" :: {}
  , "State48" :: {}
  , "State49" :: {}
  , "State50" :: {}
  , "State51" :: {}
  , "State52" :: {}
  , "State53" :: {}
  , "State54" :: {}
  , "State55" :: {}
  , "State56" :: {}
  , "State57" :: {}
  , "State58" :: {}
  , "State59" :: {}
  , "State60" :: {}
  , "State61" :: {}
  , "State62" :: {}
  , "State63" :: {}
  , "State64" :: {}
  , "State65" :: {}
  , "State66" :: {}
  , "State67" :: {}
  , "State68" :: {}
  , "State69" :: {}
  , "State70" :: {}
  , "State71" :: {}
  , "State72" :: {}
  , "State73" :: {}
  , "State74" :: {}
  , "State75" :: {}
  , "State76" :: {}
  , "State77" :: {}
  , "State78" :: {}
  , "State79" :: {}
  , "State80" :: {}
  , "State81" :: {}
  , "State82" :: {}
  , "State83" :: {}
  , "State84" :: {}
  , "State85" :: {}
  , "State86" :: {}
  , "State87" :: {}
  , "State88" :: {}
  , "State89" :: {}
  , "State90" :: {}
  )

printState :: State -> String
printState v = t
  where
  VariantRep { type: t } = unsafeCoerce v

init :: State
init = v @"State01"

type Msg = Variant
  ( "Msg01" :: {}
  , "Msg02" :: {}
  , "Msg03" :: {}
  , "Msg04" :: {}
  , "Msg05" :: {}
  , "Msg06" :: {}
  , "Msg07" :: {}
  , "Msg08" :: {}
  , "Msg09" :: {}
  , "Msg10" :: {}
  , "Msg11" :: {}
  , "Msg12" :: {}
  , "Msg13" :: {}
  , "Msg14" :: {}
  , "Msg15" :: {}
  , "Msg16" :: {}
  , "Msg17" :: {}
  , "Msg18" :: {}
  , "Msg19" :: {}
  , "Msg20" :: {}
  , "Msg21" :: {}
  , "Msg22" :: {}
  , "Msg23" :: {}
  , "Msg24" :: {}
  , "Msg25" :: {}
  , "Msg26" :: {}
  , "Msg27" :: {}
  , "Msg28" :: {}
  , "Msg29" :: {}
  , "Msg30" :: {}
  , "Msg31" :: {}
  , "Msg32" :: {}
  , "Msg33" :: {}
  , "Msg34" :: {}
  , "Msg35" :: {}
  , "Msg36" :: {}
  , "Msg37" :: {}
  , "Msg38" :: {}
  , "Msg39" :: {}
  , "Msg40" :: {}
  , "Msg41" :: {}
  , "Msg42" :: {}
  , "Msg43" :: {}
  , "Msg44" :: {}
  , "Msg45" :: {}
  , "Msg46" :: {}
  , "Msg47" :: {}
  , "Msg48" :: {}
  , "Msg49" :: {}
  , "Msg50" :: {}
  , "Msg51" :: {}
  , "Msg52" :: {}
  , "Msg53" :: {}
  , "Msg54" :: {}
  , "Msg55" :: {}
  , "Msg56" :: {}
  , "Msg57" :: {}
  , "Msg58" :: {}
  , "Msg59" :: {}
  , "Msg60" :: {}
  , "Msg61" :: {}
  , "Msg62" :: {}
  , "Msg63" :: {}
  , "Msg64" :: {}
  , "Msg65" :: {}
  , "Msg66" :: {}
  , "Msg67" :: {}
  , "Msg68" :: {}
  , "Msg69" :: {}
  , "Msg70" :: {}
  , "Msg71" :: {}
  , "Msg72" :: {}
  , "Msg73" :: {}
  , "Msg74" :: {}
  , "Msg75" :: {}
  , "Msg76" :: {}
  , "Msg77" :: {}
  , "Msg78" :: {}
  , "Msg79" :: {}
  , "Msg80" :: {}
  , "Msg81" :: {}
  , "Msg82" :: {}
  , "Msg83" :: {}
  , "Msg84" :: {}
  , "Msg85" :: {}
  , "Msg86" :: {}
  , "Msg87" :: {}
  , "Msg88" :: {}
  , "Msg89" :: {}
  , "Msg90" :: {}
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
    :* ("State40" :@ "Msg40" >| "State41")
    :* ("State41" :@ "Msg41" >| "State42")
    :* ("State42" :@ "Msg42" >| "State43")
    :* ("State43" :@ "Msg43" >| "State44")
    :* ("State44" :@ "Msg44" >| "State45")
    :* ("State45" :@ "Msg45" >| "State46")
    :* ("State46" :@ "Msg46" >| "State47")
    :* ("State47" :@ "Msg47" >| "State48")
    :* ("State48" :@ "Msg48" >| "State49")
    :* ("State49" :@ "Msg49" >| "State50")
    :* ("State50" :@ "Msg50" >| "State51")
    :* ("State51" :@ "Msg51" >| "State52")
    :* ("State52" :@ "Msg52" >| "State53")
    :* ("State53" :@ "Msg53" >| "State54")
    :* ("State54" :@ "Msg54" >| "State55")
    :* ("State55" :@ "Msg55" >| "State56")
    :* ("State56" :@ "Msg56" >| "State57")
    :* ("State57" :@ "Msg57" >| "State58")
    :* ("State58" :@ "Msg58" >| "State59")
    :* ("State59" :@ "Msg59" >| "State60")
    :* ("State60" :@ "Msg60" >| "State61")
    :* ("State61" :@ "Msg61" >| "State62")
    :* ("State62" :@ "Msg62" >| "State63")
    :* ("State63" :@ "Msg63" >| "State64")
    :* ("State64" :@ "Msg64" >| "State65")
    :* ("State65" :@ "Msg65" >| "State66")
    :* ("State66" :@ "Msg66" >| "State67")
    :* ("State67" :@ "Msg67" >| "State68")
    :* ("State68" :@ "Msg68" >| "State69")
    :* ("State69" :@ "Msg69" >| "State70")
    :* ("State70" :@ "Msg70" >| "State71")
    :* ("State71" :@ "Msg71" >| "State72")
    :* ("State72" :@ "Msg72" >| "State73")
    :* ("State73" :@ "Msg73" >| "State74")
    :* ("State74" :@ "Msg74" >| "State75")
    :* ("State75" :@ "Msg75" >| "State76")
    :* ("State76" :@ "Msg76" >| "State77")
    :* ("State77" :@ "Msg77" >| "State78")
    :* ("State78" :@ "Msg78" >| "State79")
    :* ("State79" :@ "Msg79" >| "State80")
    :* ("State80" :@ "Msg80" >| "State81")
    :* ("State81" :@ "Msg81" >| "State82")
    :* ("State82" :@ "Msg82" >| "State83")
    :* ("State83" :@ "Msg83" >| "State84")
    :* ("State84" :@ "Msg84" >| "State85")
    :* ("State85" :@ "Msg85" >| "State86")
    :* ("State86" :@ "Msg86" >| "State87")
    :* ("State87" :@ "Msg87" >| "State88")
    :* ("State88" :@ "Msg88" >| "State89")
    :* ("State89" :@ "Msg89" >| "State90")
    :* ("State90" :@ "Msg90" >| "State01")

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
  (match @"State40" @"Msg40" \_ _ -> return @"State41")
  (match @"State41" @"Msg41" \_ _ -> return @"State42")
  (match @"State42" @"Msg42" \_ _ -> return @"State43")
  (match @"State43" @"Msg43" \_ _ -> return @"State44")
  (match @"State44" @"Msg44" \_ _ -> return @"State45")
  (match @"State45" @"Msg45" \_ _ -> return @"State46")
  (match @"State46" @"Msg46" \_ _ -> return @"State47")
  (match @"State47" @"Msg47" \_ _ -> return @"State48")
  (match @"State48" @"Msg48" \_ _ -> return @"State49")
  (match @"State49" @"Msg49" \_ _ -> return @"State50")
  (match @"State50" @"Msg50" \_ _ -> return @"State51")
  (match @"State51" @"Msg51" \_ _ -> return @"State52")
  (match @"State52" @"Msg52" \_ _ -> return @"State53")
  (match @"State53" @"Msg53" \_ _ -> return @"State54")
  (match @"State54" @"Msg54" \_ _ -> return @"State55")
  (match @"State55" @"Msg55" \_ _ -> return @"State56")
  (match @"State56" @"Msg56" \_ _ -> return @"State57")
  (match @"State57" @"Msg57" \_ _ -> return @"State58")
  (match @"State58" @"Msg58" \_ _ -> return @"State59")
  (match @"State59" @"Msg59" \_ _ -> return @"State60")
  (match @"State60" @"Msg60" \_ _ -> return @"State61")
  (match @"State61" @"Msg61" \_ _ -> return @"State62")
  (match @"State62" @"Msg62" \_ _ -> return @"State63")
  (match @"State63" @"Msg63" \_ _ -> return @"State64")
  (match @"State64" @"Msg64" \_ _ -> return @"State65")
  (match @"State65" @"Msg65" \_ _ -> return @"State66")
  (match @"State66" @"Msg66" \_ _ -> return @"State67")
  (match @"State67" @"Msg67" \_ _ -> return @"State68")
  (match @"State68" @"Msg68" \_ _ -> return @"State69")
  (match @"State69" @"Msg69" \_ _ -> return @"State70")
  (match @"State70" @"Msg70" \_ _ -> return @"State71")
  (match @"State71" @"Msg71" \_ _ -> return @"State72")
  (match @"State72" @"Msg72" \_ _ -> return @"State73")
  (match @"State73" @"Msg73" \_ _ -> return @"State74")
  (match @"State74" @"Msg74" \_ _ -> return @"State75")
  (match @"State75" @"Msg75" \_ _ -> return @"State76")
  (match @"State76" @"Msg76" \_ _ -> return @"State77")
  (match @"State77" @"Msg77" \_ _ -> return @"State78")
  (match @"State78" @"Msg78" \_ _ -> return @"State79")
  (match @"State79" @"Msg79" \_ _ -> return @"State80")
  (match @"State80" @"Msg80" \_ _ -> return @"State81")
  (match @"State81" @"Msg81" \_ _ -> return @"State82")
  (match @"State82" @"Msg82" \_ _ -> return @"State83")
  (match @"State83" @"Msg83" \_ _ -> return @"State84")
  (match @"State84" @"Msg84" \_ _ -> return @"State85")
  (match @"State85" @"Msg85" \_ _ -> return @"State86")
  (match @"State86" @"Msg86" \_ _ -> return @"State87")
  (match @"State87" @"Msg87" \_ _ -> return @"State88")
  (match @"State88" @"Msg88" \_ _ -> return @"State89")
  (match @"State89" @"Msg89" \_ _ -> return @"State90")
  (match @"State90" @"Msg90" \_ _ -> return @"State01")

walk :: Array (Msg /\ State)
walk =
  [ v @"Msg01" /\ v @"State02"
  , v @"Msg02" /\ v @"State03"
  , v @"Msg03" /\ v @"State04"
  , v @"Msg04" /\ v @"State05"
  , v @"Msg05" /\ v @"State06"
  , v @"Msg06" /\ v @"State07"
  , v @"Msg07" /\ v @"State08"
  , v @"Msg08" /\ v @"State09"
  , v @"Msg09" /\ v @"State10"
  , v @"Msg10" /\ v @"State11"
  , v @"Msg11" /\ v @"State12"
  , v @"Msg12" /\ v @"State13"
  , v @"Msg13" /\ v @"State14"
  , v @"Msg14" /\ v @"State15"
  , v @"Msg15" /\ v @"State16"
  , v @"Msg16" /\ v @"State17"
  , v @"Msg17" /\ v @"State18"
  , v @"Msg18" /\ v @"State19"
  , v @"Msg19" /\ v @"State20"
  , v @"Msg20" /\ v @"State21"
  , v @"Msg21" /\ v @"State22"
  , v @"Msg22" /\ v @"State23"
  , v @"Msg23" /\ v @"State24"
  , v @"Msg24" /\ v @"State25"
  , v @"Msg25" /\ v @"State26"
  , v @"Msg26" /\ v @"State27"
  , v @"Msg27" /\ v @"State28"
  , v @"Msg28" /\ v @"State29"
  , v @"Msg29" /\ v @"State30"
  , v @"Msg30" /\ v @"State31"
  , v @"Msg31" /\ v @"State32"
  , v @"Msg32" /\ v @"State33"
  , v @"Msg33" /\ v @"State34"
  , v @"Msg34" /\ v @"State35"
  , v @"Msg35" /\ v @"State36"
  , v @"Msg36" /\ v @"State37"
  , v @"Msg37" /\ v @"State38"
  , v @"Msg38" /\ v @"State39"
  , v @"Msg39" /\ v @"State40"
  , v @"Msg40" /\ v @"State41"
  , v @"Msg41" /\ v @"State42"
  , v @"Msg42" /\ v @"State43"
  , v @"Msg43" /\ v @"State44"
  , v @"Msg44" /\ v @"State45"
  , v @"Msg45" /\ v @"State46"
  , v @"Msg46" /\ v @"State47"
  , v @"Msg47" /\ v @"State48"
  , v @"Msg48" /\ v @"State49"
  , v @"Msg49" /\ v @"State50"
  , v @"Msg50" /\ v @"State51"
  , v @"Msg51" /\ v @"State52"
  , v @"Msg52" /\ v @"State53"
  , v @"Msg53" /\ v @"State54"
  , v @"Msg54" /\ v @"State55"
  , v @"Msg55" /\ v @"State56"
  , v @"Msg56" /\ v @"State57"
  , v @"Msg57" /\ v @"State58"
  , v @"Msg58" /\ v @"State59"
  , v @"Msg59" /\ v @"State60"
  , v @"Msg60" /\ v @"State61"
  , v @"Msg61" /\ v @"State62"
  , v @"Msg62" /\ v @"State63"
  , v @"Msg63" /\ v @"State64"
  , v @"Msg64" /\ v @"State65"
  , v @"Msg65" /\ v @"State66"
  , v @"Msg66" /\ v @"State67"
  , v @"Msg67" /\ v @"State68"
  , v @"Msg68" /\ v @"State69"
  , v @"Msg69" /\ v @"State70"
  , v @"Msg70" /\ v @"State71"
  , v @"Msg71" /\ v @"State72"
  , v @"Msg72" /\ v @"State73"
  , v @"Msg73" /\ v @"State74"
  , v @"Msg74" /\ v @"State75"
  , v @"Msg75" /\ v @"State76"
  , v @"Msg76" /\ v @"State77"
  , v @"Msg77" /\ v @"State78"
  , v @"Msg78" /\ v @"State79"
  , v @"Msg79" /\ v @"State80"
  , v @"Msg80" /\ v @"State81"
  , v @"Msg81" /\ v @"State82"
  , v @"Msg82" /\ v @"State83"
  , v @"Msg83" /\ v @"State84"
  , v @"Msg84" /\ v @"State85"
  , v @"Msg85" /\ v @"State86"
  , v @"Msg86" /\ v @"State87"
  , v @"Msg87" /\ v @"State88"
  , v @"Msg88" /\ v @"State89"
  , v @"Msg89" /\ v @"State90"
  , v @"Msg90" /\ v @"State01"
  ]
