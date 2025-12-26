module Test.BenchDef.TransitSize20 where

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
    :* ("State20" :@ "Msg20" >| "State01")

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
  (match @"State20" @"Msg20" \_ _ -> return @"State01")

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
  , v @"Msg20" /\ v @"State01"
  ]
