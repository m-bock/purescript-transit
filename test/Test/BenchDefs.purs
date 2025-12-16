module Test.BenchDefs where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Variant (Variant)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdate, mkUpdateGeneric, return)
import Type.Function (type ($))

data State
  = State01
  | State02
  | State03
  | State04
  | State05
  | State06
  | State07
  | State08
  | State09
  | State10
  | State11
  | State12
  | State13
  | State14
  | State15
  | State16
  | State17
  | State18
  | State19
  | State20

derive instance Generic State _

data Msg
  = Msg01
  | Msg02
  | Msg03
  | Msg04
  | Msg05
  | Msg06
  | Msg07
  | Msg08
  | Msg09
  | Msg10
  | Msg11
  | Msg12
  | Msg13
  | Msg14
  | Msg15
  | Msg16
  | Msg17
  | Msg18
  | Msg19
  | Msg20

derive instance Generic Msg _

type StateV = Variant
  ( "State01" :: Unit
  , "State02" :: Unit
  , "State03" :: Unit
  , "State04" :: Unit
  , "State05" :: Unit
  , "State06" :: Unit
  , "State07" :: Unit
  , "State08" :: Unit
  , "State09" :: Unit
  , "State10" :: Unit
  , "State11" :: Unit
  , "State12" :: Unit
  , "State13" :: Unit
  , "State14" :: Unit
  , "State15" :: Unit
  , "State16" :: Unit
  , "State17" :: Unit
  , "State18" :: Unit
  , "State19" :: Unit
  , "State20" :: Unit
  )

type MsgV = Variant
  ( "Msg01" :: Unit
  , "Msg02" :: Unit
  , "Msg03" :: Unit
  , "Msg04" :: Unit
  , "Msg05" :: Unit
  , "Msg06" :: Unit
  , "Msg07" :: Unit
  , "Msg08" :: Unit
  , "Msg09" :: Unit
  , "Msg10" :: Unit
  , "Msg11" :: Unit
  , "Msg12" :: Unit
  , "Msg13" :: Unit
  , "Msg14" :: Unit
  , "Msg15" :: Unit
  , "Msg16" :: Unit
  , "Msg17" :: Unit
  , "Msg18" :: Unit
  , "Msg19" :: Unit
  , "Msg20" :: Unit
  )

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  State01, Msg01 -> State02
  State02, Msg02 -> State03
  State03, Msg03 -> State04
  State04, Msg04 -> State05
  State05, Msg05 -> State06
  State06, Msg06 -> State07
  State07, Msg07 -> State08
  State08, Msg08 -> State09
  State09, Msg09 -> State10
  State10, Msg10 -> State11
  State11, Msg11 -> State12
  State12, Msg12 -> State13
  State13, Msg13 -> State14
  State14, Msg14 -> State15
  State15, Msg15 -> State16
  State16, Msg16 -> State17
  State17, Msg17 -> State18
  State18, Msg18 -> State19
  State19, Msg19 -> State20
  State20, Msg20 -> State01
  _, _ -> state

type Size5Transit =
  Transit $ Empty
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
update = mkUpdateGeneric @Size5Transit
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

updateV :: StateV -> MsgV -> StateV
updateV = mkUpdate @Size5Transit
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
