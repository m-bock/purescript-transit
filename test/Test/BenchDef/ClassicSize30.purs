module Test.BenchDef.ClassicSize30 where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))

data StateD
  = State01 {}
  | State02 {}
  | State03 {}
  | State04 {}
  | State05 {}
  | State06 {}
  | State07 {}
  | State08 {}
  | State09 {}
  | State10 {}
  | State11 {}
  | State12 {}
  | State13 {}
  | State14 {}
  | State15 {}
  | State16 {}
  | State17 {}
  | State18 {}
  | State19 {}
  | State20 {}
  | State21 {}
  | State22 {}
  | State23 {}
  | State24 {}
  | State25 {}
  | State26 {}
  | State27 {}
  | State28 {}
  | State29 {}
  | State30 {}

derive instance Eq StateD

printStateD :: StateD -> String
printStateD = case _ of
  State01 {} -> "State01"
  State02 {} -> "State02"
  State03 {} -> "State03"
  State04 {} -> "State04"
  State05 {} -> "State05"
  State06 {} -> "State06"
  State07 {} -> "State07"
  State08 {} -> "State08"
  State09 {} -> "State09"
  State10 {} -> "State10"
  State11 {} -> "State11"
  State12 {} -> "State12"
  State13 {} -> "State13"
  State14 {} -> "State14"
  State15 {} -> "State15"
  State16 {} -> "State16"
  State17 {} -> "State17"
  State18 {} -> "State18"
  State19 {} -> "State19"
  State20 {} -> "State20"
  State21 {} -> "State21"
  State22 {} -> "State22"
  State23 {} -> "State23"
  State24 {} -> "State24"
  State25 {} -> "State25"
  State26 {} -> "State26"
  State27 {} -> "State27"
  State28 {} -> "State28"
  State29 {} -> "State29"
  State30 {} -> "State30"

data MsgD
  = Msg01 {}
  | Msg02 {}
  | Msg03 {}
  | Msg04 {}
  | Msg05 {}
  | Msg06 {}
  | Msg07 {}
  | Msg08 {}
  | Msg09 {}
  | Msg10 {}
  | Msg11 {}
  | Msg12 {}
  | Msg13 {}
  | Msg14 {}
  | Msg15 {}
  | Msg16 {}
  | Msg17 {}
  | Msg18 {}
  | Msg19 {}
  | Msg20 {}
  | Msg21 {}
  | Msg22 {}
  | Msg23 {}
  | Msg24 {}
  | Msg25 {}
  | Msg26 {}
  | Msg27 {}
  | Msg28 {}
  | Msg29 {}
  | Msg30 {}

derive instance Eq MsgD

printMsgD :: MsgD -> String
printMsgD = case _ of
  Msg01 {} -> "Msg01"
  Msg02 {} -> "Msg02"
  Msg03 {} -> "Msg03"
  Msg04 {} -> "Msg04"
  Msg05 {} -> "Msg05"
  Msg06 {} -> "Msg06"
  Msg07 {} -> "Msg07"
  Msg08 {} -> "Msg08"
  Msg09 {} -> "Msg09"
  Msg10 {} -> "Msg10"
  Msg11 {} -> "Msg11"
  Msg12 {} -> "Msg12"
  Msg13 {} -> "Msg13"
  Msg14 {} -> "Msg14"
  Msg15 {} -> "Msg15"
  Msg16 {} -> "Msg16"
  Msg17 {} -> "Msg17"
  Msg18 {} -> "Msg18"
  Msg19 {} -> "Msg19"
  Msg20 {} -> "Msg20"
  Msg21 {} -> "Msg21"
  Msg22 {} -> "Msg22"
  Msg23 {} -> "Msg23"
  Msg24 {} -> "Msg24"
  Msg25 {} -> "Msg25"
  Msg26 {} -> "Msg26"
  Msg27 {} -> "Msg27"
  Msg28 {} -> "Msg28"
  Msg29 {} -> "Msg29"
  Msg30 {} -> "Msg30"

updateClassic :: StateD -> MsgD -> StateD
updateClassic state msg = case state, msg of
  State01 {}, Msg01 {} -> State02 {}
  State02 {}, Msg02 {} -> State03 {}
  State03 {}, Msg03 {} -> State04 {}
  State04 {}, Msg04 {} -> State05 {}
  State05 {}, Msg05 {} -> State06 {}
  State06 {}, Msg06 {} -> State07 {}
  State07 {}, Msg07 {} -> State08 {}
  State08 {}, Msg08 {} -> State09 {}
  State09 {}, Msg09 {} -> State10 {}
  State10 {}, Msg10 {} -> State11 {}
  State11 {}, Msg11 {} -> State12 {}
  State12 {}, Msg12 {} -> State13 {}
  State13 {}, Msg13 {} -> State14 {}
  State14 {}, Msg14 {} -> State15 {}
  State15 {}, Msg15 {} -> State16 {}
  State16 {}, Msg16 {} -> State17 {}
  State17 {}, Msg17 {} -> State18 {}
  State18 {}, Msg18 {} -> State19 {}
  State19 {}, Msg19 {} -> State20 {}
  State20 {}, Msg20 {} -> State21 {}
  State21 {}, Msg21 {} -> State22 {}
  State22 {}, Msg22 {} -> State23 {}
  State23 {}, Msg23 {} -> State24 {}
  State24 {}, Msg24 {} -> State25 {}
  State25 {}, Msg25 {} -> State26 {}
  State26 {}, Msg26 {} -> State27 {}
  State27 {}, Msg27 {} -> State28 {}
  State28 {}, Msg28 {} -> State29 {}
  State29 {}, Msg29 {} -> State30 {}
  State30 {}, Msg30 {} -> State01 {}
  _, _ -> state

walkD :: Array (MsgD /\ StateD)
walkD =
  [
  Msg01 {} /\ State02 {},
  Msg02 {} /\ State03 {},
  Msg03 {} /\ State04 {},
  Msg04 {} /\ State05 {},
  Msg05 {} /\ State06 {},
  Msg06 {} /\ State07 {},
  Msg07 {} /\ State08 {},
  Msg08 {} /\ State09 {},
  Msg09 {} /\ State10 {},
  Msg10 {} /\ State11 {},
  Msg11 {} /\ State12 {},
  Msg12 {} /\ State13 {},
  Msg13 {} /\ State14 {},
  Msg14 {} /\ State15 {},
  Msg15 {} /\ State16 {},
  Msg16 {} /\ State17 {},
  Msg17 {} /\ State18 {},
  Msg18 {} /\ State19 {},
  Msg19 {} /\ State20 {},
  Msg20 {} /\ State21 {},
  Msg21 {} /\ State22 {},
  Msg22 {} /\ State23 {},
  Msg23 {} /\ State24 {},
  Msg24 {} /\ State25 {},
  Msg25 {} /\ State26 {},
  Msg26 {} /\ State27 {},
  Msg27 {} /\ State28 {},
  Msg28 {} /\ State29 {},
  Msg29 {} /\ State30 {},
  Msg30 {} /\ State01 {}
  ]

