module Test.BenchDef.ClassicSize20 where

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

derive instance Eq StateD

printStateClassic :: StateD -> String
printStateClassic = case _ of
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

initClassic :: StateD
initClassic = State01 {}

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

derive instance Eq MsgD

printMsgClassic :: MsgD -> String
printMsgClassic = case _ of
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
  State20 {}, Msg20 {} -> State01 {}
  _, _ -> state

walkClassic :: Array (MsgD /\ StateD)
walkClassic =
  [ Msg01 {} /\ State02 {}
  , Msg02 {} /\ State03 {}
  , Msg03 {} /\ State04 {}
  , Msg04 {} /\ State05 {}
  , Msg05 {} /\ State06 {}
  , Msg06 {} /\ State07 {}
  , Msg07 {} /\ State08 {}
  , Msg08 {} /\ State09 {}
  , Msg09 {} /\ State10 {}
  , Msg10 {} /\ State11 {}
  , Msg11 {} /\ State12 {}
  , Msg12 {} /\ State13 {}
  , Msg13 {} /\ State14 {}
  , Msg14 {} /\ State15 {}
  , Msg15 {} /\ State16 {}
  , Msg16 {} /\ State17 {}
  , Msg17 {} /\ State18 {}
  , Msg18 {} /\ State19 {}
  , Msg19 {} /\ State20 {}
  , Msg20 {} /\ State01 {}
  ]
