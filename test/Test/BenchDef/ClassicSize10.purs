module Test.BenchDef.ClassicSize10 where

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
  State10 {}, Msg10 {} -> State01 {}
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
  Msg10 {} /\ State01 {}
  ]

