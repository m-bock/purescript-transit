module Test.Bench.Classic.Size010 where

import Prelude

import Data.Tuple.Nested (type (/\), (/\))

data StateD
  = State001 {}
  | State002 {}
  | State003 {}
  | State004 {}
  | State005 {}
  | State006 {}
  | State007 {}
  | State008 {}
  | State009 {}
  | State010 {}

derive instance Eq StateD

printStateClassic :: StateD -> String
printStateClassic = case _ of
  State001 {} -> "State001"
  State002 {} -> "State002"
  State003 {} -> "State003"
  State004 {} -> "State004"
  State005 {} -> "State005"
  State006 {} -> "State006"
  State007 {} -> "State007"
  State008 {} -> "State008"
  State009 {} -> "State009"
  State010 {} -> "State010"

initClassic :: StateD
initClassic = State001 {}

data MsgD
  = Msg001 {}
  | Msg002 {}
  | Msg003 {}
  | Msg004 {}
  | Msg005 {}
  | Msg006 {}
  | Msg007 {}
  | Msg008 {}
  | Msg009 {}
  | Msg010 {}

derive instance Eq MsgD

printMsgClassic :: MsgD -> String
printMsgClassic = case _ of
  Msg001 {} -> "Msg001"
  Msg002 {} -> "Msg002"
  Msg003 {} -> "Msg003"
  Msg004 {} -> "Msg004"
  Msg005 {} -> "Msg005"
  Msg006 {} -> "Msg006"
  Msg007 {} -> "Msg007"
  Msg008 {} -> "Msg008"
  Msg009 {} -> "Msg009"
  Msg010 {} -> "Msg010"

updateClassic :: StateD -> MsgD -> StateD
updateClassic state msg = case state, msg of
  State001 {}, Msg001 {} -> State002 {}
  State002 {}, Msg002 {} -> State003 {}
  State003 {}, Msg003 {} -> State004 {}
  State004 {}, Msg004 {} -> State005 {}
  State005 {}, Msg005 {} -> State006 {}
  State006 {}, Msg006 {} -> State007 {}
  State007 {}, Msg007 {} -> State008 {}
  State008 {}, Msg008 {} -> State009 {}
  State009 {}, Msg009 {} -> State010 {}
  State010 {}, Msg010 {} -> State001 {}
  _, _ -> state

walkClassic :: Array (MsgD /\ StateD)
walkClassic =
  [ Msg001 {} /\ State002 {}
  , Msg002 {} /\ State003 {}
  , Msg003 {} /\ State004 {}
  , Msg004 {} /\ State005 {}
  , Msg005 {} /\ State006 {}
  , Msg006 {} /\ State007 {}
  , Msg007 {} /\ State008 {}
  , Msg008 {} /\ State009 {}
  , Msg009 {} /\ State010 {}
  , Msg010 {} /\ State001 {}
  ]
