module Test.Bench.Classic.Size030 where

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
  | State011 {}
  | State012 {}
  | State013 {}
  | State014 {}
  | State015 {}
  | State016 {}
  | State017 {}
  | State018 {}
  | State019 {}
  | State020 {}
  | State021 {}
  | State022 {}
  | State023 {}
  | State024 {}
  | State025 {}
  | State026 {}
  | State027 {}
  | State028 {}
  | State029 {}
  | State030 {}

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
  State011 {} -> "State011"
  State012 {} -> "State012"
  State013 {} -> "State013"
  State014 {} -> "State014"
  State015 {} -> "State015"
  State016 {} -> "State016"
  State017 {} -> "State017"
  State018 {} -> "State018"
  State019 {} -> "State019"
  State020 {} -> "State020"
  State021 {} -> "State021"
  State022 {} -> "State022"
  State023 {} -> "State023"
  State024 {} -> "State024"
  State025 {} -> "State025"
  State026 {} -> "State026"
  State027 {} -> "State027"
  State028 {} -> "State028"
  State029 {} -> "State029"
  State030 {} -> "State030"

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
  | Msg011 {}
  | Msg012 {}
  | Msg013 {}
  | Msg014 {}
  | Msg015 {}
  | Msg016 {}
  | Msg017 {}
  | Msg018 {}
  | Msg019 {}
  | Msg020 {}
  | Msg021 {}
  | Msg022 {}
  | Msg023 {}
  | Msg024 {}
  | Msg025 {}
  | Msg026 {}
  | Msg027 {}
  | Msg028 {}
  | Msg029 {}
  | Msg030 {}

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
  Msg011 {} -> "Msg011"
  Msg012 {} -> "Msg012"
  Msg013 {} -> "Msg013"
  Msg014 {} -> "Msg014"
  Msg015 {} -> "Msg015"
  Msg016 {} -> "Msg016"
  Msg017 {} -> "Msg017"
  Msg018 {} -> "Msg018"
  Msg019 {} -> "Msg019"
  Msg020 {} -> "Msg020"
  Msg021 {} -> "Msg021"
  Msg022 {} -> "Msg022"
  Msg023 {} -> "Msg023"
  Msg024 {} -> "Msg024"
  Msg025 {} -> "Msg025"
  Msg026 {} -> "Msg026"
  Msg027 {} -> "Msg027"
  Msg028 {} -> "Msg028"
  Msg029 {} -> "Msg029"
  Msg030 {} -> "Msg030"

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
  State010 {}, Msg010 {} -> State011 {}
  State011 {}, Msg011 {} -> State012 {}
  State012 {}, Msg012 {} -> State013 {}
  State013 {}, Msg013 {} -> State014 {}
  State014 {}, Msg014 {} -> State015 {}
  State015 {}, Msg015 {} -> State016 {}
  State016 {}, Msg016 {} -> State017 {}
  State017 {}, Msg017 {} -> State018 {}
  State018 {}, Msg018 {} -> State019 {}
  State019 {}, Msg019 {} -> State020 {}
  State020 {}, Msg020 {} -> State021 {}
  State021 {}, Msg021 {} -> State022 {}
  State022 {}, Msg022 {} -> State023 {}
  State023 {}, Msg023 {} -> State024 {}
  State024 {}, Msg024 {} -> State025 {}
  State025 {}, Msg025 {} -> State026 {}
  State026 {}, Msg026 {} -> State027 {}
  State027 {}, Msg027 {} -> State028 {}
  State028 {}, Msg028 {} -> State029 {}
  State029 {}, Msg029 {} -> State030 {}
  State030 {}, Msg030 {} -> State001 {}
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
  , Msg010 {} /\ State011 {}
  , Msg011 {} /\ State012 {}
  , Msg012 {} /\ State013 {}
  , Msg013 {} /\ State014 {}
  , Msg014 {} /\ State015 {}
  , Msg015 {} /\ State016 {}
  , Msg016 {} /\ State017 {}
  , Msg017 {} /\ State018 {}
  , Msg018 {} /\ State019 {}
  , Msg019 {} /\ State020 {}
  , Msg020 {} /\ State021 {}
  , Msg021 {} /\ State022 {}
  , Msg022 {} /\ State023 {}
  , Msg023 {} /\ State024 {}
  , Msg024 {} /\ State025 {}
  , Msg025 {} /\ State026 {}
  , Msg026 {} /\ State027 {}
  , Msg027 {} /\ State028 {}
  , Msg028 {} /\ State029 {}
  , Msg029 {} /\ State030 {}
  , Msg030 {} /\ State001 {}
  ]
