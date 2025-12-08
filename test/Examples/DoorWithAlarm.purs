module Test.Examples.DoorWithAlarm (main, spec) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return, returnVia)
import Transit.DSL (type (:?))
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Transit.StateGraph (mkStateGraph)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))
import Effect.Class.Console as Console

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data State
  = DoorOpen
  | DoorClosed
  | DoorLocked { pin :: String, attempts :: Int }
  | Alarm

data Msg
  = Close
  | Open
  | Lock { newPin :: String }
  | Unlock { enteredPin :: String }

--------------------------------------------------------------------------------
--- TraditionalUpdate
--------------------------------------------------------------------------------

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  DoorClosed, Lock { newPin } -> DoorLocked { pin: newPin, attempts: 0 }
  DoorLocked { pin, attempts }, Unlock { enteredPin } ->
    if pin == enteredPin then
      DoorClosed
    else if attempts < 3 then
      DoorLocked { pin, attempts: attempts + 1 }
    else
      Alarm
  _, _ -> state

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

type DoorDSL =
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :*
      ( "DoorLocked" :@ "Unlock"
          >| ("PinCorrect" :? "DoorClosed")
          >| ("PinIncorrect" :? "DoorLocked")
          >| ("TooManyAttempts" :? "Alarm")
      )

update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
  ( match @"DoorClosed" @"Lock" \_ msg ->
      return @"DoorLocked" { pin: msg.newPin, attempts: 0 }
  )
  ( match @"DoorLocked" @"Unlock" \state msg ->
      if state.pin == msg.enteredPin then
        returnVia @"PinCorrect" @"DoorClosed"
      else if state.attempts < 3 then
        returnVia @"PinIncorrect" @"DoorLocked" { pin: state.pin, attempts: state.attempts + 1 }
      else
        returnVia @"TooManyAttempts" @"Alarm"
  )

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  pure unit

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    stateGraph = mkStateGraph (reflectType (Proxy @DoorDSL))

  TransitGraphviz.writeToFile (_ { title = "Door with Alarm" }) stateGraph "graphs/door-with-alarm.dot"
  TransitTable.writeToFile (_ { title = "Door with Alarm" }) stateGraph "graphs/door-with-alarm.html"

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive instance Eq State
derive instance Eq Msg

derive instance Generic State _
derive instance Generic Msg _

instance Show State where
  show = genericShow

instance Show Msg where
  show = genericShow
