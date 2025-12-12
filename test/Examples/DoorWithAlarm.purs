module Test.Examples.DoorWithAlarm (main, spec, DoorTransit, State(..), Msg(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Class.Console as Console
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return, returnVia)
import Transit.Colors (themeHarmonyDark, themeHarmonyLight)
import Transit.DSL (type (:?))
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Transit.StateGraph (mkStateGraph)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

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
    let
      pinCorrect = pin == enteredPin
      attemptsExceeded = attempts >= 3
    in
      case pinCorrect, attemptsExceeded of
        true, _ -> DoorClosed
        false, true -> DoorLocked { pin, attempts: attempts + 1 }
        false, false -> Alarm
  _, _ -> state

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

type DoorTransit =
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
update = mkUpdateGeneric @DoorTransit
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
  ( match @"DoorClosed" @"Lock" \_ msg -> return @"DoorLocked"
      { pin: msg.newPin
      , attempts: 0
      }
  )
  ( match @"DoorLocked" @"Unlock" \state msg ->
      let
        pinCorrect = state.pin == msg.enteredPin
        attemptsExceeded = state.attempts >= 3
      in
        case pinCorrect, attemptsExceeded of
          true, _ -> returnVia @"PinCorrect" @"DoorClosed"
          false, true -> returnVia @"PinIncorrect" @"DoorLocked"
            { pin: state.pin
            , attempts: state.attempts + 1
            }
          false, false -> returnVia @"TooManyAttempts" @"Alarm"

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
    transit = reflectType (Proxy @DoorTransit)

  for_
    [ { theme: themeHarmonyLight, file: "graphs/door-with-alarm-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/door-with-alarm-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { title = "Door with Alarm"
        , theme = opts.theme
        }

  TransitTable.writeToFile "graphs/door-with-alarm.html" transit _
    { title = "Door with Alarm" }

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
