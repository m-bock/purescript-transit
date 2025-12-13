module Test.Examples.DoorWithPin (main, spec, DoorTransit, State(..), Msg(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_, scanl)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:?), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return, returnVia)
import Transit.Colors (themeHarmonyDark, themeHarmonyLight)
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data State
  = DoorOpen
  | DoorClosed
  | DoorLocked { pin :: String }

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
  DoorClosed, Lock { newPin } -> DoorLocked { pin: newPin }
  DoorLocked { pin }, Unlock { enteredPin } ->
    if pin == enteredPin then
      DoorClosed
    else
      DoorLocked { pin }
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
      )

update :: State -> Msg -> State
update = mkUpdateGeneric @DoorTransit
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
  ( match @"DoorClosed" @"Lock" \_ msg ->
      return @"DoorLocked" { pin: msg.newPin }
  )
  ( match @"DoorLocked" @"Unlock" \state msg ->
      if state.pin == msg.enteredPin then
        returnVia @"PinCorrect" @"DoorClosed"
      else
        returnVia @"PinIncorrect" @"DoorLocked" { pin: state.pin }
  )

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

spec :: Spec Unit
spec = describe "" do
  let

    initState = DoorOpen

    walk =
      [ { msg: Close
        , state: DoorClosed
        }
      , { msg: Open
        , state: DoorOpen
        }
      , { msg: Close
        , state: DoorClosed
        }
      , { msg: Lock { newPin: "1234" }
        , state: DoorLocked { pin: "1234" }
        }
      , { msg: Unlock { enteredPin: "abcd" }
        , state: DoorLocked { pin: "1234" }
        }
      , { msg: Unlock { enteredPin: "1234" }
        , state: DoorClosed
        }
      , { msg: Open
        , state: DoorOpen
        }
      ]

    msgs = map _.msg walk
    expectedStates = map _.state walk

  for_ [ updateClassic, update ] \updateFn ->
    it "should follow the walk" do
      let
        actualStates = scanl updateFn initState msgs
      actualStates `shouldEqual` expectedStates

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @DoorTransit)

  for_
    [ { theme: themeHarmonyLight, file: "graphs/door-with-pin-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/door-with-pin-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { title = "Door with Pin"
        , theme = opts.theme
        , entryPoints = [ "DoorOpen" ]
        }

  TransitTable.writeToFile "graphs/door-with-pin.html" transit _
    { title = "Door with Pin" }

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
