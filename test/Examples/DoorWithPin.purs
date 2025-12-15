module Test.Examples.DoorWithPin (main, spec, DoorWithPinTransit, State(..), Msg(..)) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Examples.Common (assertWalk, (~>))
import Test.Spec (Spec, describe, it)
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
--- Classic Approach
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
--- Transit Approach
--------------------------------------------------------------------------------

type DoorWithPinTransit =
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
update = mkUpdateGeneric @DoorWithPinTransit
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

assert4 :: Aff Unit
assert4 =
  for_ [ updateClassic, update ]
    \fn ->
      assertWalk fn
        DoorOpen
        [ Close ~> DoorClosed
        , Open ~> DoorOpen
        , Lock { newPin: "1234" } ~> DoorLocked { pin: "1234" }
        , Unlock { enteredPin: "abcd" } ~> DoorLocked { pin: "1234" }
        , Unlock { enteredPin: "1234" } ~> DoorClosed
        , Open ~> DoorOpen
        ]

spec :: Spec Unit
spec = describe "DoorWithPin" do
  it "should follow the walk" do
    assert4

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @DoorWithPinTransit)

  for_
    [ { theme: themeHarmonyLight, file: "graphs/door-with-pin-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/door-with-pin-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { title = Just "Door with Pin"
        , theme = opts.theme
        , entryPoints = [ "DoorOpen" ]
        }

  TransitTable.writeToFile "graphs/door-with-pin.html" transit _
    { title = Just "Door with Pin" }

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
