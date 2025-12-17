module Test.Examples.DoorWithPin
  ( main
  , spec
  , update
  , DoorWithPinTransit
  , State(..)
  , Msg(..)
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Traversable (for_)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Examples.Common (assertWalk, (~>))
import Test.Spec (Spec, describe, it)
import Transit (type (:*), type (:?), type (:@), type (>|), Empty, Transit, match, mkUpdate, return, returnVia)
import Transit.Colors (themeHarmonyDark, themeHarmonyLight)
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data StateD
  = DoorOpen
  | DoorClosed
  | DoorLocked { pin :: String }

data MsgD
  = Close
  | Open
  | Lock { newPin :: String }
  | Unlock { enteredPin :: String }

--------------------------------------------------------------------------------
--- Classic Approach
--------------------------------------------------------------------------------

updateClassic :: StateD -> MsgD -> StateD
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

type State = Variant
  ( "DoorOpen" :: {}
  , "DoorClosed" :: {}
  , "DoorLocked" :: { pin :: String }
  )

type Msg = Variant
  ( "Close" :: {}
  , "Open" :: {}
  , "Lock" :: { newPin :: String }
  , "Unlock" :: { enteredPin :: String }
  )

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
update = mkUpdate @DoorWithPinTransit
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
  assertWalk update
    (v @"DoorOpen")
    [ v @"Close" ~> v @"DoorClosed"
    , v @"Open" ~> v @"DoorOpen"
    , v @"Close" ~> v @"DoorClosed"
    , v @"Lock" { newPin: "1234" }
        ~> v @"DoorLocked" { pin: "1234" }
    , v @"Unlock" { enteredPin: "abcd" }
        ~> v @"DoorLocked" { pin: "1234" }
    , v @"Unlock" { enteredPin: "1234" }
        ~> v @"DoorClosed"
    , v @"Open" ~> v @"DoorOpen"
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
