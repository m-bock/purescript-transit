module Examples.DoorPin
  ( main
  , spec
  , update
  , DoorPinTransit
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
import Examples.Common (assertWalk, (~>))
import Test.Spec (Spec, describe, it)
import Transit (type (:*), type (:?), type (:@), type (>|), Transit, match, mkUpdate, return, returnVia)
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Proxy (Proxy(..))

--------------------------------------------------------------------------------
--- Classic Approach
--------------------------------------------------------------------------------

data StateD
  = DoorOpen
  | DoorClosed
  | DoorLocked { storedPin :: String }

data MsgD
  = Close
  | Open
  | Lock { newPin :: String }
  | Unlock { enteredPin :: String }

updateClassic :: StateD -> MsgD -> StateD
updateClassic state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  DoorClosed, Lock { newPin } -> DoorLocked { storedPin: newPin }
  DoorLocked { storedPin }, Unlock { enteredPin } ->
    if storedPin == enteredPin then
      DoorClosed
    else
      DoorLocked { storedPin }
  _, _ -> state

--------------------------------------------------------------------------------
--- Transit Approach
--------------------------------------------------------------------------------

type State = Variant
  ( "DoorOpen" :: {}
  , "DoorClosed" :: {}
  , "DoorLocked" :: { storedPin :: String }
  )

type Msg = Variant
  ( "Close" :: {}
  , "Open" :: {}
  , "Lock" :: { newPin :: String }
  , "Unlock" :: { enteredPin :: String }
  )

type DoorPinTransit =
  Transit
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :*
      ( "DoorLocked" :@ "Unlock"
          >| ("PinCorrect" :? "DoorClosed")
          >| ("PinIncorrect" :? "DoorLocked")
      )

update :: State -> Msg -> State
update = mkUpdate @DoorPinTransit
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
  ( match @"DoorClosed" @"Lock" \_ msg ->
      return @"DoorLocked" { storedPin: msg.newPin }
  )
  ( match @"DoorLocked" @"Unlock" \state msg ->
      let
        isCorrect = state.storedPin == msg.enteredPin
      in
        if isCorrect then
          returnVia @"PinCorrect" @"DoorClosed"
        else
          returnVia @"PinIncorrect" @"DoorLocked" { storedPin: state.storedPin }
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
        ~> v @"DoorLocked" { storedPin: "1234" }
    , v @"Unlock" { enteredPin: "abcd" }
        ~> v @"DoorLocked" { storedPin: "1234" }
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
    transit = reflectType (Proxy @DoorPinTransit)

  for_
    [ { theme: themeHarmonyLight, file: "renders/door-pin-light.dot" }
    , { theme: themeHarmonyDark, file: "renders/door-pin-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { title = Just "Door with Pin"
        , theme = opts.theme
        , entryPoints = [ "DoorOpen" ]
        }

  TransitTable.writeToFile "renders/door-pin.md" transit _
    { outputFormat = TransitTable.Markdown
    }
