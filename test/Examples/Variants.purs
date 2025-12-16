module Test.Examples.Variants where

import Prelude

import Data.Symbol (class IsSymbol)
import Data.Traversable (scanl)
import Data.Tuple.Nested (type (/\))
import Data.Variant (Variant)
import Data.Variant as V
import Prim.Row as Row
import Test.Examples.Common ((~>))
import Test.Examples.DoorWithPin (DoorWithPinTransit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (match, mkUpdate, return, returnVia)
import Type.Proxy (Proxy(..))
import Unsafe.Coerce (unsafeCoerce)

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

updateClassic :: State -> Msg -> State
updateClassic state msg =
  ( V.default state
      # on @"DoorOpen"
          ( \_ ->
              ( V.default state
                  # on @"Close" (\_ -> inj @"DoorClosed" {})
              ) msg
          )
      # on @"DoorClosed"
          ( \_ ->
              ( V.default state
                  # on @"Open" (\_ -> inj @"DoorOpen" {})
                  # on @"Lock" (\msg -> inj @"DoorLocked" { pin: msg.newPin })
              ) msg
          )
      # on @"DoorLocked"
          ( \st ->
              ( V.default state
                  # on @"Unlock"
                      ( \msg ->
                          if st.pin == msg.enteredPin then
                            inj @"DoorClosed" {}
                          else
                            inj @"DoorLocked" { pin: st.pin }
                      )
              ) msg
          )
  ) state

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

inj :: forall @sym a r1 r2. Row.Cons sym a r1 r2 => IsSymbol sym => a -> Variant r2
inj = V.inj (Proxy :: _ sym)

on :: forall @sym a b r1 r2. Row.Cons sym a r1 r2 => IsSymbol sym => (a -> b) -> (Variant r1 -> b) -> Variant r2 -> b
on f = V.on (Proxy :: _ sym) f

-- walk :: Array (Msg /\ State)
-- walk =
--   [ inj @"Close" ~> inj @"DoorClosed"
--   , inj @"Open" ~> inj @"DoorOpen"
--   , inj @"Close" ~> inj @"DoorClosed"
--   , inj @"Lock" { newPin: "1234" }
--       ~> inj @"DoorLocked" { pin: "1234" }
--   , inj @"Unlock" { enteredPin: "abcd" }
--       ~> inj @"DoorLocked" { pin: "1234" }
--   , inj @"Unlock" { enteredPin: "1234" }
--       ~> inj @"DoorClosed"
--   , inj @"Open" ~> inj @"DoorOpen"
--   ]

spec :: Spec Unit
spec = do
  describe "Variants" do
    let
      initState :: State
      initState = V.inj (Proxy @"DoorOpen") {}

      walk :: Array { msg :: Msg, state :: State }
      walk =
        [ { msg: inj @"Close" {}
          , state: inj @"DoorClosed" {}
          }
        , { msg: inj @"Open" {}
          , state: inj @"DoorOpen" {}
          }
        , { msg: inj @"Close" {}
          , state: inj @"DoorClosed" {}
          }
        , { msg: inj @"Lock" { newPin: "1234" }
          , state: inj @"DoorLocked" { pin: "1234" }
          }
        , { msg: inj @"Unlock" { enteredPin: "abcd" }
          , state: inj @"DoorLocked" { pin: "1234" }
          }
        , { msg: inj @"Unlock" { enteredPin: "1234" }
          , state: inj @"DoorClosed" {}
          }
        , { msg: inj @"Open" {}
          , state: inj @"DoorOpen" {}
          }
        ]

      msgs :: Array Msg
      msgs = map _.msg walk

      expectedStates :: Array State
      expectedStates = map _.state walk

    describe "classic update" do
      it "should follow the walk" do
        let
          actualStates :: Array State
          actualStates = scanl updateClassic initState msgs
        actualStates `shouldEqual` expectedStates

    describe "transit update" do
      it "should follow the walk" do
        let
          actualStates :: Array State
          actualStates = scanl update initState msgs
        actualStates `shouldEqual` expectedStates
