module Test.Examples.DoorWithPin where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Empty, Wrap, match, mkUpdateGeneric, return, return_)
import Transit.Gen.Graphviz as TransitGraphviz
import Type.Function (type ($))

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

type DoorDSL =
  Wrap $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :*
      ( "DoorLocked" :@ "Unlock"
          >| "DoorClosed"
          >| "DoorLocked"
      )

update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  (match @"DoorOpen" @"Close" \_ _ -> return_ @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return_ @"DoorOpen")
  (match @"DoorClosed" @"Lock" \_ { newPin } -> return @"DoorLocked" { pin: newPin })
  ( match @"DoorLocked" @"Unlock" \{ pin } { enteredPin } ->
      if pin == enteredPin then
        return_ @"DoorClosed"
      else
        return @"DoorLocked" { pin }
  )

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

mkTest :: (State -> Msg -> State) -> Spec Unit
mkTest updateFn = do
  describe "purescript-spec" do
    it "awesome" do
      updateFn DoorOpen Close `shouldEqual` DoorClosed
      updateFn DoorOpen Open `shouldEqual` DoorOpen
      updateFn DoorClosed Open `shouldEqual` DoorOpen
      updateFn DoorClosed Close `shouldEqual` DoorClosed

spec :: Spec Unit
spec = do
  mkTest updateClassic
  mkTest update

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  TransitGraphviz.writeToFile_ @DoorDSL "graphs/door-with-pin.dot"

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
