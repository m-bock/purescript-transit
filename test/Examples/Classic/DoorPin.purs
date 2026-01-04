module Examples.Classic.DoorPin (State(..), Msg(..), update, spec) where

import Prelude

import Examples.Common (assertWalk, (~>))
import Test.Spec (Spec, describe, it)
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)

data State
  = DoorOpen
  | DoorClosed
  | DoorLocked { storedPin :: String }

derive instance Eq State
derive instance Ord State

derive instance Generic State _

instance Show State where
  show = genericShow

data Msg
  = Close
  | Open
  | Lock { newPin :: String }
  | Unlock { enteredPin :: String }

update :: State -> Msg -> State
update state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  DoorClosed, Lock { newPin } -> DoorLocked { storedPin: newPin }
  DoorLocked { storedPin }, Unlock { enteredPin } ->
    if storedPin == enteredPin then
      DoorClosed
    else
      DoorLocked { storedPin }
  _, _ -> state

spec1 :: Spec Unit
spec1 = do
  it "should follow the walk and visit the expected intermediate states" do
    assertWalk update
      DoorOpen
      [ Close ~> DoorClosed
      , Open ~> DoorOpen
      , Close ~> DoorClosed
      , Close ~> DoorClosed
      , Open ~> DoorOpen
      , Open ~> DoorOpen
      ]

spec :: Spec Unit
spec = do
  describe "DoorPin" do
    spec1