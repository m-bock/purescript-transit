module Examples.Classic.Door (State(..), Msg(..), update, spec) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Examples.Common (assertWalk, (~>))
import Test.Spec (Spec, describe, it)

--------------------------------------------------------------------------------
--- Classic Approach
--------------------------------------------------------------------------------

data State
  = DoorOpen
  | DoorClosed

data Msg
  = Close
  | Open

update :: State -> Msg -> State
update state msg =
  case state, msg of
    DoorOpen, Close -> DoorClosed
    DoorClosed, Open -> DoorOpen
    _, _ -> state

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

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  describe "Classic Door" do
    it "follows the walk and visits the expected intermediate states" do
      assertWalk update
        DoorOpen
        [ Close ~> DoorClosed
        , Open ~> DoorOpen
        , Close ~> DoorClosed
        , Close ~> DoorClosed
        , Open ~> DoorOpen
        , Open ~> DoorOpen
        ]
