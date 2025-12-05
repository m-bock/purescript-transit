module Test.Examples.DoorWithLock where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Empty, Wrap, match, mkUpdateGeneric, return', return_)
import Transit.Gen.Graphviz as TransitGraphviz
import Type.Function (type ($))

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data State
  = DoorOpen
  | DoorClosed
  | DoorLocked

data Msg
  = Close
  | Open
  | Lock
  | Unlock

--------------------------------------------------------------------------------
--- TraditionalUpdate
--------------------------------------------------------------------------------

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  DoorClosed, Lock -> DoorLocked
  DoorLocked, Unlock -> DoorClosed
  _, _ -> state

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

type DoorDSL =
  Wrap $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :* ("DoorLocked" :@ "Unlock" >| "DoorClosed")

update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  (match @"DoorOpen" @"Close" \_ _ -> return' @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return' @"DoorOpen")
  (match @"DoorClosed" @"Lock" \_ _ -> return' @"DoorLocked")
  (match @"DoorLocked" @"Unlock" \_ _ -> return' @"DoorClosed")

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

-- main :: Effect Unit
-- main = do
--   TransitGraphviz.writeToFile_ @DoorDSL "graphs/door-with-lock.dot"

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
