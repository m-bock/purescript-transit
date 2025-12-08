module Test.Examples.DoorWithLock (main, spec) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Transit.Core (TransitCore_)
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
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :* ("DoorLocked" :@ "Unlock" >| "DoorClosed")

update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")
  (match @"DoorClosed" @"Lock" \_ _ -> return @"DoorLocked")
  (match @"DoorLocked" @"Unlock" \_ _ -> return @"DoorClosed")

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
  let
    transit :: TransitCore_
    transit = reflectType (Proxy @DoorDSL)

  TransitGraphviz.writeToFile (_ { title = "Door with Lock" }) transit "graphs/door-with-lock.dot"
  TransitTable.writeToFile (_ { title = "Door with Lock" }) transit "graphs/door-with-lock.html"

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
