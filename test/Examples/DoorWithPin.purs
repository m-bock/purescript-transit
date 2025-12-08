module Test.Examples.DoorWithPin (main, spec) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Transit.StateGraph (mkStateGraph)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable

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
  Transit $ Empty
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
        return @"DoorClosed"
      else
        return @"DoorLocked" { pin: state.pin }
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
  let
    transit = reflectType (Proxy @DoorDSL)

  TransitGraphviz.writeToFile (_ { title = "Door with Pin" }) transit "graphs/door-with-pin.dot"
  TransitTable.writeToFile (_ { title = "Door with Pin" }) transit "graphs/door-with-pin.html"

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
