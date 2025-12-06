module Test.Examples.Door (main, spec) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.Spec (Spec)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Transit.Gen.Graphviz as TransitGraphviz
import Transit.Gen.TransitionTable as TransitTable
import Transit.StateGraph (mkStateGraph)
import Type.Function (type ($))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- Types
--------------------------------------------------------------------------------

data State = DoorOpen | DoorClosed

data Msg = Close | Open

--------------------------------------------------------------------------------
--- TraditionalUpdate
--------------------------------------------------------------------------------

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  _, _ -> state

updateClassic_flawed :: State -> Msg -> State
updateClassic_flawed state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorClosed
  _, _ -> state

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

type DoorDSL =
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")

update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

spec :: Spec Unit
spec = do
  pure unit

-- describe "Dead ends" do
--   it "should be empty" do
--     let r = reflectType (Proxy @DoorDSL)
--     let states = R.getStates r
--     let deadEnds = Array.filter (\x -> R.getOutgoing x r == []) states
--     deadEnds `shouldEqual` []

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    g = mkStateGraph (reflectType (Proxy @DoorDSL))

  TransitGraphviz.writeToFile_ g "graphs/door.dot"
  TransitTable.writeToFile_ g "graphs/door.html"

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
