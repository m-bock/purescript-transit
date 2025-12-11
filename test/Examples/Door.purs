module Test.Examples.Door (main, spec, DoorDSL) where

import Prelude

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Examples.Common (checkWalk)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
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

-- f :: Array ({ msg :: Msg, state :: State }) -> State -> Aff Unit
-- f xs state = 

checkSimpleWalk :: Aff Unit
checkSimpleWalk = do
  let
    initState = DoorOpen

    walk =
      [ Close, Open, Open, Close, Open, Close, Close ]

    finalStateA = foldl updateClassic initState walk
    finalStateB = foldl update initState walk

  finalStateA `shouldEqual` DoorClosed
  finalStateB `shouldEqual` DoorClosed

spec :: Spec Unit
spec = do
  describe "Door" do
    it "should follow the simple walk" do
      checkSimpleWalk

    it "should follow the  walk" do
      let
        walk =
          { initialState: DoorOpen
          , steps:
              [ { msg: Close, state: DoorClosed }
              , { msg: Open, state: DoorOpen }
              , { msg: Open, state: DoorOpen }
              , { msg: Close, state: DoorClosed }
              , { msg: Open, state: DoorOpen }
              , { msg: Close, state: DoorClosed }
              , { msg: Close, state: DoorClosed }
              ]
          }

      checkWalk updateClassic walk
      checkWalk update walk

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] spec

  let
    transit = reflectType (Proxy @DoorDSL)

  TransitGraphviz.writeToFile "graphs/door.dot" transit _
    { title = "Door" }

  TransitTable.writeToFile "graphs/door.html" transit _
    { title = "Door" }

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
