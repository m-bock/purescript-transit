module Test.Examples.SimpleDoor (main, spec, SimpleDoorTransit, State(..), Msg(..)) where

import Prelude

import Data.Foldable (foldl, for_)
import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Data.Traversable (scanl)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Transit.Colors (themeHarmonyDark, themeHarmonyLight)
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
--- Traditional Approach
--------------------------------------------------------------------------------

updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  _, _ -> state

--------------------------------------------------------------------------------
--- Transit Approach
--------------------------------------------------------------------------------

type SimpleDoorTransit =
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")

update :: State -> Msg -> State
update = mkUpdateGeneric @SimpleDoorTransit
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

spec1 :: Spec Unit
spec1 = describe "should follow the walk" do
  it "classic update" do
    foldl update DoorOpen [ Close, Open, Close ]
      `shouldEqual` DoorClosed

    foldl updateClassic DoorOpen [ Close, Open, Close ]
      `shouldEqual` DoorClosed

spec2 :: Spec Unit
spec2 = describe "" do
  let

    initState = DoorOpen

    walk =
      [ { msg: Close, state: DoorClosed }
      , { msg: Open, state: DoorOpen }
      , { msg: Open, state: DoorOpen }
      , { msg: Close, state: DoorClosed }
      , { msg: Open, state: DoorOpen }
      , { msg: Close, state: DoorClosed }
      , { msg: Close, state: DoorClosed }
      ]

    msgs = map _.msg walk
    expectedStates = map _.state walk

  for_ [ updateClassic, update ] \updateFn ->
    it "should follow the walk" do
      let
        actualStates = scanl updateFn initState msgs
      actualStates `shouldEqual` expectedStates

spec :: Spec Unit
spec = do
  describe "SimpleDoor" do
    spec1
    spec2

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

    title = "Simple Door State Machine"

  TransitGraphviz.writeToFile "graphs/simple-door-light.dot" transit _
    { title = title
    , theme = themeHarmonyLight
    }

  TransitGraphviz.writeToFile "graphs/simple-door-dark.dot" transit _
    { title = title
    , theme = themeHarmonyDark
    }

  TransitTable.writeToFile "graphs/simple-door.html" transit _
    { title = title }

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
