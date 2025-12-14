module Test.Examples.SimpleDoor (main, spec, SimpleDoorTransit, State(..), Msg(..)) where

import Prelude

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Data.Traversable (for_, scanl)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Examples.Common (assertWalk)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
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

assert1 :: Aff Unit
assert1 =
  (foldl update DoorOpen [ Close, Open, Close ])
    `shouldEqual`
      DoorClosed

assert2 :: Aff Unit
assert2 =
  (scanl update DoorOpen [ Close, Open, Close ])
    `shouldEqual`
      [ DoorClosed, DoorOpen, DoorClosed ]

assert3 :: Aff Unit
assert3 =
  assertWalk update
    DoorOpen
    [ Close /\ DoorClosed
    , Open /\ DoorOpen
    , Close /\ DoorClosed
    ]

assert4 :: Aff Unit
assert4 =
  for_ [ updateClassic, update ]
    \fn ->
      assertWalk fn
        DoorOpen
        [ Close /\ DoorClosed
        , Open /\ DoorOpen
        , Open /\ DoorOpen
        , Close /\ DoorClosed
        , Open /\ DoorOpen
        ]

spec :: Spec Unit
spec = do
  describe "SimpleDoor" do
    it "" do
      assert1
      assert2
      assert3
      assert4

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

  for_
    [ { theme: themeHarmonyLight, file: "graphs/simple-door-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/simple-door-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { theme = opts.theme
        }

  TransitTable.writeToFile "graphs/simple-door.html" transit identity

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
