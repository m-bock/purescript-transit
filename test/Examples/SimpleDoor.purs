module Test.Examples.SimpleDoor (main, spec, SimpleDoorTransit, State(..), Msg(..)) where

import Prelude

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Data.Traversable (scanl)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
import Test.Examples.Common (assertWalk, (~>))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Transit, match, mkUpdate, return)
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Function (type ($))
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- Classic Approach
--------------------------------------------------------------------------------

data StateD = DoorOpen | DoorClosed

data MsgD = Close | Open

updateD :: StateD -> MsgD -> StateD
updateD state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  _, _ -> state

--------------------------------------------------------------------------------
--- Transit Approach
--------------------------------------------------------------------------------

type State = Variant
  ( "DoorOpen" :: {}
  , "DoorClosed" :: {}
  )

type Msg = Variant
  ( "Close" :: {}
  , "Open" :: {}
  )

type SimpleDoorTransit =
  Transit
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")

update :: State -> Msg -> State
update = mkUpdate @SimpleDoorTransit
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

assert1 :: Aff Unit
assert1 =
  foldl update (v @"DoorOpen") [ v @"Close", v @"Open", v @"Close" ]
    `shouldEqual`
      (v @"DoorClosed")

assert2 :: Aff Unit
assert2 =
  scanl update (v @"DoorOpen") [ v @"Close", v @"Open", v @"Close" ]
    `shouldEqual`
      [ v @"DoorClosed", v @"DoorOpen", v @"DoorClosed" ]

assert3 :: Aff Unit
assert3 =
  assertWalk update
    (v @"DoorOpen")
    [ v @"Close" ~> v @"DoorClosed"
    , v @"Open" ~> v @"DoorOpen"
    , v @"Close" ~> v @"DoorClosed"
    , v @"Close" ~> v @"DoorClosed"
    , v @"Open" ~> v @"DoorOpen"
    , v @"Open" ~> v @"DoorOpen"
    , v @"Open" ~> v @"DoorOpen"
    ]

spec :: Spec Unit
spec = do
  describe "SimpleDoor" do
    it "Tests" do
      assert1
      assert2
      assert3

--------------------------------------------------------------------------------
--- Diagram and Table generation
--------------------------------------------------------------------------------

generateStateDiagram :: Effect Unit
generateStateDiagram = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

  TransitGraphviz.writeToFile "graphs/simple-door-light.dot" transit _
    { theme = themeHarmonyLight
    }

  TransitGraphviz.writeToFile "graphs/simple-door-dark.dot" transit _
    { theme = themeHarmonyDark
    }

generateTransitionTable :: Effect Unit
generateTransitionTable = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

  TransitTable.writeToFile "graphs/simple-door.html" transit identity

main :: Effect Unit
main = do
  generateStateDiagram
  generateTransitionTable

--------------------------------------------------------------------------------
--- Instances
--------------------------------------------------------------------------------

derive instance Eq StateD
derive instance Eq MsgD

derive instance Generic StateD _
derive instance Generic MsgD _

instance Show StateD where
  show = genericShow

instance Show MsgD where
  show = genericShow
