module Examples.Door (main, spec, DoorTransit, State(..), Msg(..)) where

import Prelude

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Data.Traversable (scanl)
import Data.Variant (Variant)
import Effect (Effect)
import Examples.Common (assertWalk, (~>))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Transit, TransitCore, match, mkUpdate, return)
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Prelude (Proxy(..))

--------------------------------------------------------------------------------
--- Classic Approach
--------------------------------------------------------------------------------

data StateD
  = DoorOpen
  | DoorClosed

data MsgD
  = Close
  | Open

updateD :: StateD -> MsgD -> StateD
updateD state msg =
  case state, msg of
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

type DoorTransit =
  Transit
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")

update :: State -> Msg -> State
update = mkUpdate @DoorTransit
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")

doorTransit :: TransitCore
doorTransit = reflectType (Proxy @DoorTransit)

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

spec1 :: Spec Unit
spec1 =
  it "follows the walk and ends in expected final state" do
    foldl update (v @"DoorOpen") [ v @"Close", v @"Open", v @"Close" ]
      `shouldEqual`
        (v @"DoorClosed")

spec2 :: Spec Unit
spec2 =
  it "follows the walk and visits the expected intermediate states" do
    scanl update (v @"DoorOpen") [ v @"Close", v @"Open", v @"Close" ]
      `shouldEqual`
        [ v @"DoorClosed", v @"DoorOpen", v @"DoorClosed" ]

spec3 :: Spec Unit
spec3 =
  it "follows the walk and visits the expected intermediate states" do
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

spec4 :: Spec Unit
spec4 =
  it "follows the walk and visits the expected intermediate states" do
    assertWalk updateD
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
  describe "SimpleDoor" do
    spec1
    spec2
    spec3
    spec4

--------------------------------------------------------------------------------
--- Diagram and Table generation
--------------------------------------------------------------------------------

generateStateDiagramLight :: Effect Unit
generateStateDiagramLight =
  let
    graph = TransitGraphviz.generate doorTransit _
      { theme = themeHarmonyLight
      }
  in
    FS.writeTextFile UTF8 "renders/door-light.dot" graph

generateStateDiagramDark :: Effect Unit
generateStateDiagramDark =
  let
    graph :: String
    graph = TransitGraphviz.generate doorTransit _
      { theme = themeHarmonyDark
      }
  in
    FS.writeTextFile UTF8 "renders/door-dark.dot" graph

generateTransitionTable :: Effect Unit
generateTransitionTable = do
  let
    table :: String
    table = TransitTable.generate doorTransit _
      { outputFormat = TransitTable.Markdown
      }

  FS.writeTextFile UTF8 "renders/door.md" table

main :: Effect Unit
main = do
  generateStateDiagramLight
  generateStateDiagramDark
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

