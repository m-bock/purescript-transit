module Examples.DoorSimple (main, spec, DoorSimpleTransit, State(..), Msg(..)) where

import Prelude

import Data.Foldable (foldl)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Data.Traversable (scanl)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Aff (Aff)
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

type DoorSimpleTransit =
  Transit
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")

update :: State -> Msg -> State
update = mkUpdate @DoorSimpleTransit
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")

doorSimpleTransit :: TransitCore
doorSimpleTransit = reflectType (Proxy @DoorSimpleTransit)

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

spec :: Spec Unit
spec = do
  describe "SimpleDoor" do
    spec1
    spec2
    spec3

--------------------------------------------------------------------------------
--- Diagram and Table generation
--------------------------------------------------------------------------------

generateStateDiagram :: Effect Unit
generateStateDiagram = do
  FS.writeTextFile UTF8 "renders/door-simple-light.dot"
    ( TransitGraphviz.generate doorSimpleTransit _
        { theme = themeHarmonyLight
        }
    )

  FS.writeTextFile UTF8 "renders/door-simple-dark.dot"
    ( TransitGraphviz.generate doorSimpleTransit _
        { theme = themeHarmonyDark
        }
    )

generateTransitionTable :: Effect Unit
generateTransitionTable = do
  FS.writeTextFile UTF8 "renders/door-simple.md"
    (TransitTable.generate doorSimpleTransit _ { outputFormat = TransitTable.Markdown })

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
