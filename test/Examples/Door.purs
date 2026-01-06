module Examples.Door (main, spec, DoorTransit, State(..), Msg(..)) where

import Prelude

import Data.Foldable (foldl)
import Data.Reflectable (reflectType)
import Data.Traversable (scanl)
import Data.Variant (Variant)
import Effect (Effect)
import Examples.Common (assertWalk, (~>))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Transit, TransitCore, match, mkUpdate, return)
import Transit.Data.DotLang (GraphvizGraph)
import Transit.Data.DotLang (toDotStr) as Graphviz
import Transit.Data.Table (Table)
import Transit.Data.Table as Table
import Transit.Render.Graphviz (Layout(..))
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Prelude (Proxy(..))

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

specWalk1 :: Spec Unit
specWalk1 =
  it "follows the walk and ends in expected final state" do
    foldl update (v @"DoorOpen") [ v @"Close", v @"Open", v @"Close" ]
      `shouldEqual`
        (v @"DoorClosed")

specWalk2 :: Spec Unit
specWalk2 =
  it "follows the walk and visits the expected intermediate states" do
    scanl update (v @"DoorOpen") [ v @"Close", v @"Open", v @"Close" ]
      `shouldEqual`
        [ v @"DoorClosed", v @"DoorOpen", v @"DoorClosed" ]

specWalk3 :: Spec Unit
specWalk3 =
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
  describe "Door" do
    specWalk1
    specWalk2
    specWalk3

--------------------------------------------------------------------------------
--- Diagram and Table generation
--------------------------------------------------------------------------------

generateGraphLight :: Effect Unit
generateGraphLight =
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate doorTransit \def -> def
      { theme = themeHarmonyLight
      , layout = Portrait
      }
  in
    FS.writeTextFile UTF8 "renders/door_graph-light.dot" (Graphviz.toDotStr graph)

generateGraphDark :: Effect Unit
generateGraphDark =
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate doorTransit \def -> def
      { theme = themeHarmonyDark
      , layout = Portrait
      }
  in
    FS.writeTextFile UTF8 "renders/door_graph-dark.dot" (Graphviz.toDotStr graph)

generateTable :: Effect Unit
generateTable = do
  let
    table :: Table
    table = TransitTable.generate doorTransit \def -> def

  FS.writeTextFile UTF8 "renders/door_table.md" (Table.toMarkdown table)

main :: Effect Unit
main = do
  generateGraphLight
  generateGraphDark
  generateTable

