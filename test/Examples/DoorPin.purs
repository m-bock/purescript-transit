module Examples.DoorPin
  ( main
  , spec
  , update
  , DoorPinTransit
  , State(..)
  , Msg(..)
  ) where

import Prelude

import Data.Reflectable (reflectType)
import Data.Variant (Variant)
import Effect (Effect)
import Examples.Common (assertWalk, (~>))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Transit (type (:*), type (:?), type (:@), type (>|), Transit, TransitCore, match, mkUpdate, return, returnVia)
import Transit.Data.DotLang (GraphvizGraph)
import Transit.Data.DotLang as Graphviz
import Transit.Data.Table (Table)
import Transit.Data.Table as Table
import Transit.Render.Graphviz (Layout(..))
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeHarmonyDark, themeHarmonyLight)
import Transit.Render.TransitionTable as TransitTable
import Transit.VariantUtils (v)
import Type.Proxy (Proxy(..))

type State = Variant
  ( "DoorOpen" :: {}
  , "DoorClosed" :: {}
  , "DoorLocked" :: { storedPin :: String }
  )

type Msg = Variant
  ( "Close" :: {}
  , "Open" :: {}
  , "Lock" :: { newPin :: String }
  , "Unlock" :: { enteredPin :: String }
  )

type DoorPinTransit =
  Transit
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :*
      ( "DoorLocked" :@ "Unlock"
          >| ("PinCorrect" :? "DoorClosed")
          >| ("PinIncorrect" :? "DoorLocked")
      )

update :: State -> Msg -> State
update = mkUpdate @DoorPinTransit
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
  ( match @"DoorClosed" @"Lock" \_ msg ->
      return @"DoorLocked" { storedPin: msg.newPin }
  )
  ( match @"DoorLocked" @"Unlock" \state msg ->
      let
        isCorrect = state.storedPin == msg.enteredPin
      in
        if isCorrect then
          returnVia @"PinCorrect" @"DoorClosed"
        else
          returnVia @"PinIncorrect" @"DoorLocked" { storedPin: state.storedPin }
  )

doorPinTransit :: TransitCore
doorPinTransit = reflectType (Proxy @DoorPinTransit)

--------------------------------------------------------------------------------
--- Tests
--------------------------------------------------------------------------------

specWalk :: Spec Unit
specWalk =
  it "should follow the walk and visit the expected intermediate states" do
    assertWalk update
      (v @"DoorOpen")
      [ v @"Close" ~> v @"DoorClosed"
      , v @"Lock" { newPin: "1234" } ~> v @"DoorLocked" { storedPin: "1234" }
      , v @"Unlock" { enteredPin: "abcd" } ~> v @"DoorLocked" { storedPin: "1234" }
      , v @"Unlock" { enteredPin: "1234" } ~> v @"DoorClosed"
      , v @"Open" ~> v @"DoorOpen"
      ]

spec :: Spec Unit
spec = describe "DoorPin" do
  specWalk

--------------------------------------------------------------------------------
--- Diagram and Table generation
--------------------------------------------------------------------------------

generateGraphLight :: Effect Unit
generateGraphLight = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate doorPinTransit \cfg -> cfg
      { theme = themeHarmonyLight
      , entryPoints = [ "DoorOpen" ]
      , layout = Landscape
      }

  FS.writeTextFile UTF8 "renders/door-pin_graph-light.dot" (Graphviz.toDotStr graph)

generateGraphDark :: Effect Unit
generateGraphDark = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate doorPinTransit \cfg -> cfg
      { theme = themeHarmonyDark
      , entryPoints = [ "DoorOpen" ]
      , layout = Landscape
      }

  FS.writeTextFile UTF8 "renders/door-pin_graph-dark.dot" (Graphviz.toDotStr graph)

generateTable :: Effect Unit
generateTable = do
  let
    table :: Table
    table = TransitTable.generate_ doorPinTransit

  FS.writeTextFile UTF8 "renders/door-pin_table.md" (Table.toMarkdown table)

main :: Effect Unit
main = do
  generateGraphLight
  generateGraphDark
  generateTable