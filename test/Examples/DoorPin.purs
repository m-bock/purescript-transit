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
import Transit.Data.DotLang as GraphvizGraph
import Transit.Data.Table (Table)
import Transit.Data.Table as Table
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

assert1 :: Spec Unit
assert1 =
  it "should follow the walk and visit the expected intermediate states" do
    assertWalk update
      -- start in the open state
      (v @"DoorOpen")
      [
        -- close the door, then expect to transition to closed state
        v @"Close" ~> v @"DoorClosed"
      ,
        -- lock the door, then expect to transition to locked state with the given pin
        v @"Lock" { newPin: "1234" } ~> v @"DoorLocked" { storedPin: "1234" }
      ,
        -- unlock the door (with wrong pin), then expect to stay in locked state
        v @"Unlock" { enteredPin: "abcd" } ~> v @"DoorLocked" { storedPin: "1234" }
      ,
        -- unlock the door (with correct pin), then expect to transition to closed state
        v @"Unlock" { enteredPin: "1234" } ~> v @"DoorClosed"
      ,
        -- open the door, then expect to transition to open state
        v @"Open" ~> v @"DoorOpen"
      ]

spec :: Spec Unit
spec = describe "DoorWithPin" do
  assert1

--------------------------------------------------------------------------------
--- State diagram generation
--------------------------------------------------------------------------------

generateStateDiagramLight :: Effect Unit
generateStateDiagramLight = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate doorPinTransit _
      { theme = themeHarmonyLight
      , entryPoints = [ "DoorOpen" ]
      }

  FS.writeTextFile UTF8 "renders/door-pin-light.dot" (GraphvizGraph.toDotStr graph)

generateStateDiagramDark :: Effect Unit
generateStateDiagramDark = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate doorPinTransit _
      { theme = themeHarmonyDark
      , entryPoints = [ "DoorOpen" ]
      }

  FS.writeTextFile UTF8 "renders/door-pin-dark.dot" (GraphvizGraph.toDotStr graph)

generateTransitionTable :: Effect Unit
generateTransitionTable = do
  let
    table :: Table
    table = TransitTable.generate_ doorPinTransit

  FS.writeTextFile UTF8 "renders/door-pin.md" (Table.toMarkdown table)

main :: Effect Unit
main = do
  generateStateDiagramLight
  generateStateDiagramDark
  generateTransitionTable