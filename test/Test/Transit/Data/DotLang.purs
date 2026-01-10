module Test.Transit.Data.DotLang
  ( spec
  ) where

import Prelude

import Color (rgb)
import Data.Array (zip, length)
import Data.Foldable (for_)
import Data.String as Str
import Data.Tuple.Nested ((/\))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Data.DotLang (Attr(..), GraphvizGraph(..), Section(..), Value(..))
import Transit.Data.DotLang as DL

assertLinesEqual :: Array String -> Array String -> Spec Unit
assertLinesEqual actualLines expectedLines =
  it "asserts lines are equal" do
    for_ (zip actualLines expectedLines) \(actualLine /\ expectedLine) ->
      actualLine `shouldEqual` expectedLine

    length actualLines `shouldEqual` length expectedLines

spec :: Spec Unit
spec = do
  describe "Transit.Data.DotLang" do
    describe "formats global graph attributes" do
      let
        graph = GraphvizGraph
          [ SecGlobalGraph [ DL.rankDirLR, DL.bgColor (rgb 255 255 255) ]
          ]

        expectedLines =
          [ "digraph {"
          , "  graph ["
          , "    rankdir = LR"
          , "    bgcolor = \"#ffffff\""
          , "  ]"
          , "}"
          ]

        actualLines = Str.split (Str.Pattern "\n") (DL.toDotStr graph)

      assertLinesEqual actualLines expectedLines

    describe "formats global node attributes" do
      let
        red = rgb 255 0 0
        graph = GraphvizGraph
          [ SecGlobalNode
              [ DL.shapeCircle
              , DL.width 0.5
              , DL.fixedSize true
              , DL.fillColor red
              ]
          ]

        expectedLines =
          [ "digraph {"
          , "  node ["
          , "    shape = circle"
          , "    width = 0.5"
          , "    fixedsize = true"
          , "    fillcolor = \"#ff0000\""
          , "  ]"
          , "}"
          ]

        actualLines = Str.split (Str.Pattern "\n") (DL.toDotStr graph)

      assertLinesEqual actualLines expectedLines

    describe "formats global edge attributes" do
      let
        blue = rgb 0 0 255
        graph = GraphvizGraph
          [ SecGlobalEdge [ DL.color blue ]
          ]

        expectedLines =
          [ "digraph {"
          , "  edge ["
          , "    color = \"#0000ff\""
          , "  ]"
          , "}"
          ]

        actualLines = Str.split (Str.Pattern "\n") (DL.toDotStr graph)

      assertLinesEqual actualLines expectedLines

    describe "formats nodes with inline attributes" do
      let
        red = rgb 255 0 0
        blue = rgb 0 0 255
        graph = GraphvizGraph
          [ DL.SecNode
              (DL.Node "Start" [ DL.shapeCircle, DL.width 0.5, DL.fixedSize true, DL.fillColor red ])
          , DL.SecNode
              (DL.Node "Active" [ DL.labelHtmlBold "Active", DL.shapeBox, Attr "fillcolor" (ValueColors [ red, blue ]) ])
          , DL.SecNode
              (DL.Node "End" [ Attr "peripheries" (ValueInt 2) ])
          ]

        expectedLines =
          [ "digraph {"
          , "  Start [shape = circle, width = 0.5, fixedsize = true, fillcolor = \"#ff0000\"]"
          , "  Active [label = <<b>Active</b>>, shape = box, fillcolor = \"#ff0000:#0000ff\"]"
          , "  End [peripheries = 2]"
          , "}"
          ]

        actualLines = Str.split (Str.Pattern "\n") (DL.toDotStr graph)

      assertLinesEqual actualLines expectedLines

    describe "formats edges with inline attributes" do
      let
        blue = rgb 0 0 255
        graph = GraphvizGraph
          [ DL.SecEdge (DL.Edge "Start" "Active" [ DL.label "Begin" ])
          , DL.SecEdge (DL.Edge "Active" "End" [ DL.label "Finish", DL.color blue ])
          ]

        expectedLines =
          [ "digraph {"
          , "  Start -> Active [label = \"Begin\"]"
          , "  Active -> End [label = \"Finish\", color = \"#0000ff\"]"
          , "}"
          ]

        actualLines = Str.split (Str.Pattern "\n") (DL.toDotStr graph)

      assertLinesEqual actualLines expectedLines

