module Test.Transit.Data.DotLang
  ( spec
  ) where

import Prelude

import Color (rgb)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Data.DotLang as DL

spec :: Spec Unit
spec = do
  describe "Transit.Data.DotLang" do
    describe "ToText Value" do
      it "converts Value String to quoted string" do
        DL.toText (DL.Value "test") `shouldEqual` "\"test\""

      it "converts ValueInt to unquoted number" do
        DL.toText (DL.ValueInt 42) `shouldEqual` "42"

      it "converts ValueNumber to unquoted number" do
        DL.toText (DL.ValueNumber 3.14) `shouldEqual` "3.14"

      it "converts ValueBoolean true to string" do
        DL.toText (DL.ValueBoolean true) `shouldEqual` "true"

      it "converts ValueBoolean false to string" do
        DL.toText (DL.ValueBoolean false) `shouldEqual` "false"

      it "converts HtmlLabel to angle brackets" do
        DL.toText (DL.HtmlLabel "<b>bold</b>") `shouldEqual` "<<b>bold</b>>"

      it "converts ValueColors to quoted hex string" do
        let red = rgb 255 0 0
        DL.toText (DL.ValueColors [ red ]) `shouldEqual` "\"#ff0000\""

      it "converts ValueColors with multiple colors" do
        let
          red = rgb 255 0 0
          blue = rgb 0 0 255
        DL.toText (DL.ValueColors [ red, blue ]) `shouldEqual` "\"#ff0000:#0000ff\""

    describe "ToText Attr" do
      it "converts Attr to name = value format" do
        DL.toText (DL.Attr "label" (DL.Value "test")) `shouldEqual` "label = \"test\""

      it "converts Attr with ValueInt" do
        DL.toText (DL.Attr "size" (DL.ValueInt 10)) `shouldEqual` "size = 10"

    describe "ToText Array Attr" do
      it "converts empty array to empty string" do
        DL.toText ([] :: Array DL.Attr) `shouldEqual` ""

      it "converts single attr" do
        DL.toText [ DL.Attr "label" (DL.Value "test") ] `shouldEqual` "label = \"test\""

      it "converts multiple attrs with comma separation" do
        DL.toText
          [ DL.Attr "label" (DL.Value "test")
          , DL.Attr "shape" (DL.Value "box")
          ]
          `shouldEqual` "label = \"test\", shape = \"box\""

    describe "ToText Node" do
      it "converts Node with no attributes" do
        DL.toText (DL.Node "State1" []) `shouldEqual` "State1 []"

      it "converts Node with single attribute" do
        DL.toText (DL.Node "State1" [ DL.Attr "label" (DL.Value "State 1") ])
          `shouldEqual` "State1 [label = \"State 1\"]"

      it "converts Node with multiple attributes" do
        DL.toText
          ( DL.Node "State1"
              [ DL.Attr "label" (DL.Value "State 1")
              , DL.Attr "shape" (DL.Value "box")
              ]
          )
          `shouldEqual` "State1 [label = \"State 1\", shape = \"box\"]"

    describe "ToText Edge" do
      it "converts Edge with no attributes" do
        DL.toText (DL.Edge "State1" "State2" []) `shouldEqual` "State1 -> State2 []"

      it "converts Edge with single attribute" do
        DL.toText (DL.Edge "State1" "State2" [ DL.Attr "label" (DL.Value "Msg1") ])
          `shouldEqual` "State1 -> State2 [label = \"Msg1\"]"

      it "converts Edge with multiple attributes" do
        DL.toText
          ( DL.Edge "State1" "State2"
              [ DL.Attr "label" (DL.Value "Msg1")
              , DL.Attr "color" (DL.Value "red")
              ]
          )
          `shouldEqual` "State1 -> State2 [label = \"Msg1\", color = \"red\"]"

    describe "ToText GlobalAttrs" do
      it "converts GlobalAttrs with no attributes" do
        DL.toText (DL.GlobalAttrs []) `shouldEqual` ""

      it "converts GlobalAttrs with single attribute" do
        DL.toText (DL.GlobalAttrs [ DL.Attr "rankdir" (DL.Value "TD") ])
          `shouldEqual` "rankdir = \"TD\""

      it "converts GlobalAttrs with multiple attributes using semicolon" do
        DL.toText
          ( DL.GlobalAttrs
              [ DL.Attr "rankdir" (DL.Value "TD")
              , DL.Attr "fontname" (DL.Value "Arial")
              ]
          )
          `shouldEqual` "rankdir = \"TD\";fontname = \"Arial\""

    describe "ToText Section" do
      it "converts SecNode" do
        DL.toText (DL.SecNode (DL.Node "State1" [ DL.Attr "label" (DL.Value "State 1") ]))
          `shouldEqual` "State1 [label = \"State 1\"]"

      it "converts SecEdge" do
        DL.toText (DL.SecEdge (DL.Edge "State1" "State2" [ DL.Attr "label" (DL.Value "Msg1") ]))
          `shouldEqual` "State1 -> State2 [label = \"Msg1\"]"

      it "converts SecGlobal" do
        DL.toText (DL.SecGlobal (DL.GlobalAttrs [ DL.Attr "rankdir" (DL.Value "TD") ]))
          `shouldEqual` "rankdir = \"TD\""

      it "converts SecGlobalRaw" do
        DL.toText (DL.SecGlobalRaw "graph [layout=sfdp];")
          `shouldEqual` "graph [layout=sfdp];"

    describe "ToText GraphvizGraph" do
      it "converts empty graph" do
        DL.toText (DL.GraphvizGraph [])
          `shouldEqual` "digraph \n{\n}"

      it "converts graph with single node" do
        DL.toText
          ( DL.GraphvizGraph
              [ DL.SecNode (DL.Node "State1" [])
              ]
          )
          `shouldEqual` "digraph \n{\nState1 []\n}"

      it "converts graph with multiple sections" do
        DL.toText
          ( DL.GraphvizGraph
              [ DL.SecGlobal (DL.GlobalAttrs [ DL.Attr "rankdir" (DL.Value "TD") ])
              , DL.SecNode (DL.Node "State1" [ DL.Attr "label" (DL.Value "State 1") ])
              , DL.SecEdge (DL.Edge "State1" "State2" [ DL.Attr "label" (DL.Value "Msg1") ])
              ]
          )
          `shouldEqual` "digraph \n{\nrankdir = \"TD\"\nState1 [label = \"State 1\"]\nState1 -> State2 [label = \"Msg1\"]\n}"

