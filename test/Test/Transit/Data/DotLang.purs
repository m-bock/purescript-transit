module Test.Transit.Data.DotLang
  ( spec
  ) where

import Prelude

import Color (rgb)
import Data.Maybe (Maybe(..))
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Data.DotLang as DL

spec :: Spec Unit
spec = do
  describe "Transit.Data.DotLang" do
    describe "ToDotStr Value" do
      it "converts Value String to quoted string" do
        DL.toDotStr (DL.Value "test") `shouldEqual` "\"test\""

      it "converts ValueInt to unquoted number" do
        DL.toDotStr (DL.ValueInt 42) `shouldEqual` "42"

      it "converts ValueNumber to unquoted number" do
        DL.toDotStr (DL.ValueNumber 3.14) `shouldEqual` "3.14"

      it "converts ValueBoolean true" do
        DL.toDotStr (DL.ValueBoolean true) `shouldEqual` "true"

      it "converts ValueBoolean false" do
        DL.toDotStr (DL.ValueBoolean false) `shouldEqual` "false"

      it "converts HtmlLabel to angle bracket format" do
        DL.toDotStr (DL.HtmlLabel "<b>bold</b>") `shouldEqual` "<<b>bold</b>>"

      it "converts ValueColors with single color to quoted hex" do
        let red = rgb 255 0 0
        DL.toDotStr (DL.ValueColors [ red ]) `shouldEqual` "\"#ff0000\""

      it "converts ValueColors with multiple colors to colon-separated hex" do
        let
          red = rgb 255 0 0
          blue = rgb 0 0 255
        DL.toDotStr (DL.ValueColors [ red, blue ]) `shouldEqual` "\"#ff0000:#0000ff\""

    describe "ToDotStr Attr" do
      it "converts Attr to name = value format" do
        DL.toDotStr (DL.Attr "label" (DL.Value "test")) `shouldEqual` "label = \"test\""

      it "converts Attr with ValueInt" do
        DL.toDotStr (DL.Attr "size" (DL.ValueInt 10)) `shouldEqual` "size = 10"

    describe "ToDotStr Array Attr" do
      it "converts empty array to empty string" do
        DL.toDotStr ([] :: Array DL.Attr) `shouldEqual` ""

      it "converts single attribute" do
        DL.toDotStr [ DL.Attr "label" (DL.Value "test") ] `shouldEqual` "label = \"test\""

      it "converts multiple attributes with comma separation" do
        DL.toDotStr
          [ DL.Attr "label" (DL.Value "test")
          , DL.Attr "shape" (DL.Value "box")
          ]
          `shouldEqual` "label = \"test\", shape = \"box\""

    describe "ToDotStr Node" do
      it "converts Node with no attributes" do
        DL.toDotStr (DL.Node "State1" Nothing []) `shouldEqual` "State1 []"

      it "converts Node with single attribute" do
        DL.toDotStr (DL.Node "State1" Nothing [ DL.Attr "label" (DL.Value "State 1") ])
          `shouldEqual` "State1 [label = \"State 1\"]"

      it "converts Node with multiple attributes" do
        DL.toDotStr
          ( DL.Node "State1"
              Nothing
              [ DL.Attr "label" (DL.Value "State 1")
              , DL.Attr "shape" (DL.Value "box")
              ]
          )
          `shouldEqual` "State1 [label = \"State 1\", shape = \"box\"]"

    describe "ToDotStr Edge" do
      it "converts Edge with no attributes" do
        DL.toDotStr (DL.Edge "State1" "State2" []) `shouldEqual` "State1 -> State2 []"

      it "converts Edge with single attribute" do
        DL.toDotStr (DL.Edge "State1" "State2" [ DL.Attr "label" (DL.Value "Msg1") ])
          `shouldEqual` "State1 -> State2 [label = \"Msg1\"]"

      it "converts Edge with multiple attributes" do
        DL.toDotStr
          ( DL.Edge "State1" "State2"
              [ DL.Attr "label" (DL.Value "Msg1")
              , DL.Attr "color" (DL.Value "red")
              ]
          )
          `shouldEqual` "State1 -> State2 [label = \"Msg1\", color = \"red\"]"

    describe "ToDotStr GlobalAttrs" do
      it "converts GlobalAttrs with no attributes to empty string" do
        DL.toDotStr (DL.GlobalAttrs []) `shouldEqual` ""

      it "converts GlobalAttrs with single attribute" do
        DL.toDotStr (DL.GlobalAttrs [ DL.Attr "rankdir" (DL.Value "TD") ])
          `shouldEqual` "rankdir = \"TD\""

      it "converts GlobalAttrs with multiple attributes using semicolon separation" do
        DL.toDotStr
          ( DL.GlobalAttrs
              [ DL.Attr "rankdir" (DL.Value "TD")
              , DL.Attr "fontname" (DL.Value "Arial")
              ]
          )
          `shouldEqual` "rankdir = \"TD\";fontname = \"Arial\""

    describe "ToDotStr Section" do
      it "converts SecNode" do
        DL.toDotStr (DL.SecNode (DL.Node "State1" Nothing [ DL.Attr "label" (DL.Value "State 1") ]))
          `shouldEqual` "State1 [label = \"State 1\"]"

      it "converts SecEdge" do
        DL.toDotStr (DL.SecEdge (DL.Edge "State1" "State2" [ DL.Attr "label" (DL.Value "Msg1") ]))
          `shouldEqual` "State1 -> State2 [label = \"Msg1\"]"

      it "converts SecGlobal" do
        DL.toDotStr (DL.SecGlobal (DL.GlobalAttrs [ DL.Attr "rankdir" (DL.Value "TD") ]))
          `shouldEqual` "rankdir = \"TD\""

      it "converts SecGlobalRaw" do
        DL.toDotStr (DL.SecGlobalRaw "graph [layout=sfdp];")
          `shouldEqual` "graph [layout=sfdp];"

    describe "ToDotStr GraphvizGraph" do
      it "converts empty graph" do
        DL.toDotStr (DL.GraphvizGraph [])
          `shouldEqual` "digraph \n{\n}"

      it "converts graph with single node" do
        DL.toDotStr
          ( DL.GraphvizGraph
              [ DL.SecNode (DL.Node "State1" Nothing [])
              ]
          )
          `shouldEqual` "digraph \n{\nState1 []\n}"

      it "converts graph with multiple sections" do
        DL.toDotStr
          ( DL.GraphvizGraph
              [ DL.SecGlobal (DL.GlobalAttrs [ DL.Attr "rankdir" (DL.Value "TD") ])
              , DL.SecNode (DL.Node "State1" Nothing [ DL.Attr "label" (DL.Value "State 1") ])
              , DL.SecEdge (DL.Edge "State1" "State2" [ DL.Attr "label" (DL.Value "Msg1") ])
              ]
          )
          `shouldEqual` "digraph \n{\nrankdir = \"TD\"\nState1 [label = \"State 1\"]\nState1 -> State2 [label = \"Msg1\"]\n}"

