module Test.Transit.Data.Html
  ( spec
  ) where

import Prelude

import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Data.Html (Attribute(..), Node(..), attrStyle, caption, nodeToHtml, table, tbody, td, text, th, thead, tr)

spec :: Spec Unit
spec = do
  describe "Transit.Data.Html" do
    describe "text" do
      it "creates a text node from string" do
        text "Hello" `shouldEqual` Text "Hello"

    describe "attrStyle" do
      it "creates a style attribute" do
        attrStyle "color: red" `shouldEqual` Attribute "style" "color: red"

    describe "nodeToHtml" do
      describe "text nodes" do
        it "converts text node to HTML string" do
          nodeToHtml (text "Hello World") `shouldEqual` "Hello World"

      describe "element nodes" do
        it "converts empty element to empty tag" do
          nodeToHtml (Node "div" [] []) `shouldEqual` "<div></div>"

        it "converts element with single child" do
          nodeToHtml (Node "div" [] [ text "Hello" ]) `shouldEqual` "<div>Hello</div>"

        it "converts element with multiple children" do
          nodeToHtml (Node "div" [] [ text "Hello", text "World" ]) `shouldEqual` "<div>HelloWorld</div>"

        it "converts element with attributes but no children" do
          nodeToHtml (Node "div" [ Attribute "class" "container" ] []) `shouldEqual` "<div class=\"container\"></div>"

        it "converts element with attributes and children" do
          nodeToHtml (Node "div" [ Attribute "class" "container" ] [ text "Hello" ]) `shouldEqual` "<div class=\"container\">Hello</div>"

        it "converts element with multiple attributes and children" do
          nodeToHtml (Node "div" [ Attribute "class" "container", Attribute "id" "main" ] [ text "Hello" ]) `shouldEqual` "<div class=\"container\" id=\"main\">Hello</div>"

        it "converts nested elements" do
          nodeToHtml (Node "div" [] [ Node "span" [] [ text "Hello" ] ]) `shouldEqual` "<div><span>Hello</span></div>"

      describe "table structures" do
        it "converts complex table structure" do
          let
            html = table []
              [ caption [] [ text "My Table" ]
              , thead []
                  [ tr []
                      [ th [] [ text "Header 1" ]
                      , th [] [ text "Header 2" ]
                      ]
                  ]
              , tbody []
                  [ tr []
                      [ td [] [ text "Cell 1" ]
                      , td [] [ text "Cell 2" ]
                      ]
                  ]
              ]
          nodeToHtml html `shouldEqual` "<table><caption>My Table</caption><thead><tr><th>Header 1</th><th>Header 2</th></tr></thead><tbody><tr><td>Cell 1</td><td>Cell 2</td></tr></tbody></table>"

        it "converts table with style attributes" do
          let
            html = table [ attrStyle "border: 1px solid black" ]
              [ tr []
                  [ td [] [ text "Cell" ]
                  ]
              ]
          nodeToHtml html `shouldEqual` "<table style=\"border: 1px solid black\"><tr><td>Cell</td></tr></table>"

