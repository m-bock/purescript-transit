module Transit.Data.Html
  ( Node(..)
  , Attribute(..)
  , text
  , table
  , thead
  , tbody
  , th
  , tr
  , td
  , caption
  , attrStyle
  , nodeToHtml
  , attrsToHtml
  , attrToHtml
  ) where

import Prelude

import Data.String as Str

data Node = Node String (Array Attribute) (Array Node) | Text String

data Attribute = Attribute String String

text :: String -> Node
text = Text

table :: Array Attribute -> Array Node -> Node
table = Node "table"

thead :: Array Attribute -> Array Node -> Node
thead = Node "thead"

tbody :: Array Attribute -> Array Node -> Node
tbody = Node "tbody"

th :: Array Attribute -> Array Node -> Node
th = Node "th"

tr :: Array Attribute -> Array Node -> Node
tr = Node "tr"

td :: Array Attribute -> Array Node -> Node
td = Node "td"

caption :: Array Attribute -> Array Node -> Node
caption = Node "caption"

attrStyle :: String -> Attribute
attrStyle style = Attribute "style" style

nodeToHtml :: Node -> String
nodeToHtml = case _ of
  Node name [] [] -> "<" <> name <> " />"
  Node name [] children -> "<" <> name <> ">" <> Str.joinWith "" (map nodeToHtml children) <> "</" <> name <> ">"
  Node name attributes children -> Str.joinWith ""
    [ "<" <> name <> " " <> attrsToHtml attributes <> ">"
    , Str.joinWith "" (map nodeToHtml children)
    , "</" <> name <> ">"
    ]
  Text text -> text

attrsToHtml :: Array Attribute -> String
attrsToHtml attributes = Str.joinWith " " (map attrToHtml attributes)

attrToHtml :: Attribute -> String
attrToHtml (Attribute name value) = name <> "=\"" <> value <> "\""