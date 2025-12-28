-- | Data types and functions for building HTML structures.
-- |
-- | This module provides a simple, type-safe way to construct HTML nodes
-- | and convert them to HTML strings. It's primarily used for generating
-- | transition tables and other HTML output.
module Transit.Data.Html
  ( Attribute(..)
  , Node(..)
  , attrStyle
  , b
  , caption
  , nodeToHtml
  , table
  , tbody
  , td
  , text
  , th
  , thead
  , tr
  ) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.String as Str

-- | An HTML node, either an element with a tag name, attributes, and children,
-- | or a text node containing raw text.
data Node = Node String (Array Attribute) (Array Node) | Text String

-- | An HTML attribute (name-value pair).
data Attribute = Attribute String String

-- | Creates a text node from a string.
text :: String -> Node
text = Text

-- | Creates a table element.
table :: Array Attribute -> Array Node -> Node
table = Node "table"

-- | Creates a thead element.
thead :: Array Attribute -> Array Node -> Node
thead = Node "thead"

-- | Creates a tbody element.
tbody :: Array Attribute -> Array Node -> Node
tbody = Node "tbody"

-- | Creates a th (table header) element.
th :: Array Attribute -> Array Node -> Node
th = Node "th"

-- | Creates a tr (table row) element.
tr :: Array Attribute -> Array Node -> Node
tr = Node "tr"

-- | Creates a td (table cell) element.
td :: Array Attribute -> Array Node -> Node
td = Node "td"

-- | Creates a b (bold) element.
b :: Array Attribute -> Array Node -> Node
b = Node "b"

-- | Creates a caption element.
caption :: Array Attribute -> Array Node -> Node
caption = Node "caption"

-- | Creates a style attribute.
attrStyle :: String -> Attribute
attrStyle styleValue = Attribute "style" styleValue

-- | Converts an HTML node to its string representation.
nodeToHtml :: Node -> String
nodeToHtml = case _ of
  Node name [] [] -> "<" <> name <> "></" <> name <> ">"
  Node name attributes [] -> "<" <> name <> " " <> attrsToHtml attributes <> "></" <> name <> ">"
  Node name [] children -> "<" <> name <> ">" <> Str.joinWith "" (map nodeToHtml children) <> "</" <> name <> ">"
  Node name attributes children -> Str.joinWith ""
    [ "<" <> name <> " " <> attrsToHtml attributes <> ">"
    , Str.joinWith "" (map nodeToHtml children)
    , "</" <> name <> ">"
    ]
  Text textContent -> textContent

-- | Converts an array of attributes to an HTML attribute string.
attrsToHtml :: Array Attribute -> String
attrsToHtml attributes = Str.joinWith " " (map attrToHtml attributes)

-- | Converts an attribute to its HTML string representation.
attrToHtml :: Attribute -> String
attrToHtml (Attribute name value) = name <> "=\"" <> value <> "\""

derive instance Generic Node _
derive instance Generic Attribute _

derive instance Eq Node
derive instance Eq Attribute

instance Show Node where
  show x = genericShow x

instance Show Attribute where
  show = genericShow