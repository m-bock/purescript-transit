module Transit.DotLang where

import Prelude

import Color (Color)
import Color as Color
import Data.String as Str

class ToText a where
  toText :: a -> String

newtype GraphvizGraph = GraphvizGraph
  { global :: Array Attr
  , nodes :: Array Node
  , edges :: Array Edge
  }

data Node = Node String (Array Attr)

data Edge = Edge String String (Array Attr)

data Attr = Attr String Value

data Value = Value String | ValueColor Color

instance ToText GraphvizGraph where
  toText (GraphvizGraph { global, nodes, edges }) =
    "digraph " <> toText global <> " {" <> "" <> Str.joinWith "\n" (map toText nodes <> map toText edges) <> "}"

instance ToText Node where
  toText (Node stateName attrs) = stateName <> " [" <> toText attrs <> "]"

instance ToText Edge where
  toText (Edge from to attrs) = from <> " -> " <> to <> " [" <> toText attrs <> "]"

instance ToText (Array Attr) where
  toText attrs = Str.joinWith ", " (map toText attrs)

instance ToText Attr where
  toText (Attr name value) = name <> " = " <> toText value

instance ToText Value where
  toText (Value str) = "\"" <> str <> "\""
  toText (ValueColor color) = "\"" <> Color.toHexString color <> "\""