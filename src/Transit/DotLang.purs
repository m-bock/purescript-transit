module Transit.DotLang where

import Prelude

import Color (Color)
import Color as Color
import Data.String as Str

class ToText a where
  toText :: a -> String

newtype GraphvizGraph = GraphvizGraph
  { global :: GlobalAttrs
  , nodes :: Array Node
  , edges :: Array Edge
  }

newtype GlobalAttrs = GlobalAttrs (Array Attr)

data Node = Node String (Array Attr)

data Edge = Edge String String (Array Attr)

data Attr = Attr String Value

data Value = Value String | ValueColor Color | ValueInt Int | ValueNumber Number | ValueBoolean Boolean | HtmlLabel String

instance ToText GraphvizGraph where
  toText (GraphvizGraph { global, nodes, edges }) =
    Str.joinWith "\n" $ join
      [ pure "digraph "
      , pure "{"
      , pure $ toText global
      , map toText nodes
      , map toText edges
      , pure "}"
      ]

instance ToText Node where
  toText (Node stateName attrs) = stateName <> " [" <> toText attrs <> "]"

instance ToText Edge where
  toText (Edge from to attrs) = from <> " -> " <> to <> " [" <> toText attrs <> "]"

instance ToText (Array Attr) where
  toText attrs = Str.joinWith ", " (map toText attrs)

instance ToText GlobalAttrs where
  toText (GlobalAttrs attrs) = Str.joinWith ";" (map toText attrs)

instance ToText Attr where
  toText (Attr name value) = name <> " = " <> toText value

instance ToText Value where
  toText (Value str) = "\"" <> str <> "\""
  toText (ValueColor color) = "\"" <> Color.toHexString color <> "\""
  toText (ValueInt int) = show int
  toText (ValueNumber number) = show number
  toText (ValueBoolean boolean) = show boolean
  toText (HtmlLabel label) = "<" <> label <> ">"

rankDirTD :: Attr
rankDirTD = Attr "rankdir" (Value "TD")

fontNameArial :: Attr
fontNameArial = Attr "fontname" (Value "Arial")

labelHtmlBold :: String -> Attr
labelHtmlBold label = Attr "label" (HtmlLabel $ "<b>" <> label <> "</b>")

labelHtmlItalic :: String -> Attr
labelHtmlItalic label = Attr "label" (HtmlLabel $ "<i>" <> label <> "</i>")

shapeBox :: Attr
shapeBox = Attr "shape" (Value "box")

fontSize :: Int -> Attr
fontSize size = Attr "fontsize" (ValueInt size)

styleFilled :: Attr
styleFilled = Attr "style" (Value "filled")

arrowSize :: Number -> Attr
arrowSize size = Attr "arrowsize" (Value $ show size)

shapeCircle :: Attr
shapeCircle = Attr "shape" (Value "circle")

label :: String -> Attr
label label = Attr "label" (Value label)

width :: Number -> Attr
width size = Attr "width" (ValueNumber size)

height :: Number -> Attr
height size = Attr "height" (ValueNumber size)

fixedSize :: Boolean -> Attr
fixedSize value = Attr "fixedsize" (ValueBoolean value)

fillColor :: Color -> Attr
fillColor color = Attr "fillcolor" (ValueColor color)

penWidth :: Number -> Attr
penWidth size = Attr "penwidth" (ValueNumber size)

fontColor :: Color -> Attr
fontColor color = Attr "fontcolor" (ValueColor color)

labelLocC :: Attr
labelLocC = Attr "labelloc" (Value "c")

color :: Color -> Attr
color color = Attr "color" (ValueColor color)