module Transit.Data.DotLang
  ( Attr(..)
  , Edge(..)
  , GlobalAttrs(..)
  , GraphvizGraph(..)
  , Node(..)
  , Section(..)
  , Value(..)
  , arrowHeadNone
  , arrowSize
  , arrowTailNone
  , class ToText
  , color
  , colorMulti
  , dirBoth
  , fillColor
  , fixedSize
  , fontColor
  , fontNameArial
  , fontSize
  , height
  , label
  , labelHtml
  , labelHtmlBold
  , labelHtmlItalic
  , labelLocC
  , labelLocT
  , penWidth
  , rankDirTD
  , shapeBox
  , shapeCircle
  , shapeDiamond
  , styleFilled
  , toText
  , width
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.String as Str
import Transit.Data.Html as Html

class ToText a where
  toText :: a -> String

data Section
  = SecNode Node
  | SecEdge Edge
  | SecGlobal GlobalAttrs
  | SecGlobalRaw String

newtype GraphvizGraph = GraphvizGraph (Array Section)

newtype GlobalAttrs = GlobalAttrs (Array Attr)

data Node = Node String (Array Attr)

data Edge = Edge String String (Array Attr)

data Attr = Attr String Value

data Value
  = Value String
  | ValueColors (Array Color)
  | ValueInt Int
  | ValueNumber Number
  | ValueBoolean Boolean
  | HtmlLabel String

instance ToText GraphvizGraph where
  toText (GraphvizGraph sections) =
    Str.joinWith "\n" $ join
      [ pure "digraph "
      , pure "{"
      , map toText sections
      , pure "}"
      ]

instance ToText Section where
  toText (SecNode node) = toText node
  toText (SecEdge edge) = toText edge
  toText (SecGlobal global) = toText global
  toText (SecGlobalRaw str) = str

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
  toText (ValueColors colors) = "\"" <> (Str.joinWith ":" $ map Color.toHexString colors) <> "\""
  toText (ValueInt int) = show int
  toText (ValueNumber number) = show number
  toText (ValueBoolean boolean) = show boolean
  toText (HtmlLabel label) = "<" <> label <> ">"

rankDirTD :: Attr
rankDirTD = Attr "rankdir" (Value "TD")

fontNameArial :: Attr
fontNameArial = Attr "fontname" (Value "Arial")

labelHtml :: Html.Node -> Attr
labelHtml node = Attr "label" (HtmlLabel $ Html.nodeToHtml node)

labelHtmlBold :: String -> Attr
labelHtmlBold label = Attr "label" (HtmlLabel $ "<b>" <> label <> "</b>")

labelHtmlItalic :: String -> Attr
labelHtmlItalic label = Attr "label" (HtmlLabel $ "<i>" <> label <> "</i>")

shapeBox :: Attr
shapeBox = Attr "shape" (Value "box")

shapeDiamond :: Attr
shapeDiamond = Attr "shape" (Value "diamond")

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
fillColor color = Attr "fillcolor" (ValueColors [ color ])

penWidth :: Number -> Attr
penWidth size = Attr "penwidth" (ValueNumber size)

fontColor :: Color -> Attr
fontColor color = Attr "fontcolor" (ValueColors [ color ])

labelLocC :: Attr
labelLocC = Attr "labelloc" (Value "c")

labelLocT :: Attr
labelLocT = Attr "labelloc" (Value "t")

color :: Color -> Attr
color color = Attr "color" (ValueColors [ color ])

colorMulti :: Array Color -> Attr
colorMulti colors = Attr "color" (ValueColors colors)

dirBoth :: Attr
dirBoth = Attr "dir" (Value "both")

arrowHeadNone :: Attr
arrowHeadNone = Attr "arrowhead" (Value "none")

arrowTailNone :: Attr
arrowTailNone = Attr "arrowtail" (Value "none")