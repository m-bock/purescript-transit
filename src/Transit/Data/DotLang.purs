-- | Data types and functions for generating Graphviz DOT language output.
-- |
-- | This module provides a type-safe way to construct DOT language graphs
-- | for visualization with Graphviz.
module Transit.Data.DotLang
  ( Attr(..)
  , Edge(..)
  , GlobalAttrs(..)
  , GraphvizGraph(..)
  , Node(..)
  , Raw
  , Section(..)
  , Value(..)
  , arrowHeadNone
  , arrowSize
  , arrowTailNone
  , bgColor
  , class ToDotStr
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
  , margin
  , pad
  , penWidth
  , rankDirTD
  , shapeBox
  , shapeCircle
  , shapeDiamond
  , styleFilled
  , toDotStr
  , width
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.Maybe (Maybe(..))
import Data.String as Str
import Transit.Data.Html as Html

-- | Type class for converting values to DOT language string representation.
class ToDotStr a where
  toDotStr :: a -> String

-- | Raw DOT language string (for custom attributes).
type Raw = String

-- | Section of a DOT graph (node, edge, or global attributes).
data Section
  = SecNode Node
  | SecEdge Edge
  | SecGlobal GlobalAttrs
  | SecGlobalRaw String

-- | Complete Graphviz directed graph.
newtype GraphvizGraph = GraphvizGraph (Array Section)

-- | Global graph attributes.
newtype GlobalAttrs = GlobalAttrs (Array Attr)

-- | Graph node with optional raw attributes and attribute list.
data Node = Node String (Maybe Raw) (Array Attr)

-- | Graph edge from one node to another with attributes.
data Edge = Edge String String (Array Attr)

-- | Graph attribute (name-value pair).
data Attr = Attr String Value

-- | Attribute value types.
data Value
  = Value String
  | ValueColors (Array Color)
  | ValueInt Int
  | ValueNumber Number
  | ValueBoolean Boolean
  | HtmlLabel String

instance ToDotStr GraphvizGraph where
  toDotStr (GraphvizGraph sections) =
    Str.joinWith "\n" $ join
      [ pure "digraph "
      , pure "{"
      , map toDotStr sections
      , pure "}"
      ]

instance ToDotStr Section where
  toDotStr (SecNode node) = toDotStr node
  toDotStr (SecEdge edge) = toDotStr edge
  toDotStr (SecGlobal global) = toDotStr global
  toDotStr (SecGlobalRaw str) = str

instance ToDotStr Node where
  toDotStr (Node stateName rawAttr attrs) = stateName <> " ["
    <> case rawAttr of
      Just raw -> raw <> ","
      Nothing -> ""
    <> toDotStr attrs
    <> "]"

instance ToDotStr Edge where
  toDotStr (Edge from to attrs) = from <> " -> " <> to <> " [" <> toDotStr attrs <> "]"

instance ToDotStr (Array Attr) where
  toDotStr attrs = Str.joinWith ", " (map toDotStr attrs)

instance ToDotStr GlobalAttrs where
  toDotStr (GlobalAttrs attrs) = Str.joinWith ";" (map toDotStr attrs)

instance ToDotStr Attr where
  toDotStr (Attr name value) = name <> " = " <> toDotStr value

instance ToDotStr Value where
  toDotStr (Value str) = "\"" <> str <> "\""
  toDotStr (ValueColors colors) = "\"" <> (Str.joinWith ":" $ map Color.toHexString colors) <> "\""
  toDotStr (ValueInt int) = show int
  toDotStr (ValueNumber number) = show number
  toDotStr (ValueBoolean boolean) = show boolean
  toDotStr (HtmlLabel html) = "<" <> html <> ">"

--------------------------------------------------------------------------------
-- Helper functions for common attributes
--------------------------------------------------------------------------------

rankDirTD :: Attr
rankDirTD = Attr "rankdir" (Value "TD")

fontNameArial :: Attr
fontNameArial = Attr "fontname" (Value "Arial")

labelHtml :: Html.Node -> Attr
labelHtml node = Attr "label" (HtmlLabel $ Html.nodeToHtml node)

labelHtmlBold :: String -> Attr
labelHtmlBold text = Attr "label" (HtmlLabel $ "<b>" <> text <> "</b>")

labelHtmlItalic :: String -> Attr
labelHtmlItalic text = Attr "label" (HtmlLabel $ "<i>" <> text <> "</i>")

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
label text = Attr "label" (Value text)

width :: Number -> Attr
width size = Attr "width" (ValueNumber size)

height :: Number -> Attr
height size = Attr "height" (ValueNumber size)

fixedSize :: Boolean -> Attr
fixedSize value = Attr "fixedsize" (ValueBoolean value)

fillColor :: Color -> Attr
fillColor c = Attr "fillcolor" (ValueColors [ c ])

penWidth :: Number -> Attr
penWidth size = Attr "penwidth" (ValueNumber size)

fontColor :: Color -> Attr
fontColor c = Attr "fontcolor" (ValueColors [ c ])

labelLocC :: Attr
labelLocC = Attr "labelloc" (Value "c")

labelLocT :: Attr
labelLocT = Attr "labelloc" (Value "t")

color :: Color -> Attr
color c = Attr "color" (ValueColors [ c ])

colorMulti :: Array Color -> Attr
colorMulti colors = Attr "color" (ValueColors colors)

dirBoth :: Attr
dirBoth = Attr "dir" (Value "both")

arrowHeadNone :: Attr
arrowHeadNone = Attr "arrowhead" (Value "none")

arrowTailNone :: Attr
arrowTailNone = Attr "arrowtail" (Value "none")

bgColor :: Color -> Attr
bgColor c = Attr "bgcolor" (ValueColors [ c ])

margin :: Number -> Attr
margin size = Attr "margin" (ValueNumber size)

pad :: Number -> Attr
pad size = Attr "pad" (ValueNumber size)