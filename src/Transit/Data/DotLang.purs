-- | Data types and functions for generating Graphviz DOT language output.
-- |
-- | This module provides a type-safe way to construct DOT language graphs
-- | for visualization with Graphviz.
module Transit.Data.DotLang
  ( Attr(..)
  , Edge(..)
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
  , layoutCirco
  , layoutDot
  , layoutNeato
  , margin
  , pad
  , penWidth
  , pos
  , rankDirLR
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
  | SecGlobalGraph (Array Attr)
  | SecGlobalNode (Array Attr)
  | SecGlobalEdge (Array Attr)
  | SecNewline
  | SecCommentLine String

-- | Complete Graphviz directed graph.
newtype GraphvizGraph = GraphvizGraph (Array Section)

-- | Graph node with optional raw attributes and attribute list.
data Node = Node String (Array Attr)

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
  | ValuePosition { x :: Number, y :: Number, exact :: Boolean }
  | HtmlLabel String
  | Raw String

instance ToDotStr GraphvizGraph where
  toDotStr (GraphvizGraph sections) =
    Str.joinWith "\n" $ join
      [ pure "digraph {"
      , map (("  " <> _) <<< toDotStr) sections
      , pure "}"
      ]

toDotStrMultiline :: Array Attr -> String
toDotStrMultiline [] = "[]"
toDotStrMultiline as =
  "[\n"
    <> Str.joinWith "\n" (map (("    " <> _) <<< toDotStrAttrMultiline) as)
    <>
      "\n  ]"
  where
  toDotStrAttrMultiline (Attr name value) =
    case value of
      Raw v -> name <> " = " <> v
      _ -> name <> " = " <> toDotStr value

instance ToDotStr Section where
  toDotStr (SecNode node) = toDotStr node
  toDotStr (SecEdge edge) = toDotStr edge
  toDotStr (SecGlobalGraph global) = "graph " <> toDotStrMultiline global
  toDotStr (SecGlobalNode attrs) = "node " <> toDotStrMultiline attrs
  toDotStr (SecGlobalEdge attrs) = "edge " <> toDotStrMultiline attrs
  toDotStr (SecNewline) = ""
  toDotStr (SecCommentLine comment) = "// " <> comment

instance ToDotStr Node where
  toDotStr (Node stateName attrs) = stateName <> " " <> toDotStr attrs

instance ToDotStr Edge where
  toDotStr (Edge from to attrs) = from <> " -> " <> to <> " " <> toDotStr attrs

instance ToDotStr (Array Attr) where
  toDotStr attrs = "[" <> Str.joinWith ", " (map toDotStr attrs) <> "]"

instance ToDotStr Attr where
  toDotStr (Attr name value) =
    case value of
      Raw v -> name <> " = " <> v
      _ -> name <> " = " <> toDotStr value

instance ToDotStr Value where
  toDotStr (Value str) = "\"" <> str <> "\""
  toDotStr (ValueColors colors) = "\"" <> (Str.joinWith ":" $ map Color.toHexString colors) <> "\""
  toDotStr (ValueInt int) = show int
  toDotStr (ValueNumber number) = show number
  toDotStr (ValueBoolean boolean) = show boolean
  toDotStr (HtmlLabel html) = "<" <> html <> ">"
  toDotStr (ValuePosition { x, y, exact }) = "\"" <> show x <> "," <> show y <> (if exact then "!" else "") <> "\""
  toDotStr (Raw str) = str

--------------------------------------------------------------------------------
-- Helper functions for common attributes
--------------------------------------------------------------------------------

rankDir :: String -> Attr
rankDir direction = Attr "rankdir" (Raw direction)

rankDirTD :: Attr
rankDirTD = rankDir "TD"

rankDirLR :: Attr
rankDirLR = rankDir "LR"

fontName :: String -> Attr
fontName name = Attr "fontname" (Raw name)

fontNameArial :: Attr
fontNameArial = fontName "Arial"

labelHtml :: Html.Node -> Attr
labelHtml node = Attr "label" (HtmlLabel $ Html.nodeToHtml node)

labelHtmlBold :: String -> Attr
labelHtmlBold text = Attr "label" (HtmlLabel $ "<b>" <> text <> "</b>")

labelHtmlItalic :: String -> Attr
labelHtmlItalic text = Attr "label" (HtmlLabel $ "<i>" <> text <> "</i>")

shape :: String -> Attr
shape shapeName = Attr "shape" (Raw shapeName)

shapeBox :: Attr
shapeBox = shape "box"

shapeDiamond :: Attr
shapeDiamond = shape "diamond"

shapeCircle :: Attr
shapeCircle = shape "circle"

fontSize :: Number -> Attr
fontSize size = Attr "fontsize" (ValueNumber size)

style :: String -> Attr
style styleName = Attr "style" (Raw styleName)

styleFilled :: Attr
styleFilled = style "filled"

arrowSize :: Number -> Attr
arrowSize size = Attr "arrowsize" (ValueNumber size)

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

labelLoc :: String -> Attr
labelLoc location = Attr "labelloc" (Raw location)

labelLocC :: Attr
labelLocC = labelLoc "c"

labelLocT :: Attr
labelLocT = labelLoc "t"

color :: Color -> Attr
color c = Attr "color" (ValueColors [ c ])

colorMulti :: Array Color -> Attr
colorMulti colors = Attr "color" (ValueColors colors)

dir :: String -> Attr
dir direction = Attr "dir" (Raw direction)

dirBoth :: Attr
dirBoth = dir "both"

arrowHead :: String -> Attr
arrowHead direction = Attr "arrowhead" (Raw direction)

arrowHeadNone :: Attr
arrowHeadNone = arrowHead "none"

arrowTail :: String -> Attr
arrowTail direction = Attr "arrowtail" (Raw direction)

arrowTailNone :: Attr
arrowTailNone = arrowTail "none"

bgColor :: Color -> Attr
bgColor c = Attr "bgcolor" (ValueColors [ c ])

margin :: Number -> Attr
margin size = Attr "margin" (ValueNumber size)

pad :: Number -> Attr
pad size = Attr "pad" (ValueNumber size)

pos :: Number -> Number -> Boolean -> Attr
pos x y exact = Attr "pos" (ValuePosition { x, y, exact })

layout :: String -> Attr
layout v = Attr "layout" (Raw v)

layoutNeato :: Attr
layoutNeato = layout "neato"

layoutDot :: Attr
layoutDot = layout "dot"

layoutCirco :: Attr
layoutCirco = layout "circo"