module Transit.Colors where

import Prelude

import Color (Color)
import Color as Color
import Data.Array ((!!))
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum, enumFromTo)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromMaybe)
import Data.Show.Generic (genericShow)

data BaseColor
  = SpringGreen
  | LemonYellow
  | OceanBlue
  | CoralPink
  | MintTeal
  | AquaBlue
  | SunsetOrange
  | MagentaGlow
  | OliveGreen
  | VividRed
  | SkyCyan

derive instance Eq BaseColor
derive instance Ord BaseColor
derive instance Generic BaseColor _
instance Show BaseColor where
  show = genericShow

instance Bounded BaseColor where
  bottom = genericBottom
  top = genericTop

instance Enum BaseColor where
  succ = genericSucc
  pred = genericPred

allBaseColors :: Array BaseColor
allBaseColors = enumFromTo bottom top

baseColorToColor :: BaseColor -> Color
baseColorToColor = case _ of
  OceanBlue -> Color.hsl 214.4 0.841 0.557
  AquaBlue -> Color.hsl 194.6 0.857 0.643
  SpringGreen -> Color.hsl 82.7 0.780 0.555
  MintTeal -> Color.hsl 147.6 0.628 0.471
  LemonYellow -> Color.hsl 46.8 1.000 0.500
  SunsetOrange -> Color.hsl 28.2 0.866 0.620
  VividRed -> Color.hsl 0.0 0.787 0.631
  CoralPink -> Color.hsl 345.5 1.000 0.661
  SkyCyan -> Color.hsl 203.2 0.709 0.514 -- if kept
  MagentaGlow -> Color.hsl 279.3 0.857 0.690 -- if kept
  OliveGreen -> Color.hsl 151.0 0.635 0.410

x :: BaseColor -> Color -> ColorHarmony
x = case _ of
  OceanBlue -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.25 color
    , edgeFont: Color.darken 0.15 color
    , edgeColor: color
    }
  SunsetOrange -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.25 color
    , edgeFont: Color.darken 0.15 color
    , edgeColor: color
    }
  VividRed -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  AquaBlue -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.3 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  SpringGreen -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  MintTeal -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  SkyCyan -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  LemonYellow -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.15 color
    , edgeFont: Color.darken 0.1 color
    , edgeColor: color
    }
  MagentaGlow -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  OliveGreen -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  CoralPink -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.3 color
    , edgeFont: Color.darken 0.15 color
    , edgeColor: color
    }

y :: BaseColor -> Color -> ColorHarmony
y bc = case bc of
  OceanBlue -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  SunsetOrange -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  VividRed -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  AquaBlue -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  SpringGreen -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  MintTeal -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  SkyCyan -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  LemonYellow -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  MagentaGlow -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  OliveGreen -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  CoralPink -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }

mkDefaultNodeBg :: BaseColor -> Color -> Color
mkDefaultNodeBg _ = Color.darken 0.2

mkDefaultEdgeColor :: BaseColor -> Color -> Color
mkDefaultEdgeColor _ = Color.darken 0.2

mkDefaultNodeFont :: BaseColor -> Color -> Color
mkDefaultNodeFont _ = Color.darken 0.2

mkDefaultEdgeFont :: BaseColor -> Color -> Color
mkDefaultEdgeFont _ = Color.lighten 0.2

type ColorHarmony =
  { nodeBg :: Color
  , nodeFont :: Color
  , edgeFont :: Color
  , edgeColor :: Color
  }

type Theme =
  { bgColor :: Color
  , titleColor :: Color
  , colorHarmonies :: Array ColorHarmony
  , undirectedEdgeColor :: Color
  , undirectedEdgeFontColor :: Color
  }

themeLight :: Theme
themeLight =
  { bgColor: Color.rgb 255 255 255
  , titleColor: Color.rgb 0 0 0
  , colorHarmonies: map (\bc -> x bc (baseColorToColor bc)) allBaseColors
  , undirectedEdgeColor: Color.rgb 0 0 0
  , undirectedEdgeFontColor: Color.rgb 0 0 0
  }

defaultColorHarmony :: ColorHarmony
defaultColorHarmony =
  { nodeBg: Color.rgb 255 255 255
  , nodeFont: Color.rgb 0 0 0
  , edgeFont: Color.rgb 0 0 0
  , edgeColor: Color.rgb 0 0 0
  }

themeDark :: Theme
themeDark =
  { bgColor: Color.rgb 0 0 0
  , titleColor: Color.rgb 255 255 255
  , colorHarmonies: map (\bc -> y bc (baseColorToColor bc)) allBaseColors
  , undirectedEdgeColor: Color.rgb 255 255 255
  , undirectedEdgeFontColor: Color.rgb 255 255 255
  }

getColorHarmony :: Theme -> Int -> ColorHarmony
getColorHarmony theme index = fromMaybe defaultColorHarmony $ theme.colorHarmonies !! index