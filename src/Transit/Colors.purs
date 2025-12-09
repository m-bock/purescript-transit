module Transit.Colors where

import Prelude

import Color (Color)
import Color as Color
import Data.Array ((!!))
import Data.Array as Array
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum, enumFromTo)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (fromJust, fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)
import Unsafe.Coerce (unsafeCoerce)

data BaseColor
  = OceanBlue
  | SunsetOrange
  | VividRed
  | AquaBlue
  | SpringGreen
  | RoyalPurple
  | GoldenYellow
  | MintTeal
  | RosePink
  | DeepAzure
  | AmberOrange
  | SoftTurquoise
  | VioletPurple
  | SkyCyan
  | CandyPink
  | ElectricBlue
  | LemonYellow
  | MagentaGlow
  | OliveGreen
  | CoralPink
  | Lavender
  | Tangerine

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
  SunsetOrange -> Color.hsl 28.2 0.866 0.620
  VividRed -> Color.hsl 0.0 0.787 0.631
  AquaBlue -> Color.hsl 194.6 0.857 0.643
  SpringGreen -> Color.hsl 82.7 0.780 0.555
  RoyalPurple -> Color.hsl 275.8 0.684 0.592
  GoldenYellow -> Color.hsl 48.0 0.867 0.620
  MintTeal -> Color.hsl 147.6 0.628 0.471
  RosePink -> Color.hsl 351.3 1.000 0.718
  DeepAzure -> Color.hsl 218.2 0.920 0.602
  AmberOrange -> Color.hsl 23.6 0.949 0.500
  SoftTurquoise -> Color.hsl 152.1 0.558 0.631
  VioletPurple -> Color.hsl 277.0 0.578 0.643
  SkyCyan -> Color.hsl 203.2 0.709 0.514
  CandyPink -> Color.hsl 349.3 1.000 0.784
  ElectricBlue -> Color.hsl 227.0 0.890 0.625
  LemonYellow -> Color.hsl 46.8 1.000 0.500
  MagentaGlow -> Color.hsl 279.3 0.857 0.690
  OliveGreen -> Color.hsl 151.0 0.635 0.410
  CoralPink -> Color.hsl 345.5 1.000 0.661
  Lavender -> Color.hsl 263.4 0.904 0.661
  Tangerine -> Color.hsl 26.6 1.000 0.610

mkDefaultColorHarmonyLight :: Color -> ColorHarmony
mkDefaultColorHarmonyLight color =
  { nodeBg: color
  , nodeFont: Color.darken 0.2 color
  , edgeFont: Color.darken 0.2 color
  , edgeColor: color
  }

mkDefaultColorHarmonyDark :: Color -> ColorHarmony
mkDefaultColorHarmonyDark color =
  { nodeBg: color
  , nodeFont: Color.darken 0.2 color
  , edgeFont: Color.lighten 0.2 color
  , edgeColor: color
  }

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
  VividRed -> mkDefaultColorHarmonyLight
  AquaBlue -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.3 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  SpringGreen -> mkDefaultColorHarmonyLight
  RoyalPurple -> mkDefaultColorHarmonyLight
  GoldenYellow -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.3 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  MintTeal -> mkDefaultColorHarmonyLight
  RosePink -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.3 color
    , edgeFont: Color.darken 0.15 color
    , edgeColor: color
    }
  DeepAzure -> mkDefaultColorHarmonyLight
  AmberOrange -> mkDefaultColorHarmonyLight
  SoftTurquoise -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.3 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  VioletPurple -> mkDefaultColorHarmonyLight
  SkyCyan -> mkDefaultColorHarmonyLight
  CandyPink -> mkDefaultColorHarmonyLight
  ElectricBlue -> mkDefaultColorHarmonyLight
  LemonYellow -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.15 color
    , edgeFont: Color.darken 0.1 color
    , edgeColor: color
    }
  MagentaGlow -> mkDefaultColorHarmonyLight
  OliveGreen -> mkDefaultColorHarmonyLight
  CoralPink -> \color ->
    { nodeBg: color
    , nodeFont: Color.darken 0.3 color
    , edgeFont: Color.darken 0.15 color
    , edgeColor: color
    }
  Lavender -> mkDefaultColorHarmonyLight
  Tangerine -> mkDefaultColorHarmonyLight

y :: BaseColor -> Color -> ColorHarmony
y bc = case bc of
  OceanBlue -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  SunsetOrange -> mkDefaultColorHarmonyDark
  VividRed -> mkDefaultColorHarmonyDark
  AquaBlue -> mkDefaultColorHarmonyDark
  SpringGreen -> mkDefaultColorHarmonyDark
  RoyalPurple -> mkDefaultColorHarmonyDark
  GoldenYellow -> mkDefaultColorHarmonyDark
  MintTeal -> mkDefaultColorHarmonyDark
  RosePink -> mkDefaultColorHarmonyDark
  DeepAzure -> mkDefaultColorHarmonyDark
  AmberOrange -> mkDefaultColorHarmonyDark
  SoftTurquoise -> mkDefaultColorHarmonyDark
  VioletPurple -> mkDefaultColorHarmonyDark
  SkyCyan -> mkDefaultColorHarmonyDark
  CandyPink -> mkDefaultColorHarmonyDark
  ElectricBlue -> mkDefaultColorHarmonyDark
  LemonYellow -> mkDefaultColorHarmonyDark
  MagentaGlow -> mkDefaultColorHarmonyDark
  OliveGreen -> mkDefaultColorHarmonyDark
  CoralPink -> \color ->
    { nodeBg: color
    , nodeFont: (x bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  Lavender -> mkDefaultColorHarmonyDark
  Tangerine -> mkDefaultColorHarmonyDark

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