module Transit.Colors
  ( ColorHarmony
  , Theme
  , getColorHarmony
  , themeHarmonyDark
  , themeHarmonyLight
  , themeContrastDark
  , themeContrastLight
  , themeGradientDark
  , themeGradientLight
  ) where

import Prelude

import Color (Color)
import Color as Color
import Data.Array (drop)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NEA
import Data.Bounded.Generic (genericBottom, genericTop)
import Data.Enum (class Enum, enumFromTo)
import Data.Enum.Generic (genericPred, genericSucc)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Show.Generic (genericShow)
import Partial.Unsafe (unsafeCrashWith)

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

allBaseColors :: NonEmptyArray BaseColor
allBaseColors = NEA.cons' bottom (drop 1 $ enumFromTo bottom top)

baseColorToColor :: BaseColor -> Color
baseColorToColor = case _ of
  OceanBlue ->
    Color.hsl 214.4 0.841 0.557
  AquaBlue ->
    Color.hsl 194.6 0.857 0.643
  SpringGreen ->
    Color.hsl 82.7 0.780 0.555
  MintTeal ->
    Color.hsl 147.6 0.628 0.471
  LemonYellow ->
    Color.hsl 46.8 1.000 0.500
  SunsetOrange ->
    Color.hsl 28.2 0.866 0.620
  VividRed ->
    Color.hsl 0.0 0.787 0.631
  CoralPink ->
    Color.hsl 345.5 1.000 0.661
  SkyCyan ->
    Color.hsl 203.2 0.709 0.514
  MagentaGlow ->
    Color.hsl 279.3 0.857 0.690
  OliveGreen ->
    Color.hsl 151.0 0.635 0.410

mkLightColorHarmony :: BaseColor -> Color -> ColorHarmony
mkLightColorHarmony = case _ of
  OceanBlue -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.25 color
    , edgeFont: Color.darken 0.15 color
    , edgeColor: color
    }
  SunsetOrange -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.25 color
    , edgeFont: Color.darken 0.15 color
    , edgeColor: color
    }
  VividRed -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  AquaBlue -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.3 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  SpringGreen -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  MintTeal -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  SkyCyan -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  LemonYellow -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.15 color
    , edgeFont: Color.darken 0.1 color
    , edgeColor: color
    }
  MagentaGlow -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  OliveGreen -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.2 color
    , edgeFont: Color.darken 0.2 color
    , edgeColor: color
    }
  CoralPink -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.3 color
    , edgeFont: Color.darken 0.15 color
    , edgeColor: color
    }

mkDarkColorHarmony :: BaseColor -> Color -> ColorHarmony
mkDarkColorHarmony bc = case bc of
  OceanBlue -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  SunsetOrange -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  VividRed -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  AquaBlue -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  SpringGreen -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  MintTeal -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  SkyCyan -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  LemonYellow -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  MagentaGlow -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  OliveGreen -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }
  CoralPink -> \color ->
    { nodeBg: color
    , nodeBorder: color
    , nodeFont: Color.darken 0.1 (mkLightColorHarmony bc color).nodeFont
    , edgeFont: Color.lighten 0.2 color
    , edgeColor: color
    }

type ColorHarmony =
  { nodeBg :: Color
  , nodeFont :: Color
  , nodeBorder :: Color
  , edgeFont :: Color
  , edgeColor :: Color
  }

type Theme =
  { bgColor :: Color
  , titleColor :: Color
  , colorHarmonies :: NonEmptyArray ColorHarmony
  , undirectedEdgeColor :: Color
  , undirectedEdgeFontColor :: Color
  }

getColorHarmony :: Theme -> Int -> ColorHarmony
getColorHarmony theme index = indexMod theme.colorHarmonies index

indexMod :: forall a. NonEmptyArray a -> Int -> a
indexMod xs index = case NEA.index xs (index `mod` NEA.length xs) of
  Just x -> x
  Nothing -> unsafeCrashWith "impossible: index out of bounds"

themeHarmonyLight :: Theme
themeHarmonyLight =
  { bgColor: Color.rgb 255 255 255
  , titleColor: Color.rgb 0 0 0
  , colorHarmonies: map (\bc -> mkLightColorHarmony bc (baseColorToColor bc)) allBaseColors
  , undirectedEdgeColor: Color.rgb 0 0 0
  , undirectedEdgeFontColor: Color.rgb 0 0 0
  }

themeHarmonyDark :: Theme
themeHarmonyDark =
  { bgColor: Color.rgb 20 20 20
  , titleColor: Color.rgb 200 200 200
  , colorHarmonies: map (\bc -> mkDarkColorHarmony bc (baseColorToColor bc)) allBaseColors
  , undirectedEdgeColor: Color.rgb 255 255 255
  , undirectedEdgeFontColor: Color.rgb 255 255 255
  }

themeContrastLight :: Theme
themeContrastLight =
  { bgColor: Color.white
  , titleColor: Color.black
  , colorHarmonies: pure
      { nodeBg: Color.white
      , nodeBorder: Color.black
      , nodeFont: Color.black
      , edgeFont: Color.black
      , edgeColor: Color.black
      }
  , undirectedEdgeColor: Color.black
  , undirectedEdgeFontColor: Color.black
  }

themeContrastDark :: Theme
themeContrastDark =
  { bgColor: Color.black
  , titleColor: Color.white
  , colorHarmonies: pure
      { nodeBg: Color.black
      , nodeBorder: Color.white
      , nodeFont: Color.white
      , edgeFont: Color.white
      , edgeColor: Color.white
      }
  , undirectedEdgeColor: Color.white
  , undirectedEdgeFontColor: Color.white
  }

themeGradientLight :: Theme
themeGradientLight =
  { bgColor: Color.rgb 255 255 255
  , titleColor: Color.rgb 0 0 0
  , colorHarmonies: pure
      { nodeBg: Color.rgb 230 230 230
      , nodeBorder: Color.rgb 255 255 255
      , nodeFont: Color.rgb 0 0 0
      , edgeFont: Color.rgb 0 0 0
      , edgeColor: Color.rgb 130 130 130
      }
  , undirectedEdgeColor: Color.rgb 0 0 0
  , undirectedEdgeFontColor: Color.rgb 0 0 0
  }

themeGradientDark :: Theme
themeGradientDark =
  { bgColor: Color.rgb 0 0 0
  , titleColor: Color.rgb 255 255 255
  , colorHarmonies: pure
      { nodeBg: Color.rgb 200 200 200
      , nodeBorder: Color.rgb 200 200 200
      , nodeFont: Color.rgb 0 0 0
      , edgeFont: Color.rgb 200 200 200
      , edgeColor: Color.rgb 130 130 130
      }
  , undirectedEdgeColor: Color.rgb 0 0 0
  , undirectedEdgeFontColor: Color.rgb 0 0 0
  }
