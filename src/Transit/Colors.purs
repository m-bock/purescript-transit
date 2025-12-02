module Transit.Colors where

import Prelude

import Color (Color)
import Color as Color
import Data.Array ((!!))
import Data.Array as Array
import Data.Maybe (fromJust)
import Data.Tuple.Nested ((/\))
import Partial.Unsafe (unsafePartial)

defLight baseColor =
  { nodeBg: baseColor
  , nodeFont: Color.darken 0.2 baseColor
  , edgeFont: Color.darken 0.2 baseColor
  , edgeColor: baseColor
  }

defDark baseColor =
  { nodeBg: baseColor
  , nodeFont: Color.darken 0.2 baseColor
  , edgeFont: Color.darken 0.2 baseColor
  , edgeColor: baseColor
  }

colors :: Array ThemedColors
colors = map (\(baseColor /\ fn) -> fn baseColor)
  [ Color.hsl 214.4 0.841 0.557 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 28.2 0.866 0.620 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 0.0 0.787 0.631 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 194.6 0.857 0.643 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 82.7 0.780 0.555 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 275.8 0.684 0.592 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 48.0 0.867 0.620 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 147.6 0.628 0.471 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 351.3 1.000 0.718 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 218.2 0.920 0.602 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 23.6 0.949 0.500 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 152.1 0.558 0.631 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 277.0 0.578 0.643 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 203.2 0.709 0.514 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 349.3 1.000 0.784 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 227.0 0.890 0.625 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 46.8 1.000 0.500 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 279.3 0.857 0.690 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 151.0 0.635 0.410 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 345.5 1.000 0.661 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 263.4 0.904 0.661 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 26.6 1.000 0.610 /\ (\c -> { light: defLight c, dark: defDark c })
  , Color.hsl 194.6 0.857 0.643 /\ (\c -> { light: defLight c, dark: defDark c })
  ]

getColor :: Int -> ThemedColors
getColor i = unsafePartial $ fromJust (colors !! i `mod` Array.length colors)

type ThemedColors =
  { dark :: Colors
  , light :: Colors
  }

type Colors =
  { nodeBg :: Color
  , nodeFont :: Color
  , edgeFont :: Color
  , edgeColor :: Color
  }
