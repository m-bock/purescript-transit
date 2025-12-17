module Test.Examples.ColorRing (main, spec) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Variant (Variant)
import Effect (Effect)
import Test.Spec (Spec)
import Transit (type (:*), type (:@), type (>|), Empty, match, mkUpdate, return)
import Transit.Colors (themeContrastDark, themeContrastLight, themeGradientDark, themeGradientLight, themeHarmonyDark, themeHarmonyLight)
import Transit.Generators.Graphviz as TransitGraphviz
import Type.Function (type ($))
import Type.Prelude (Proxy(..))

type State = Variant
  ( "SpringGreen" :: {}
  , "LemonYellow" :: {}
  , "OceanBlue" :: {}
  , "CoralPink" :: {}
  , "MintTeal" :: {}
  , "AquaBlue" :: {}
  , "SunsetOrange" :: {}
  , "MagentaGlow" :: {}
  , "OliveGreen" :: {}
  , "VividRed" :: {}
  , "SkyCyan" :: {}
  )

type Msg = Variant
  ( "MsgA" :: {}
  , "MsgB" :: {}
  , "MsgC" :: {}
  , "MsgD" :: {}
  , "MsgE" :: {}
  , "MsgF" :: {}
  , "MsgG" :: {}
  , "MsgH" :: {}
  , "MsgI" :: {}
  , "MsgJ" :: {}
  , "MsgK" :: {}
  )

type ColorsFSM = Empty
  :* ("SpringGreen" :@ "MsgA" >| "LemonYellow")
  :* ("LemonYellow" :@ "MsgB" >| "OceanBlue")
  :* ("OceanBlue" :@ "MsgC" >| "CoralPink")
  :* ("CoralPink" :@ "MsgD" >| "MintTeal")
  :* ("MintTeal" :@ "MsgE" >| "AquaBlue")
  :* ("AquaBlue" :@ "MsgF" >| "SunsetOrange")
  :* ("SunsetOrange" :@ "MsgG" >| "MagentaGlow")
  :* ("MagentaGlow" :@ "MsgH" >| "OliveGreen")
  :* ("OliveGreen" :@ "MsgI" >| "VividRed")
  :* ("VividRed" :@ "MsgJ" >| "SkyCyan")
  :* ("SkyCyan" :@ "MsgK" >| "SpringGreen")

update :: State -> Msg -> State
update = mkUpdate @ColorsFSM
  (match @"SpringGreen" @"MsgA" \_ _ -> return @"LemonYellow")
  (match @"LemonYellow" @"MsgB" \_ _ -> return @"OceanBlue")
  (match @"OceanBlue" @"MsgC" \_ _ -> return @"CoralPink")
  (match @"CoralPink" @"MsgD" \_ _ -> return @"MintTeal")
  (match @"MintTeal" @"MsgE" \_ _ -> return @"AquaBlue")
  (match @"AquaBlue" @"MsgF" \_ _ -> return @"SunsetOrange")
  (match @"SunsetOrange" @"MsgG" \_ _ -> return @"MagentaGlow")
  (match @"MagentaGlow" @"MsgH" \_ _ -> return @"OliveGreen")
  (match @"OliveGreen" @"MsgI" \_ _ -> return @"VividRed")
  (match @"VividRed" @"MsgJ" \_ _ -> return @"SkyCyan")
  (match @"SkyCyan" @"MsgK" \_ _ -> return @"SpringGreen")

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @ColorsFSM)

    globalAttrs = "graph [layout=sfdp;overlap=false, K=2.5, repulsiveforce=4, splines=true];"

  TransitGraphviz.writeToFile "graphs/themes/harmony-light.dot" transit _
    { title = Just "Harmony Light"
    , globalAttrsRaw = Just globalAttrs
    , theme = themeHarmonyLight
    }

  TransitGraphviz.writeToFile "graphs/themes/harmony-dark.dot" transit _
    { title = Just "Harmony Dark"
    , globalAttrsRaw = Just globalAttrs
    , theme = themeHarmonyDark
    }

  TransitGraphviz.writeToFile "graphs/themes/contrast-light.dot" transit _
    { title = Just "Contrast Light"
    , globalAttrsRaw = Just globalAttrs
    , theme = themeContrastLight
    }

  TransitGraphviz.writeToFile "graphs/themes/contrast-dark.dot" transit _
    { title = Just "Contrast Dark"
    , globalAttrsRaw = Just globalAttrs
    , theme = themeContrastDark
    }

  TransitGraphviz.writeToFile "graphs/themes/gradient-light.dot" transit _
    { title = Just "Gradient Light"
    , globalAttrsRaw = Just globalAttrs
    , theme = themeGradientLight
    }

  TransitGraphviz.writeToFile "graphs/themes/gradient-dark.dot" transit _
    { title = Just "Gradient Dark"
    , globalAttrsRaw = Just globalAttrs
    , theme = themeGradientDark
    }

spec :: Spec Unit
spec = do
  pure unit