module Examples.ColorRing (main, spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Traversable (for_)
import Data.Variant (Variant)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec)
import Transit (type (:*), type (:@), type (>|), Transit, match, mkUpdate, return)
import Transit.Render.Theme (themeContrastDark, themeContrastLight, themeGradientDark, themeGradientLight, themeHarmonyDark, themeHarmonyLight)
import Transit.Render.Graphviz as TransitGraphviz
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

type ColorsFSM = Transit
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

  for_
    [ { file: "renders/themes/harmony-light.dot", title: "Harmony Light", theme: themeHarmonyLight }
    , { file: "renders/themes/harmony-dark.dot", title: "Harmony Dark", theme: themeHarmonyDark }
    , { file: "renders/themes/contrast-light.dot", title: "Contrast Light", theme: themeContrastLight }
    , { file: "renders/themes/contrast-dark.dot", title: "Contrast Dark", theme: themeContrastDark }
    , { file: "renders/themes/gradient-light.dot", title: "Gradient Light", theme: themeGradientLight }
    , { file: "renders/themes/gradient-dark.dot", title: "Gradient Dark", theme: themeGradientDark }
    ]
    \opts -> do
      FS.writeTextFile UTF8 opts.file
        ( TransitGraphviz.generate transit _
            { title = Just opts.title
            , globalAttrsRaw = Just globalAttrs
            , theme = opts.theme
            }
        )

spec :: Spec Unit
spec = do
  pure unit