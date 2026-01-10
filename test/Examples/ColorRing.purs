module Examples.ColorRing (main, spec) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Data.Traversable (for_)
import Data.Variant (Variant)
import Effect (Effect)
import Examples.Common (assertWalk, (~>))
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS
import Test.Spec (Spec, describe, it)
import Transit (type (:*), type (:@), type (>|), Transit, TransitCore, mkUpdateAuto)
import Transit.Data.DotLang (Attr(..), Value(..))
import Transit.Data.DotLang as Graphviz
import Transit.Render.Graphviz (Layout(..))
import Transit.Render.Graphviz as TransitGraphviz
import Transit.Render.Theme (themeContrastDark, themeContrastLight, themeGradientDark, themeGradientLight, themeHarmonyDark, themeHarmonyLight)
import Transit.VariantUtils (v)
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

type ColorRingTransit = Transit
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
update = mkUpdateAuto @ColorRingTransit

spec :: Spec Unit
spec = do
  describe "ColorRing" do
    it "should follow the walk and visit the expected intermediate states" do
      assertWalk update (v @"SpringGreen")
        [ v @"MsgA" ~> v @"LemonYellow"
        , v @"MsgB" ~> v @"OceanBlue"
        , v @"MsgC" ~> v @"CoralPink"
        , v @"MsgD" ~> v @"MintTeal"
        , v @"MsgE" ~> v @"AquaBlue"
        , v @"MsgF" ~> v @"SunsetOrange"
        , v @"MsgG" ~> v @"MagentaGlow"
        , v @"MsgH" ~> v @"OliveGreen"
        , v @"MsgI" ~> v @"VividRed"
        , v @"MsgJ" ~> v @"SkyCyan"
        , v @"MsgK" ~> v @"SpringGreen"
        ]

colorRingTransit :: TransitCore
colorRingTransit = reflectType (Proxy @ColorRingTransit)

main :: Effect Unit
main =
  for_
    [ { file: "renders/themes/harmony-light.dot"
      , title: "Harmony Light"
      , theme: themeHarmonyLight
      }
    , { file: "renders/themes/harmony-dark.dot"
      , title: "Harmony Dark"
      , theme: themeHarmonyDark
      }
    , { file: "renders/themes/contrast-light.dot"
      , title: "Contrast Light"
      , theme: themeContrastLight
      }
    , { file: "renders/themes/contrast-dark.dot"
      , title: "Contrast Dark"
      , theme: themeContrastDark
      }
    , { file: "renders/themes/gradient-light.dot"
      , title: "Gradient Light"
      , theme: themeGradientLight
      }
    , { file: "renders/themes/gradient-dark.dot"
      , title: "Gradient Dark"
      , theme: themeGradientDark
      }
    ]
    \opts -> do
      FS.writeTextFile UTF8 opts.file
        ( Graphviz.toDotStr $ TransitGraphviz.generate colorRingTransit \def -> def
            { title = Just opts.title
            , extraGraphAttrs =
                [ Attr "layout" (Value "sfdp")
                , Attr "overlap" (ValueBoolean false)
                , Attr "K" (ValueNumber 2.5)
                , Attr "repulsiveforce" (ValueInt 4)
                , Attr "splines" (ValueBoolean true)
                ]
            , theme = opts.theme
            , layout = None
            }
        )

