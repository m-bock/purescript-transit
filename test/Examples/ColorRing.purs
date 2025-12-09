module Test.Examples.ColorRing where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Reflectable (reflectType)
import Effect (Effect)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return)
import Transit.Colors (BaseColor, themeDark)
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.StateGraph (mkStateGraph)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

type State = BaseColor

data Msg = MsgA | MsgB | MsgC | MsgD | MsgE | MsgF | MsgG | MsgH | MsgI | MsgJ | MsgK

derive instance Generic Msg _

type ColorsFSM = Transit $ Empty
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
update = mkUpdateGeneric @ColorsFSM
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

  TransitGraphviz.writeToFile
    ( _
        { title = "Color Ring"
        , globalAttrsRaw = Just "graph [layout=sfdp;overlap=false, K=2.5, repulsiveforce=4, splines=true];"
        }
    )
    transit
    "graphs/color-ring.dot"

  TransitGraphviz.writeToFile
    ( _
        { title = "Color Ring"
        , globalAttrsRaw = Just "graph [layout=sfdp;overlap=false, K=2.5, repulsiveforce=4, splines=true];"
        , theme = themeDark
        }
    )
    transit
    "graphs/color-ring-dark.dot"