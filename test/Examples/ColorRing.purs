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

data Msg = MsgA | MsgB | MsgC | MsgD | MsgE | MsgF | MsgG | MsgH | MsgI | MsgJ | MsgK | MsgL | MsgM | MsgN | MsgO | MsgP | MsgQ | MsgR | MsgS | MsgT | MsgU | MsgV

derive instance Generic Msg _

type ColorsFSM = Transit $ Empty
  :* ("OceanBlue" :@ "MsgA" >| "SunsetOrange")
  :* ("SunsetOrange" :@ "MsgB" >| "VividRed")
  :* ("VividRed" :@ "MsgC" >| "AquaBlue")
  :* ("AquaBlue" :@ "MsgD" >| "SpringGreen")
  :* ("SpringGreen" :@ "MsgE" >| "RoyalPurple")
  :* ("RoyalPurple" :@ "MsgF" >| "GoldenYellow")
  :* ("GoldenYellow" :@ "MsgG" >| "MintTeal")
  :* ("MintTeal" :@ "MsgH" >| "RosePink")
  :* ("RosePink" :@ "MsgI" >| "DeepAzure")
  :* ("DeepAzure" :@ "MsgJ" >| "AmberOrange")
  :* ("AmberOrange" :@ "MsgK" >| "SoftTurquoise")
  :* ("SoftTurquoise" :@ "MsgL" >| "VioletPurple")
  :* ("VioletPurple" :@ "MsgM" >| "SkyCyan")
  :* ("SkyCyan" :@ "MsgN" >| "CandyPink")
  :* ("CandyPink" :@ "MsgO" >| "ElectricBlue")
  :* ("ElectricBlue" :@ "MsgP" >| "LemonYellow")
  :* ("LemonYellow" :@ "MsgQ" >| "MagentaGlow")
  :* ("MagentaGlow" :@ "MsgR" >| "OliveGreen")
  :* ("OliveGreen" :@ "MsgS" >| "CoralPink")
  :* ("CoralPink" :@ "MsgT" >| "Lavender")
  :* ("Lavender" :@ "MsgU" >| "Tangerine")
  :* ("Tangerine" :@ "MsgV" >| "OceanBlue")

update :: State -> Msg -> State
update = mkUpdateGeneric @ColorsFSM
  (match @"OceanBlue" @"MsgA" \_ _ -> return @"SunsetOrange")
  (match @"SunsetOrange" @"MsgB" \_ _ -> return @"VividRed")
  (match @"VividRed" @"MsgC" \_ _ -> return @"AquaBlue")
  (match @"AquaBlue" @"MsgD" \_ _ -> return @"SpringGreen")
  (match @"SpringGreen" @"MsgE" \_ _ -> return @"RoyalPurple")
  (match @"RoyalPurple" @"MsgF" \_ _ -> return @"GoldenYellow")
  (match @"GoldenYellow" @"MsgG" \_ _ -> return @"MintTeal")
  (match @"MintTeal" @"MsgH" \_ _ -> return @"RosePink")
  (match @"RosePink" @"MsgI" \_ _ -> return @"DeepAzure")
  (match @"DeepAzure" @"MsgJ" \_ _ -> return @"AmberOrange")
  (match @"AmberOrange" @"MsgK" \_ _ -> return @"SoftTurquoise")
  (match @"SoftTurquoise" @"MsgL" \_ _ -> return @"VioletPurple")
  (match @"VioletPurple" @"MsgM" \_ _ -> return @"SkyCyan")
  (match @"SkyCyan" @"MsgN" \_ _ -> return @"CandyPink")
  (match @"CandyPink" @"MsgO" \_ _ -> return @"ElectricBlue")
  (match @"ElectricBlue" @"MsgP" \_ _ -> return @"LemonYellow")
  (match @"LemonYellow" @"MsgQ" \_ _ -> return @"MagentaGlow")
  (match @"MagentaGlow" @"MsgR" \_ _ -> return @"OliveGreen")
  (match @"OliveGreen" @"MsgS" \_ _ -> return @"CoralPink")
  (match @"CoralPink" @"MsgT" \_ _ -> return @"Lavender")
  (match @"Lavender" @"MsgU" \_ _ -> return @"Tangerine")
  (match @"Tangerine" @"MsgV" \_ _ -> return @"OceanBlue")

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