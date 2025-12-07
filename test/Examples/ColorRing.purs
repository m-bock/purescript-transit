module Test.Examples.ColorRing where

import Prelude

import Data.Reflectable (reflectType)
import Effect (Effect)
import Transit (type (:*), type (:@), type (>|), Empty, Transit)
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.StateGraph (mkStateGraph)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))

type ColorsFSM = Transit $ Empty
  :* ("StateA" :@ "MsgA" >| "StateB")
  :* ("StateB" :@ "MsgB" >| "StateC")
  :* ("StateC" :@ "MsgC" >| "StateD")
  :* ("StateD" :@ "MsgD" >| "StateE")
  :* ("StateE" :@ "MsgE" >| "StateF")
  :* ("StateF" :@ "MsgF" >| "StateG")
  :* ("StateG" :@ "MsgG" >| "StateH")
  :* ("StateH" :@ "MsgH" >| "StateI")
  :* ("StateI" :@ "MsgI" >| "StateJ")
  :* ("StateJ" :@ "MsgJ" >| "StateK")
  :* ("StateK" :@ "MsgK" >| "StateL")
  :* ("StateL" :@ "MsgL" >| "StateM")
  :* ("StateM" :@ "MsgM" >| "StateN")
  :* ("StateN" :@ "MsgN" >| "StateO")
  :* ("StateO" :@ "MsgO" >| "StateP")
  :* ("StateP" :@ "MsgP" >| "StateQ")
  :* ("StateQ" :@ "MsgQ" >| "StateR")
  :* ("StateR" :@ "MsgR" >| "StateS")
  :* ("StateS" :@ "MsgS" >| "StateT")
  :* ("StateT" :@ "MsgT" >| "StateU")
  :* ("StateU" :@ "MsgU" >| "StateV")
  :* ("StateV" :@ "MsgV" >| "StateW")
  :* ("StateW" :@ "MsgW" >| "StateX")
  :* ("StateX" :@ "MsgX" >| "StateY")
  :* ("StateY" :@ "MsgY" >| "StateZ")
  :* ("StateZ" :@ "MsgZ" >| "StateA")

main :: Effect Unit
main = do
  let
    stateGraph = mkStateGraph (reflectType (Proxy @ColorsFSM))

  -- graph [layout=sfdp, overlap=false, K=2.5, repulsiveforce=4, splines=true]
  TransitGraphviz.writeToFile_ stateGraph "graphs/color-ring.dot"
