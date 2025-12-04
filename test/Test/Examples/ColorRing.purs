module Test.Examples.ColorRing where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Effect (Effect)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), Empty, Wrap, match, mkUpdateGeneric, return_, type (>|))
import Transit.Gen.Graphviz as TransitGraphviz
import Type.Function (type ($))

type ColorTest = Wrap $ Empty
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

