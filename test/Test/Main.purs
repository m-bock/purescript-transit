module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Transit as Test.Transit
import Test.Transit.Class.CurryN as Test.Transit.Class.CurryN
import Test.Transit.Colors as Test.Transit.Colors
import Test.Transit.Core as Test.Transit.Core
import Test.Transit.Data.DotLang as Test.Transit.Data.DotLang
import Test.Transit.Data.Graph as Test.Transit.Data.Graph
import Test.Transit.Data.Html as Test.Transit.Data.Html
import Test.Transit.Generators.Graphviz as Test.Transit.Generators.Graphviz
import Test.Transit.Generators.TransitionTable as Test.Transit.Generators.TransitionTable
import Test.Transit.StateGraph as Test.Transit.StateGraph
import Test.Transit.Util as Test.Transit.Util
import Test.Transit.Class.GetSubset as Test.Transit.Class.GetSubset
import Test.Transit.Class.MatchBySym as Test.Transit.Class.MatchBySym

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] do
    Test.Transit.spec
    Test.Transit.Class.CurryN.spec
    Test.Transit.Colors.spec
    Test.Transit.Core.spec
    Test.Transit.Data.DotLang.spec
    Test.Transit.Data.Graph.spec
    Test.Transit.Data.Html.spec
    Test.Transit.Generators.Graphviz.spec
    Test.Transit.Generators.TransitionTable.spec
    Test.Transit.StateGraph.spec
    Test.Transit.Util.spec
    Test.Transit.Class.GetSubset.spec
    Test.Transit.Class.MatchBySym.spec