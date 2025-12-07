module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Transit as Test.Transit
import Test.Transit.Colors as Test.Transit.Colors
import Test.Transit.Core as Test.Transit.Core
import Test.Transit.CurryN as Test.Transit.CurryN
import Test.Transit.DotLang as Test.Transit.DotLang
import Test.Transit.Generators.Graphviz as Test.Transit.Generators.Graphviz
import Test.Transit.Generators.TransitionTable as Test.Transit.Generators.TransitionTable
import Test.Transit.Graph as Test.Transit.Graph
import Test.Transit.Html as Test.Transit.Html
import Test.Transit.StateGraph as Test.Transit.StateGraph
import Test.Transit.Util as Test.Transit.Util

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] do
    Test.Transit.spec
    Test.Transit.Colors.spec
    Test.Transit.Core.spec
    Test.Transit.CurryN.spec
    Test.Transit.DotLang.spec
    Test.Transit.Generators.Graphviz.spec
    Test.Transit.Generators.TransitionTable.spec
    Test.Transit.Graph.spec
    Test.Transit.Html.spec
    Test.Transit.StateGraph.spec
    Test.Transit.Util.spec
