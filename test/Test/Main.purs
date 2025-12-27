module Test.Main (main) where

import Prelude

import Effect (Effect)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Transit as Test.Transit
import Test.Transit.Class.CurryN as Test.Transit.Class.CurryN
import Test.Transit.Render.Theme as Test.Transit.Render.Theme
import Test.Transit.Core as Test.Transit.Core
import Test.Transit.Data.DotLang as Test.Transit.Data.DotLang
import Test.Transit.Data.Graph as Test.Transit.Data.Graph
import Test.Transit.Data.Html as Test.Transit.Data.Html
import Test.Transit.Render.Graphviz as Test.Transit.Render.Graphviz
import Test.Transit.Render.TransitionTable as Test.Transit.Render.TransitionTable
import Test.Transit.StateGraph as Test.Transit.StateGraph
import Test.Transit.Class.ExpandReturn as Test.Transit.Class.ExpandReturn
import Test.Transit.Class.MkUpdate as Test.Transit.Class.MkUpdate
import Test.Transit.DSL as Test.Transit.DSL
import Test.Transit.VariantUtils as Test.Transit.VariantUtils

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] do
    Test.Transit.spec
    Test.Transit.Class.CurryN.spec
    Test.Transit.Render.Theme.spec
    Test.Transit.Core.spec
    Test.Transit.Data.DotLang.spec
    Test.Transit.Data.Graph.spec
    Test.Transit.Data.Html.spec
    Test.Transit.Render.Graphviz.spec
    Test.Transit.Render.TransitionTable.spec
    Test.Transit.StateGraph.spec
    Test.Transit.Class.ExpandReturn.spec
    Test.Transit.Class.MkUpdate.spec
    Test.Transit.DSL.spec
    Test.Transit.VariantUtils.spec