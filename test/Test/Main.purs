module Test.Main (main) where

import Prelude

import Effect (Effect)
import Patchdown as Patchdown
import Test.Examples.BridgesKoenigsberg as Test.Examples.BridgesKoenigsberg
import Test.Examples.ColorRing as Test.Examples.ColorRing
import Test.Examples.Door as Test.Examples.Door
import Test.Examples.DoorWithLock as Test.Examples.DoorWithLock
import Test.Examples.DoorWithPin as Test.Examples.DoorWithPin
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)
import Test.Transit as Test.Transit
import Test.Transit.Colors as Test.Transit.Colors
import Test.Transit.Core as Test.Transit.Core
import Test.Transit.CurryN as Test.Transit.CurryN
import Test.Transit.DotLang as Test.Transit.DotLang
import Test.Transit.Gen.Graphviz as Test.Transit.Gen.Graphviz
import Test.Transit.Gen.TransitionTable as Test.Transit.Gen.TransitionTable
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
    Test.Transit.Gen.Graphviz.spec
    Test.Transit.Gen.TransitionTable.spec
    Test.Transit.Graph.spec
    Test.Transit.Html.spec
    Test.Transit.StateGraph.spec
    Test.Transit.Util.spec
  --Test.Examples.ColorRing.main
  Test.Examples.Door.main
  --Test.Examples.DoorWithLock.main
  --Test.Examples.DoorWithPin.main
  Test.Examples.BridgesKoenigsberg.main
  Test.Examples.ColorRing.main

  Patchdown.main

