module Docs.Main where

import Prelude

import Effect (Effect)
import Examples.Door as Examples.Door
import Examples.Classic.Door as Examples.Classic.Door
import Examples.BridgesKoenigsberg as Examples.BridgesKoenigsberg
import Examples.ColorRing as Examples.ColorRing
import Examples.DoorPin as Examples.DoorPin
import Examples.HouseSantaClaus as Examples.HouseSantaClaus
import Examples.ErrorHandling as Examples.ErrorHandling
import Examples.ErrorHandlingMonadic as Examples.ErrorHandlingMonadic
import Examples.Monadic as Examples.Monadic
import Examples.Common as Examples.Common
import Examples.DoorReadme as Examples.DoorReadme
import Examples.CountDown as Examples.CountDown
import Examples.Circle as Examples.Circle
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

spec :: Spec Unit
spec = do
  Examples.Door.spec
  Examples.Classic.Door.spec
  Examples.DoorPin.spec
  Examples.BridgesKoenigsberg.spec
  Examples.HouseSantaClaus.spec
  Examples.ColorRing.spec
  Examples.ErrorHandling.spec
  Examples.ErrorHandlingMonadic.spec
  Examples.Monadic.spec
  Examples.Common.spec
  Examples.CountDown.spec

runExamples :: Effect Unit
runExamples = do
  Examples.Door.main
  Examples.DoorPin.main
  Examples.BridgesKoenigsberg.main
  Examples.HouseSantaClaus.main
  Examples.ColorRing.main
  Examples.ErrorHandling.main
  Examples.ErrorHandlingMonadic.main
  Examples.Monadic.main
  Examples.Common.main
  Examples.DoorReadme.main
  Examples.CountDown.main
  Examples.Circle.main

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] spec

  runExamples

