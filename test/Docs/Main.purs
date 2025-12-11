module Docs.Main where

import Prelude

import Effect (Effect)
import Patchdown as Patchdown
import Test.Examples.Door as Test.Examples.Door
import Test.Examples.BridgesKoenigsberg as Test.Examples.BridgesKoenigsberg
import Test.Examples.ColorRing as Test.Examples.ColorRing
import Test.Examples.DoorWithPin as Test.Examples.DoorWithPin
import Test.Examples.DoorWithAlarm as Test.Examples.DoorWithAlarm
import Test.Examples.HouseOfSantaClaus as Test.Examples.HouseOfSantaClaus
import Test.Spec (Spec)
import Test.Spec.Reporter.Console (consoleReporter)
import Test.Spec.Runner.Node (runSpecAndExitProcess)

spec :: Spec Unit
spec = do
  Test.Examples.Door.spec
  Test.Examples.DoorWithPin.spec
  Test.Examples.DoorWithAlarm.spec
  Test.Examples.BridgesKoenigsberg.spec
  Test.Examples.HouseOfSantaClaus.spec
  Test.Examples.ColorRing.spec

main :: Effect Unit
main = do
  runSpecAndExitProcess [ consoleReporter ] spec

  Test.Examples.Door.main
  Test.Examples.DoorWithPin.main
  Test.Examples.DoorWithAlarm.main
  Test.Examples.BridgesKoenigsberg.main
  Test.Examples.HouseOfSantaClaus.main
  Test.Examples.ColorRing.main

  Patchdown.main