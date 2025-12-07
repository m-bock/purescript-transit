module Docs.Main where

import Prelude

import Effect (Effect)
import Patchdown as Patchdown
import Test.Examples.Door as Test.Examples.Door
import Test.Examples.BridgesKoenigsberg as Test.Examples.BridgesKoenigsberg
import Test.Examples.ColorRing as Test.Examples.ColorRing
import Test.Examples.DoorWithLock as Test.Examples.DoorWithLock

main :: Effect Unit
main = do
  --Test.Examples.ColorRing.main
  Test.Examples.Door.main
  Test.Examples.DoorWithLock.main
  --Test.Examples.DoorWithPin.main
  Test.Examples.BridgesKoenigsberg.main
  Test.Examples.ColorRing.main

  Patchdown.main