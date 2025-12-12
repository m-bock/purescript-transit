module Test.Examples.GenerateTransitionTables (main) where

import Prelude

import Data.Reflectable (reflectType)
import Effect (Effect)
import Test.Examples.Door (DoorTransit)
import Transit.Generators.TransitionTable as TransitTable
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @DoorTransit)

  TransitTable.writeToFile "graphs/door.html" transit _
    { title = "Door" }

