module Test.Examples.GenerateTransitionTables (main) where

import Prelude

import Data.Reflectable (reflectType)
import Effect (Effect)
import Test.Examples.Door (DoorDSL)
import Transit.Core (TransitCore_)
import Transit.Generators.TransitionTable as TransitTable
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  let
    transit :: TransitCore_
    transit = reflectType (Proxy @DoorDSL)

  TransitTable.writeToFile (_ { title = "Door" }) transit "graphs/door.html"

