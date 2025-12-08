module Test.Examples.GenerateTransitionTables (main) where

import Prelude

import Data.Reflectable (reflectType)
import Effect (Effect)
import Test.Examples.Door (DoorDSL)
import Transit.Generators.TransitionTable as TransitTable
import Transit.StateGraph (mkStateGraph)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  let
    stateGraph = mkStateGraph (reflectType (Proxy @DoorDSL))

  TransitTable.writeToFile (_ { title = "Door" }) stateGraph "graphs/door.html"

