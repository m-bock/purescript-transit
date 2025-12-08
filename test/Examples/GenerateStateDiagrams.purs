module Test.Examples.GenerateStateDiagrams (main) where

import Prelude

import Data.Reflectable (reflectType)
import Effect (Effect)
import Test.Examples.Door (DoorDSL)
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.StateGraph (mkStateGraph)
import Type.Prelude (Proxy(..))

main :: Effect Unit
main = do
  let
    stateGraph = mkStateGraph (reflectType (Proxy @DoorDSL))

  TransitGraphviz.writeToFile (_ { title = "Door" }) stateGraph "graphs/door.dot"

