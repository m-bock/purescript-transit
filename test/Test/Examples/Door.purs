module Test.Examples.Door where

import Prelude

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Transit (type (:*), type (:@), Empty, Wrap, match, mkUpdateGeneric, return_, type (>|))
import Transit.Gen.Graphviz as TransitGraphviz
import Type.Function (type ($))

data State = DoorIsOpen | DoorIsClosed

data Msg = CloseTheDoor | OpenTheDoor

derive instance Generic State _
derive instance Generic Msg _

update' :: Msg -> State -> State
update' msg state = case state, msg of
  DoorIsOpen, CloseTheDoor -> DoorIsClosed
  DoorIsClosed, OpenTheDoor -> DoorIsOpen
  _, _ -> state

type Spec =
  Wrap $ Empty
    :* ("DoorIsOpen" :@ "CloseTheDoor" >| "DoorIsClosed")
    :* ("DoorIsClosed" :@ "OpenTheDoor" >| "DoorIsOpen")

update :: State -> Msg -> State
update = mkUpdateGeneric @Spec
  (match @"DoorIsOpen" @"CloseTheDoor" \_ _ -> return_ @"DoorIsClosed")
  (match @"DoorIsClosed" @"OpenTheDoor" \_ _ -> return_ @"DoorIsOpen")

main :: Effect Unit
main = do
  pure unit
  TransitGraphviz.writeToFile_ @Spec "graphs/door-graph.dot"
