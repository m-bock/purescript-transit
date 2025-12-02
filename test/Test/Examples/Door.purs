module Test.Examples.Door where

import Prelude

import Data.Generic.Rep (class Generic)
import Effect (Effect)
import Transit (MkStateSpec, type (:*), type (:@), type (:>))
import Transit.Gen.Graphviz as TransitGraphviz

type DoorSpec = MkStateSpec
  :* ("DoorIsOpen" :@ "CloseTheDoor" :> "DoorIsClosed")
  :* ("DoorIsClosed" :@ "OpenTheDoor" :> "DoorIsOpen")

data State = DoorIsOpen | DoorIsClosed

data Msg = CloseTheDoor | OpenTheDoor

derive instance Generic State _
derive instance Generic Msg _

update :: Msg -> State -> State
update msg state = case state, msg of
  DoorIsOpen, CloseTheDoor -> DoorIsClosed
  DoorIsClosed, OpenTheDoor -> DoorIsOpen
  _, _ -> state

-- update2 :: Msg -> State -> State
-- update2 = mkUpdateG @DoorSpec $
--   unit
--     & match @"DoorIsOpen" @"CloseTheDoor" (\msg state -> return @"DoorIsClosed")
--     & match @"DoorIsClosed" @"OpenTheDoor" (\msg state -> return @"DoorIsOpen")

main :: Effect Unit
main = do
  TransitGraphviz.writeToFile_ @DoorSpec "graphs/door-graph.dot"
