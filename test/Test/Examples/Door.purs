module Test.Examples.Door where

import Prelude

import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Transit (match, mkUpdateGeneric, return, return_)
import Transit (type (:*), type (:@), Empty, Wrap, mkUpdateGeneric)
import Transit.Core (MkReturn, MkStateGraph, MkTransition, ReturnState(..), StateGraph)
import Transit.DSL (type (>|))
import Transit.Gen.Graphviz as TransitGraphviz
import Transit.MkUpdate (mkUpdate)
import Transit.Tmp (build)
import Transit.Util (Generically)
import Type.Data.List (Nil')
import Type.Data.List as L
import Type.Function (type ($))

data State = DoorIsOpen | DoorIsClosed

data Msg = CloseTheDoor | OpenTheDoor

derive instance Generic State _
derive instance Generic Msg _

update :: Msg -> State -> State
update msg state = case state, msg of
  DoorIsOpen, CloseTheDoor -> DoorIsClosed
  DoorIsClosed, OpenTheDoor -> DoorIsOpen
  _, _ -> state

type Spec =
  Wrap $ Empty
    :* ("DoorIsOpen" :@ "CloseTheDoor" >| "DoorIsClosed")
    :* ("DoorIsClosed" :@ "OpenTheDoor" >| "DoorIsOpen")

update2 :: State -> Msg -> State
update2 = build (mkUpdateGeneric @Spec)
  ( match @"DoorIsOpen" @"CloseTheDoor" \state msg ->
      return_ @"DoorIsClosed"
  )
  ( match @"DoorIsClosed" @"OpenTheDoor" \state msg ->
      return_ @"DoorIsOpen"
  )

main :: Effect Unit
main = do
  pure unit
  TransitGraphviz.writeToFile_ @Spec "graphs/door-graph.dot"
