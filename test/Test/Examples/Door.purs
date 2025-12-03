module Test.Examples.Door where

import Prelude

import Data.Function (($))
import Data.Generic.Rep (class Generic)
import Data.Identity (Identity)
import Data.Tuple (Tuple)
import Data.Tuple.Nested (type (/\), (/\))
import Effect (Effect)
import Transit (match, mkUpdateGeneric, return)
import Transit (type (:*), type (:>), type (:@), MkStateSpec, mkUpdateGeneric)
import Transit.Core (MkReturn, MkStateGraph, MkTransition, ReturnState(..), StateGraph)
import Transit.Gen.Graphviz as TransitGraphviz
import Transit.MkUpdate (mkUpdate)
import Transit.Tmp (build)
import Transit.Util (type (:<), Generically, Id)
import Type.Data.List (Nil')
import Type.Data.List as L
import Type.Function (type ($))

-- type DoorSpec =
--   MkStateSpec
--     :$ ("DoorIsOpen" :+ "CloseTheDoor" := "DoorIsClosed")
--     :* ("DoorIsClosed" :+ "OpenTheDoor" := "DoorIsOpen")

-- type DSL = MkStateSpec
--   $ ("DoorIsOpen" :@ "CloseTheDoor" :> "DoorIsClosed")
--       :* ("DoorIsClosed" :@ "OpenTheDoor" :> "DoorIsOpen")

type DoorStateGraph :: StateGraph
type DoorStateGraph = MkStateGraph
  ( Id $ (MkTransition "DoorIsOpen" "CloseTheDoor" (MkReturn "DoorIsClosed" L.:> Nil'))
      L.:> (MkTransition "DoorIsClosed" "OpenTheDoor" (MkReturn "DoorIsOpen" L.:> Nil'))
      L.:> Nil'
  )

-- type MyStateGraph :: StateGraph
-- type MyStateGraph = MkStateGraph
--   ( Nil'
--       :< (MkTransition "State1" "Msg1" (Nil' :< MkReturn "State2"))
--       :< (MkTransition "State2" "Msg2" (Nil' :< MkReturnVia "foo" "State3" :< MkReturn "State1"))
--   )

-- :* ("DoorIsClosed" :@ "OpenTheDoor" :> "DoorIsOpen")

data State = DoorIsOpen {} | DoorIsClosed { foo :: Int }

data Msg = CloseTheDoor {} | OpenTheDoor {}

derive instance Generic State _
derive instance Generic Msg _

-- update :: Msg -> State -> State
-- update msg state = case state, msg of
--   DoorIsOpen, CloseTheDoor -> DoorIsClosed
--   DoorIsClosed, OpenTheDoor -> DoorIsOpen
--   _, _ -> state

update2 :: Msg -> State -> State
update2 = build (mkUpdateGeneric @DoorStateGraph)
  ( match @"DoorIsOpen" @"CloseTheDoor" \msg state ->
      return @"DoorIsClosed" { foo: 2 }
  )
  ( match @"DoorIsClosed" @"OpenTheDoor" \msg state ->
      return @"DoorIsOpen" {}
  )

--(unit /\ (match @"DoorIsOpen" @"CloseTheDoor" (\msg state -> return @"DoorIsClosed" (ReturnState { foo: 2 }))))

--  build (mkUpdateGeneric @DoorSpec) ?a ?b

-- (match @"DoorIsOpen" @"CloseTheDoor" (\msg state -> return @"DoorIsClosed"))
-- (match @"DoorIsClosed" @"OpenTheDoor" (\msg state -> return @"DoorIsOpen"))

-- & match @"DoorIsClosed" @"OpenTheDoor" (\msg state -> return @"DoorIsOpen")

main :: Effect Unit
main = do
  pure unit
--TransitGraphviz.writeToFile_ @DoorSpec "graphs/door-graph.dot"
