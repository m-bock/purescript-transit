module Test.Transit.StateGraph
  ( spec
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.Set as Set
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Core (Match(..), Return(..), TransitCore(..))
import Transit.Data.Graph as Graph
import Transit.StateGraph (mkStateGraph)

spec :: Spec Unit
spec = do
  describe "Transit.StateGraph" do
    describe "complex state graph" do
      it "creates a complex state graph" do
        let
          transitCore = TransitCore
            [ Match "State1" "Msg1" [ Return "State2" ]
            , Match "State2" "Msg2" [ Return "State3", Return "State1" ]
            , Match "State3" "Msg3" [ Return "State1", ReturnVia "Guard1" "State2", ReturnVia "Guard2" "State3" ]
            ]
          graph = mkStateGraph transitCore
        Graph.getEdges graph `shouldEqual` Set.fromFoldable
          [ { fromNode: "State1", toNode: "State2", edgeLabel: { msg: "Msg1", guard: Nothing } }
          , { fromNode: "State2", toNode: "State3", edgeLabel: { msg: "Msg2", guard: Nothing } }
          , { fromNode: "State2", toNode: "State1", edgeLabel: { msg: "Msg2", guard: Nothing } }
          , { fromNode: "State3", toNode: "State1", edgeLabel: { msg: "Msg3", guard: Nothing } }
          , { fromNode: "State3", toNode: "State2", edgeLabel: { msg: "Msg3", guard: Just "Guard1" } }
          , { fromNode: "State3", toNode: "State3", edgeLabel: { msg: "Msg3", guard: Just "Guard2" } }
          ]