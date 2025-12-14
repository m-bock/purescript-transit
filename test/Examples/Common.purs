module Test.Examples.Common where

import Prelude

import Data.Array (fromFoldable, scanl)
import Data.Array as Array
import Data.Int as Int
import Data.Set as Set
import Data.Tuple (fst, snd)
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit.Data.Graph (Graph)
import Transit.Data.Graph as Graph
import Transit.StateGraph (StateNode, StateGraph)

nodeDegree :: StateNode -> StateGraph -> Int
nodeDegree state graph = Set.size (Graph.getOutgoingEdges state graph)

hasEulerTrail :: StateGraph -> Boolean
hasEulerTrail graph =
  let
    nodes :: Array StateNode
    nodes = fromFoldable (Graph.getNodes graph)

    countEdgesByNode :: Array Int
    countEdgesByNode = map (\node -> Set.size (Graph.getOutgoingEdges node graph)) nodes

    sumOddEdges :: Int
    sumOddEdges = (Array.length <<< Array.filter Int.odd) countEdgesByNode
  in
    sumOddEdges == 2 || sumOddEdges == 0

assertWalk
  :: forall msg state
   . Eq state
  => Show state
  => (state -> msg -> state)
  -> state
  -> Array (msg /\ state)
  -> Aff Unit
assertWalk updateFn initState walk = do
  let
    msgs :: Array msg
    msgs = map fst walk

    expectedStates :: Array state
    expectedStates = map snd walk

    actualStates :: Array state
    actualStates = scanl updateFn initState msgs

  actualStates `shouldEqual` expectedStates

sameLengthPermutations :: forall a. Eq a => Array a -> Array (Array a)
sameLengthPermutations xs
  | Array.null xs = [ [] ]
  | otherwise =
      xs >>= \x ->
        (Array.cons x) <$> sameLengthPermutations (Array.delete x xs)