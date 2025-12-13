module Test.Examples.Common where

import Prelude

import Data.Array (scanl)
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

hasEulerCircle :: forall e n. Ord n => Ord e => Graph e n -> Boolean
hasEulerCircle g = true
  && Graph.isUndirected g
  && countOddOutgoingEdges g == 0

hasEulerTrail :: forall e n. Ord n => Ord e => Graph e n -> Boolean
hasEulerTrail g = true
  && Graph.isUndirected g
  && (countOddOutgoingEdges g == 0 || countOddOutgoingEdges g == 2)

countOddOutgoingEdges :: forall e n. Ord n => Ord e => Graph e n -> Int
countOddOutgoingEdges g =
  let
    nodes = Graph.getNodes g
  in
    Array.length $ Array.filter
      (\node -> Int.odd $ Set.size (Graph.getOutgoingEdges node g))
      (Set.toUnfoldable nodes)

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