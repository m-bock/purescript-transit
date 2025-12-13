module Test.Examples.Common where

import Prelude

import Data.Array (scanl)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldM)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Traversable (for_, scanr)
import Data.Tuple (Tuple(..), fst, snd)
import Data.Tuple.Nested (type (/\))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
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

mkSpec
  :: forall msg state
   . Eq state
  => Show state
  => (state -> msg -> state)
  -> state
  -> Array (msg /\ state)
  -> Spec Unit
mkSpec updateFn initState walk = describe "" do
  let
    msgs = map fst walk
    expectedStates = map snd walk

  it "should follow the walk" do
    let
      actualStates :: Array state
      actualStates = scanl updateFn initState msgs

    actualStates `shouldEqual` expectedStates

sameLengthPermutations :: forall a. Eq a => Array a -> Array (Array a)
sameLengthPermutations xs
  | Array.null xs = [ [] ]
  | otherwise =
      xs >>= \x ->
        (Array.cons x) <$> sameLengthPermutations (Array.delete x xs)