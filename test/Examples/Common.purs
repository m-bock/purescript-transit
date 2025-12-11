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
import Data.Traversable (scanr)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Test.Spec (Spec, it)
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

type Step msg state = { msg :: msg, state :: state }

type Walk msg state =
  { initialState :: state
  , steps :: Array (Step msg state)
  }

runWalk :: forall msg state. (state -> msg -> state) -> Walk msg state -> Array state
runWalk updateFn { initialState, steps } = do
  scanl (\state step -> updateFn state step.msg) initialState steps
