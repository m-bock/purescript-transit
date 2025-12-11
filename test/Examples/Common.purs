module Test.Examples.Common where

import Prelude

import Data.Array as Array
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Effect.Console as Console
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

checkWalk
  :: forall msg state
   . Eq state
  => Show state
  => (state -> msg -> state)
  -> Walk msg state
  -> Aff Unit
checkWalk updateFn { initialState, steps } = do
  case Array.uncons steps of
    Just { head, tail } -> do
      let newState = updateFn initialState head.msg
      newState `shouldEqual` head.state
      liftEffect $ Console.log $ "newState: " <> show newState
      checkWalk updateFn { initialState: newState, steps: tail }
    Nothing -> pure unit
