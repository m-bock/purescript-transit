module Test.Examples.ErrorHandling where

import Prelude

import Data.Either (Either(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff)
import Test.Examples.SimpleDoor (Msg, State, SimpleDoorTransit)
import Test.Spec.Assertions (shouldEqual)
import Transit (match, mkUpdateEither, return)
import Transit.Class.MkUpdate (TransitError(..))
import Transit.VariantUtils (v)

update :: State -> Msg -> Either TransitError State
update = mkUpdateEither @SimpleDoorTransit
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )

assert1 :: Aff Unit
assert1 =
  update (v @"DoorOpen") (v @"Close") `shouldEqual` Right (v @"DoorClosed")

assert2 :: Aff Unit
assert2 =
  update (v @"DoorClosed") (v @"Close") `shouldEqual` Left IllegalTransitionRequest

