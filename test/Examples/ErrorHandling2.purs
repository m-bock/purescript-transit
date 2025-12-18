module Test.Examples.ErrorHandling2 where

import Prelude

import Data.Either (Either(..))
import Data.Identity (Identity(..))
import Effect.Aff (Aff)
import Test.Examples.SimpleDoor (Msg, State, SimpleDoorTransit)
import Test.Spec.Assertions (shouldEqual)
import Transit (matchM, mkUpdateEitherM, return, TransitError(..))
import Transit.VariantUtils (v)
import Test.Spec (Spec, describe, it)
import Effect (Effect)

update :: forall m. Monad m => State -> Msg -> m (Either TransitError State)
update = mkUpdateEitherM @SimpleDoorTransit
  ( matchM @"DoorOpen" @"Close" \_ _ ->
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ ->
      pure $ return @"DoorOpen"
  )

assert1 :: Aff Unit
assert1 =
  update (v @"DoorOpen") (v @"Close") `shouldEqual` Identity (Right (v @"DoorClosed"))

assert2 :: Aff Unit
assert2 =
  update (v @"DoorClosed") (v @"Close") `shouldEqual` Identity (Left IllegalTransitionRequest)

spec :: Spec Unit
spec = do
  describe "ErrorHandling2" do
    describe "update" do
      it "should return the correct state" do
        update (v @"DoorOpen") (v @"Close") `shouldEqual` Identity (Right (v @"DoorClosed"))
      it "should return the correct state" do
        update (v @"DoorClosed") (v @"Close") `shouldEqual` Identity (Left IllegalTransitionRequest)

main :: Effect Unit
main = do
  pure unit