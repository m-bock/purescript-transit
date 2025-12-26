module Test.Examples.ErrorHandling2 where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Test.Examples.SimpleDoor (Msg, State, SimpleDoorTransit)
import Test.Spec.Assertions (shouldEqual)
import Transit (matchM, mkUpdateEitherM, return)
import Transit.VariantUtils (v)
import Test.Spec (Spec, describe, it)
import Effect (Effect)

update :: forall m. Monad m => State -> Msg -> m (Maybe State)
update = mkUpdateEitherM @SimpleDoorTransit
  ( matchM @"DoorOpen" @"Close" \_ _ ->
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ ->
      pure $ return @"DoorOpen"
  )

assert1 :: Aff Unit
assert1 =
  update (v @"DoorOpen") (v @"Close") `shouldEqual` Identity (Just (v @"DoorClosed"))

assert2 :: Aff Unit
assert2 =
  update (v @"DoorClosed") (v @"Close") `shouldEqual` Identity Nothing

spec :: Spec Unit
spec = do
  describe "ErrorHandling2" do
    describe "update" do
      it "should return the correct state" do
        update (v @"DoorOpen") (v @"Close") `shouldEqual` Identity (Just (v @"DoorClosed"))
      it "should return the correct state" do
        update (v @"DoorClosed") (v @"Close") `shouldEqual` Identity Nothing

main :: Effect Unit
main = do
  pure unit