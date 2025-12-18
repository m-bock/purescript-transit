module Test.Examples.ErrorHandling where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff)
import Test.Examples.SimpleDoor (Msg, State, SimpleDoorTransit)
import Test.Spec.Assertions (shouldEqual)
import Transit (match, mkUpdateEither, return, TransitError(..))
import Transit.VariantUtils (v)
import Test.Spec (Spec, describe, it)
import Effect (Effect)

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

spec :: Spec Unit
spec = do
  describe "ErrorHandling" do
    describe "update" do
      it "should return the correct state" do
        update (v @"DoorOpen") (v @"Close") `shouldEqual` Right (v @"DoorClosed")
      it "should return the correct state" do
        update (v @"DoorClosed") (v @"Close") `shouldEqual` Left IllegalTransitionRequest

main :: Effect Unit
main = do
  pure unit