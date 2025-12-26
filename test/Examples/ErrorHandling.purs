module Test.Examples.ErrorHandling where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Test.Examples.SimpleDoor (Msg, State, SimpleDoorTransit)
import Test.Spec.Assertions (shouldEqual)
import Transit (match, mkUpdateEither, return)
import Transit.VariantUtils (v)
import Test.Spec (Spec, describe, it)
import Effect (Effect)

update :: State -> Msg -> Maybe State
update = mkUpdateEither @SimpleDoorTransit
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )

assert1 :: Aff Unit
assert1 =
  update (v @"DoorOpen") (v @"Close") `shouldEqual` Just (v @"DoorClosed")

assert2 :: Aff Unit
assert2 =
  update (v @"DoorClosed") (v @"Close") `shouldEqual` Nothing

spec :: Spec Unit
spec = do
  describe "ErrorHandling" do
    describe "update" do
      it "should return the correct state" do
        update (v @"DoorOpen") (v @"Close") `shouldEqual` Just (v @"DoorClosed")
      it "should return the correct state" do
        update (v @"DoorClosed") (v @"Close") `shouldEqual` Nothing

main :: Effect Unit
main = do
  pure unit