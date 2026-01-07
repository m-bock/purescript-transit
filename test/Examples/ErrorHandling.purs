module Examples.ErrorHandling where

import Prelude

import Data.Maybe (Maybe(..))
import Examples.Door (Msg, State, DoorTransit)
import Test.Spec.Assertions (shouldEqual)
import Transit (match, mkUpdateMaybe, return)
import Transit.VariantUtils (v)
import Test.Spec (Spec, describe, it)
import Effect (Effect)

update :: State -> Msg -> Maybe State
update = mkUpdateMaybe @DoorTransit
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )

specSuccess :: Spec Unit
specSuccess = do
  it "should return the correct state" do
    update (v @"DoorOpen") (v @"Close") `shouldEqual` Just (v @"DoorClosed")

specFailure :: Spec Unit
specFailure = do
  it "should return the correct state" do
    update (v @"DoorOpen") (v @"Open") `shouldEqual` Nothing

spec :: Spec Unit
spec = do
  describe "ErrorHandling" do
    specSuccess
    specFailure

main :: Effect Unit
main = do
  pure unit