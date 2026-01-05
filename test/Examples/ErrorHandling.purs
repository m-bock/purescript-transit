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