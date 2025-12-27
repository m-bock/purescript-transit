module Examples.ErrorHandlingMonadic where

import Prelude

import Data.Identity (Identity(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Examples.DoorSimple (Msg, State, DoorSimpleTransit)
import Test.Spec.Assertions (shouldEqual)
import Transit (matchM, mkUpdateMaybeM, return)
import Transit.VariantUtils (v)
import Test.Spec (Spec, describe, it)
import Effect (Effect)

update :: forall m. Monad m => State -> Msg -> m (Maybe State)
update = mkUpdateMaybeM @DoorSimpleTransit
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
  describe "ErrorHandlingMonadic" do
    describe "update" do
      it "should return the correct state" do
        update (v @"DoorOpen") (v @"Close") `shouldEqual` Identity (Just (v @"DoorClosed"))
      it "should return the correct state" do
        update (v @"DoorClosed") (v @"Close") `shouldEqual` Identity Nothing

main :: Effect Unit
main = do
  pure unit