module Examples.ErrorHandlingMonadic where

import Prelude

import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Writer (class MonadWriter, Writer, execWriter, tell)
import Data.Maybe (Maybe)
import Effect (Effect)
import Examples.Door (Msg, State, DoorTransit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (matchM, mkUpdateMaybeM, return)
import Transit.VariantUtils (v)

type Accum = Array String

update :: forall m. MonadWriter Accum m => State -> Msg -> m (Maybe State)
update = mkUpdateMaybeM @DoorTransit
  ( matchM @"DoorOpen" @"Close" \_ _ -> do
      tell [ "Closing door" ]
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ -> do
      tell [ "Opening door" ]
      pure $ return @"DoorOpen"
  )

walk :: MaybeT (Writer Accum) State
walk = do
  let s0 = v @"DoorOpen"
  s1 <- MaybeT $ update s0 (v @"Close")
  s2 <- MaybeT $ update s1 (v @"Open")
  s3 <- MaybeT $ update s2 (v @"Close")
  s4 <- MaybeT $ update s3 (v @"Close") -- here we request illegal transition
  s5 <- MaybeT $ update s4 (v @"Open")
  pure s5

spec :: Spec Unit
spec = do
  describe "ErrorHandlingMonadic" do
    it "should return the correct state" do
      let
        logs :: Array String
        logs = execWriter (runMaybeT walk)
      logs `shouldEqual`
        [ "Closing door"
        , "Opening door"
        , "Closing door"
        ]

main :: Effect Unit
main = do
  pure unit