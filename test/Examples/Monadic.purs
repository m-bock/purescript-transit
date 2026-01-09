module Examples.Monadic where

import Prelude

import Control.Monad.Writer (class MonadWriter, Writer, execWriter, tell)
import Effect (Effect)
import Examples.Door (Msg, State, DoorTransit)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (matchM, mkUpdateM, return)
import Transit.VariantUtils (v)

type Accum = Array String

update :: forall m. MonadWriter Accum m => State -> Msg -> m State
update = mkUpdateM @DoorTransit
  ( matchM @"DoorOpen" @"Close" \_ _ -> do
      tell [ "You just closed the door" ]
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ -> do
      tell [ "You just opened the door" ]
      pure $ return @"DoorOpen"
  )

walk :: Writer Accum State
walk = do
  let s0 = v @"DoorOpen"
  s1 <- update s0 (v @"Close")
  s2 <- update s1 (v @"Open")
  s3 <- update s2 (v @"Close")
  pure s3

specLogs :: Spec Unit
specLogs = do
  it "should return the correct state" do
    let
      logs :: Accum
      logs = execWriter walk

    logs `shouldEqual`
      [ "You just closed the door"
      , "You just opened the door"
      , "You just closed the door"
      ]

spec :: Spec Unit
spec = do
  describe "Monadic" do
    specLogs

main :: Effect Unit
main = do
  pure unit