module Test.Examples.Monadic where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Test.Examples.SimpleDoor (Msg, State, SimpleDoorTransit)
import Transit (matchM, mkUpdateM, return)
import Test.Spec (Spec)

update :: State -> Msg -> Effect State
update = mkUpdateM @SimpleDoorTransit
  ( matchM @"DoorOpen" @"Close" \_ _ -> do
      Console.log "You just closed the door"
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ -> do
      Console.log "You just opened the door"
      pure $ return @"DoorOpen"
  )

spec :: Spec Unit
spec = do
  pure unit

main :: Effect Unit
main = do
  pure unit