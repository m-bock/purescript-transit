module Test.Examples.Monadic where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Test.Examples.DoorWithLock (Msg, State, DoorDSL)
import Transit (matchM, mkUpdateGenericM, return)

update :: State -> Msg -> Effect State
update = mkUpdateGenericM @DoorDSL
  ( matchM @"DoorOpen" @"Close" \_ _ -> do
      Console.log "You just closed the door"
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ -> do
      Console.log "You just opened the door"
      pure $ return @"DoorOpen"
  )
  ( matchM @"DoorClosed" @"Lock" \_ _ -> do
      Console.log "You just locked the door"
      pure $ return @"DoorLocked"
  )
  ( matchM @"DoorLocked" @"Unlock" \_ _ -> do
      Console.log "You just unlocked the door"
      pure $ return @"DoorClosed"
  )
