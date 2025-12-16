module Test.Examples.Monadic where

import Prelude

import Effect (Effect)
import Effect.Class.Console as Console
import Test.Examples.SimpleDoor (Msg, State, SimpleDoorTransit)
import Transit (matchM, mkUpdateGenericM, return)

update = 1

-- update :: State -> Msg -> Effect State
-- update = mkUpdateGenericM @SimpleDoorTransit
--   ( matchM @"DoorOpen" @"Close" \_ _ -> do
--       Console.log "You just closed the door"
--       pure $ return @"DoorClosed" unit
--   )
--   ( matchM @"DoorClosed" @"Open" \_ _ -> do
--       Console.log "You just opened the door"
--       pure $ return @"DoorOpen" unit
--   )
