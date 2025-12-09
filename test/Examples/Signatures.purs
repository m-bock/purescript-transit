module Test.Examples.Signatures (update) where

import Prelude

import Data.Variant (Variant)
import Test.Examples.DoorWithAlarm (Msg, State, DoorDSL)
import Transit (match, mkUpdateGeneric)
import Transit.Core (ReturnState, ReturnStateVia)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  ( match @"DoorOpen" @"Close"
      ( \(state :: Unit) (msg :: Unit) ->
          unsafeCoerce "todo"
            :: Variant ("DoorClosed" :: ReturnState Unit)
      )
  )
  ( match @"DoorClosed" @"Open"
      ( \(state :: Unit) (msg :: Unit) ->
          unsafeCoerce "todo"
            :: Variant ("DoorOpen" :: ReturnState Unit)
      )
  )
  ( match @"DoorClosed" @"Lock"
      ( \(state :: Unit) (msg :: { newPin :: String }) ->
          unsafeCoerce "todo"
            :: Variant ("DoorLocked" :: ReturnState { attempts :: Int, pin :: String })
      )
  )
  ( match @"DoorLocked" @"Unlock"
      ( \(state :: { attempts :: Int, pin :: String }) (msg :: { enteredPin :: String }) ->
          unsafeCoerce "todo"
            :: Variant
                 ( "Alarm" :: ReturnStateVia "TooManyAttempts" Unit
                 , "DoorClosed" :: ReturnStateVia "PinCorrect" Unit
                 , "DoorLocked" :: ReturnStateVia "PinIncorrect" { attempts :: Int, pin :: String }
                 )
      )
  )