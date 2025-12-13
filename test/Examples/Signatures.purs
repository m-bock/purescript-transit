module Test.Examples.Signatures (update) where

import Prelude

import Data.Variant (Variant)
import Test.Examples.DoorWithPin (Msg, State, DoorTransit)
import Transit (match, mkUpdateGeneric)
import Transit.Core (ReturnState, ReturnStateVia)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

unimplemented :: forall a. a
unimplemented = unsafeCoerce "not yet implemented"

update :: State -> Msg -> State
update = mkUpdateGeneric @DoorTransit
  ( match @"DoorOpen" @"Close"
      ( \(state :: Unit) (msg :: Unit) ->
          unimplemented
            :: Variant ("DoorClosed" :: ReturnState Unit)
      )
  )
  ( match @"DoorClosed" @"Open"
      ( \(state :: Unit) (msg :: Unit) ->
          unimplemented
            :: Variant ("DoorOpen" :: ReturnState Unit)
      )
  )
  ( match @"DoorClosed" @"Lock"
      ( \(state :: Unit) (msg :: { newPin :: String }) ->
          unimplemented
            :: Variant ("DoorLocked" :: ReturnState { pin :: String })
      )
  )
  ( match @"DoorLocked" @"Unlock"
      ( \(state :: { pin :: String }) (msg :: { enteredPin :: String }) ->
          unimplemented
            :: Variant
                 ( "DoorClosed" :: ReturnStateVia "PinCorrect" Unit
                 , "DoorLocked" :: ReturnStateVia "PinIncorrect" { pin :: String }
                 )
      )
  )