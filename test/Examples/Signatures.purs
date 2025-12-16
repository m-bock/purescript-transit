module Test.Examples.Signatures (update) where

import Prelude

import Data.Variant (Variant)
import Test.Examples.DoorWithPin (Msg, State, DoorWithPinTransit)
import Transit (match, mkUpdate)
import Transit.Core (ReturnState, ReturnStateVia)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

unimplemented :: forall a. a
unimplemented = unsafeCoerce "not yet implemented"

update :: State -> Msg -> State
update = mkUpdate @DoorWithPinTransit
  ( match @"DoorOpen" @"Close"
      ( \(state :: {}) (msg :: {}) ->
          unimplemented
            :: Variant ("DoorClosed" :: ReturnState {})
      )
  )
  ( match @"DoorClosed" @"Open"
      ( \(state :: {}) (msg :: {}) ->
          unimplemented
            :: Variant ("DoorOpen" :: ReturnState {})
      )
  )
  ( match @"DoorClosed" @"Lock"
      ( \(state :: {}) (msg :: { newPin :: String }) ->
          unimplemented
            :: Variant ("DoorLocked" :: ReturnState { pin :: String })
      )
  )
  ( match @"DoorLocked" @"Unlock"
      ( \(state :: { pin :: String }) (msg :: { enteredPin :: String }) ->
          unimplemented
            :: Variant
                 ( "DoorClosed" :: ReturnStateVia "PinCorrect" {}
                 , "DoorLocked" :: ReturnStateVia "PinIncorrect" { pin :: String }
                 )
      )
  )