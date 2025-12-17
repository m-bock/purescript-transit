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
      (unimplemented :: Handler1)
  )
  ( match @"DoorClosed" @"Open"
      (unimplemented :: Handler2)
  )
  ( match @"DoorClosed" @"Lock"
      (unimplemented :: Handler3)
  )
  ( match @"DoorLocked" @"Unlock"
      (unimplemented :: Handler4)
  )

type Handler1 = {} -> {} -> Variant ("DoorClosed" :: ReturnState {})

type Handler2 = {} -> {} -> Variant ("DoorOpen" :: ReturnState {})

type Handler3 = {} -> { newPin :: String } -> Variant ("DoorLocked" :: ReturnState { pin :: String })

type Handler4 =
  { pin :: String }
  -> { enteredPin :: String }
  -> Variant
       ( "DoorClosed" :: ReturnStateVia "PinCorrect" {}
       , "DoorLocked" :: ReturnStateVia "PinIncorrect" { pin :: String }
       )
