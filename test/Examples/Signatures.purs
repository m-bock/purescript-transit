module Test.Examples.Signatures (update) where

import Prelude

import Data.Identity (Identity)
import Data.Variant (Variant)
import Test.Examples.DoorWithPin (Msg, State, DoorWithPinTransit)
import Transit (match, match', mkUpdate)
import Transit.Core (MatchImpl, ReturnState, Via)
import Unsafe.Coerce (unsafeCoerce)

--------------------------------------------------------------------------------
--- transit Approach
--------------------------------------------------------------------------------

unimplemented :: forall a. a
unimplemented = unsafeCoerce "not yet implemented"

update :: State -> Msg -> State
update = mkUpdate @DoorWithPinTransit
  (match @"DoorOpen" @"Close" (unimplemented :: Handler1))
  (match @"DoorClosed" @"Open" (unimplemented :: Handler2))
  (match @"DoorClosed" @"Lock" (unimplemented :: Handler3))
  (match @"DoorLocked" @"Unlock" (unimplemented :: Handler4))

type Handler1 = {} -> {} -> Variant ("DoorClosed" :: {})

type Handler2 = {} -> {} -> Variant ("DoorOpen" :: {})

type Handler3 = {} -> { newPin :: String } -> Variant ("DoorLocked" :: { activePin :: String })

type Handler4 =
  { activePin :: String }
  -> { enteredPin :: String }
  -> Variant
       ( "DoorClosed" :: Via "PinCorrect" {}
       , "DoorLocked" :: Via "PinIncorrect" { activePin :: String }
       )
