module Test.Examples.Signatures (main, spec, update) where

import Data.Variant (Variant)
import Test.Examples.DoorWithPin (Msg, State, DoorWithPinTransit)
import Transit (match, mkUpdate)
import Transit.Core (Ret, RetVia)
import Unsafe.Coerce (unsafeCoerce)
import Test.Spec (Spec)
import Effect (Effect)
import Prelude

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

type Handler1 = {} -> {} -> Variant ("DoorClosed" :: Ret {})

type Handler2 = {} -> {} -> Variant ("DoorOpen" :: Ret {})

type Handler3 = {} -> { newPin :: String } -> Variant ("DoorLocked" :: Ret { activePin :: String })

type Handler4 =
  { activePin :: String }
  -> { enteredPin :: String }
  -> Variant
       ( "DoorClosed" :: RetVia "PinCorrect" {}
       , "DoorLocked" :: RetVia "PinIncorrect" { activePin :: String }
       )

spec :: Spec Unit
spec = do
  pure unit

main :: Effect Unit
main = do
  pure unit