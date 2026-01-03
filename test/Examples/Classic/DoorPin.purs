module Examples.Classic.DoorPin (State(..), Msg(..), update) where

import Prelude

--------------------------------------------------------------------------------
--- Classic Approach
--------------------------------------------------------------------------------

data State
  = DoorOpen
  | DoorClosed
  | DoorLocked { storedPin :: String }

data Msg
  = Close
  | Open
  | Lock { newPin :: String }
  | Unlock { enteredPin :: String }

update :: State -> Msg -> State
update state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  DoorClosed, Lock { newPin } -> DoorLocked { storedPin: newPin }
  DoorLocked { storedPin }, Unlock { enteredPin } ->
    if storedPin == enteredPin then
      DoorClosed
    else
      DoorLocked { storedPin }
  _, _ -> state

