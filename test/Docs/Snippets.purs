module Test.Docs.Snippets where

import Examples.Door (Msg, State)
import Transit.VariantUtils (v)

doorOpen :: State
doorOpen = v @"DoorOpen" {}

doorClosed :: State
doorClosed = v @"DoorClosed" {}

close :: Msg
close = v @"Close" {}

open :: Msg
open = v @"Open" {}

doorOpenShort :: State
doorOpenShort = v @"DoorOpen"