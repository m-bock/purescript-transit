module Test.Examples.Signatures (update) where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Reflectable (reflectType)
import Data.Show.Generic (genericShow)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Class.Console as Console
import Test.Examples.DoorWithAlarm (Msg, State, DoorDSL)
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import Transit (type (:*), type (:@), type (>|), Empty, Transit, match, mkUpdateGeneric, return, returnVia)
import Transit.Core (ReturnState, ReturnStateVia)
import Transit.DSL (type (:?))
import Transit.Generators.Graphviz as TransitGraphviz
import Transit.Generators.TransitionTable as TransitTable
import Transit.StateGraph (mkStateGraph)
import Type.Function (type ($))
import Type.Proxy (Proxy(..))
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