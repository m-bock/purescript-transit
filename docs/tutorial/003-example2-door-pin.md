# Example 2: Door with Pin

Full source code: _[test/Examples/DoorPin.purs](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs)_

![Door with Pin](assets/door-pin-header.jpg){width=50%}

Now let's extend our door to support PIN-based locking. In this enhanced version, you can lock the door with a PIN code, and then only unlock it by entering the correct PIN. This introduces two important concepts: **states with data** and **conditional transitions**.

## The State Machine

We add a new state `DoorLocked` and the new messages `Lock` and `Unlock`:

![Door with Pin state diagram](renders/door-pin_graph-dark.svg){.dark-light}

Notice the diamond node in the state diagram — this represents a conditional transition where the outcome depends on runtime data: The unlock operation can succeed (transitioning to `DoorClosed`) if the condition `PinCorrect` is met - or fail (staying in `DoorLocked`) when the condition `PinIncorrect` is met.

In the transition table the conditional transitions are expressed by the new "Guard" column. For most transitions however, this column is empty — these are unconditional transitions that always succeed.

<!-- PD_START:raw { filePath: renders/door-pin_table.md, wrapNl: true } -->

| State      |       | Message |       | Guard        |       | State      |
| ---------- | ----- | ------- | ----- | ------------ | ----- | ---------- |
| DoorOpen   | **⟶** | Close   |       |              | **⟶** | DoorClosed |
| DoorClosed | **⟶** | Open    |       |              | **⟶** | DoorOpen   |
| DoorClosed | **⟶** | Lock    |       |              | **⟶** | DoorLocked |
| DoorLocked | **⟶** | Unlock  | **?** | PinIncorrect | **⟶** | DoorLocked |
| DoorLocked | **⟶** | Unlock  | **?** | PinCorrect   | **⟶** | DoorClosed |

<!-- PD_END -->

Guard labels are not strictly required. Transitions can have multiple target states without explicit labels - like in the Countdown example. But the labels can be very useful to make the code and the state diagram more readable.

## The Classic Approach

Let's briefly recap how we would implement this using the classic approach.

### States and Message types

The PureScript types now include data in both states and messages:

<!-- PD_START:purs { filePath: test/Examples/Classic/DoorPin.purs, pick: [State, Msg] } -->

```purescript
data State
  = DoorOpen
  | DoorClosed
  | DoorLocked { storedPin :: String }

data Msg
  = Close
  | Open
  | Lock { newPin :: String }
  | Unlock { enteredPin :: String }
```

[test/Examples/Classic/DoorPin.purs (lines 10-27)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Classic/DoorPin.purs#L10-L27){.fileLink}

<!-- PD_END -->

### The update function

Accordingly the update function now needs to handle state and message data:

<!-- PD_START:purs
filePath: test/Examples/Classic/DoorPin.purs
pick:
  - update
-->

```purescript
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
```

[test/Examples/Classic/DoorPin.purs (lines 29-39)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Classic/DoorPin.purs#L29-L39){.fileLink}

<!-- PD_END -->

## Implementation using Transit

### State and Message Types

Also in the **Transit** approach we define `State` and `Msg` types. This time some cases of those types have data attached to them:

<!-- PD_START:purs { filePath: test/Examples/DoorPin.purs, pick: [State, Msg] } -->

```purescript
type State = Variant
  ( "DoorOpen" :: {}
  , "DoorClosed" :: {}
  , "DoorLocked" :: { storedPin :: String }
  )

type Msg = Variant
  ( "Close" :: {}
  , "Open" :: {}
  , "Lock" :: { newPin :: String }
  , "Unlock" :: { enteredPin :: String }
  )
```

[test/Examples/DoorPin.purs (lines 31-42)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs#L31-L42){.fileLink}

<!-- PD_END -->

### Transit Specification

In the DSL specification, we express conditional transitions by listing multiple possible target states:

<!-- PD_START:purs { filePath: test/Examples/DoorPin.purs, pick: [DoorPinTransit] } -->

```purescript
type DoorPinTransit =
  Transit
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :*
      ( "DoorLocked" :@ "Unlock"
          >| ("PinCorrect" :? "DoorClosed")
          >| ("PinIncorrect" :? "DoorLocked")
      )
```

[test/Examples/DoorPin.purs (lines 44-53)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs#L44-L53){.fileLink}

<!-- PD_END -->

The syntax `("PinCorrect" :? "DoorClosed") >| ("PinIncorrect" :? "DoorLocked")` indicates that the `Unlock` message from `DoorLocked` can transition to either state, depending on runtime conditions. The `:?` operator associates a condition label (like `"PinCorrect"`) with a target state, and `>|` chains multiple conditional outcomes together.

### The Update Function

The handlers in the update function now have access to both the matching state and message data, allowing you to implement the conditional runtime logic for the transition.

<!-- PD_START:purs { filePath: test/Examples/DoorPin.purs, pick: [update] } -->

```purescript
update :: State -> Msg -> State
update = mkUpdate @DoorPinTransit
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
  ( match @"DoorClosed" @"Lock" \_ msg ->
      return @"DoorLocked" { storedPin: msg.newPin }
  )
  ( match @"DoorLocked" @"Unlock" \state msg ->
      let
        isCorrect = state.storedPin == msg.enteredPin
      in
        if isCorrect then
          returnVia @"PinCorrect" @"DoorClosed"
        else
          returnVia @"PinIncorrect" @"DoorLocked" { storedPin: state.storedPin }
  )
```

[test/Examples/DoorPin.purs (lines 55-74)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs#L55-L74){.fileLink}

<!-- PD_END -->

The order of match handlers in `mkUpdate` must match the order of transitions in the DSL specification. The compiler _can_ detect if the returned state of a handler is legal for a given transition. However, it _cannot_ detect if an implementation forgets to return a possible case. For example, if a transition can return either `DoorClosed` or `DoorLocked`, but your handler always returns `DoorClosed`, the compiler would not detect this error. The compiler cannot verify whether your handler implements the conditional logic correctly, so missing a case is just one of many possible errors.

## Testing the update function

We'll use the same test function which we used in the previous example. Let's recap how it works quickly by looking at its type signature:

<!-- PD_START:purs
pick: [{tag: "signature_or_foreign", name: "assertWalk", filePath: test/Examples/Common.purs}]
-->

```purescript
assertWalk
  :: forall msg state
   . Eq state
  => Show state
  => (state -> msg -> state)
  -> state
  -> Array (msg /\ state)
  -> Aff Unit
```

<!-- PD_END -->

We want to start the state machine in the `DoorOpen` state and then follow this sequence of transitions:

1. `Close` the door, expect transition to `DoorClosed`
2. `Lock` the door with PIN "1234", expect transition to `DoorLocked` with the stored PIN
3. Attempt to `Unlock` with the wrong PIN "abcd", expect to stay in `DoorLocked` with the original PIN
4. `Unlock` with the correct PIN "1234", expect transition to `DoorClosed`
5. `Open` the door, expect transition to `DoorOpen`

In code this looks like this:

<!-- PD_START:purs { filePath: test/Examples/DoorPin.purs, pick: [specWalk] } -->

```purescript
specWalk :: Spec Unit
specWalk =
  it "should follow the walk and visit the expected intermediate states" do
    assertWalk update
      (v @"DoorOpen")
      [ v @"Close" ~> v @"DoorClosed"
      , v @"Lock" { newPin: "1234" } ~> v @"DoorLocked" { storedPin: "1234" }
      , v @"Unlock" { enteredPin: "abcd" } ~> v @"DoorLocked" { storedPin: "1234" }
      , v @"Unlock" { enteredPin: "1234" } ~> v @"DoorClosed"
      , v @"Open" ~> v @"DoorOpen"
      ]
```

[test/Examples/DoorPin.purs (lines 83-93)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs#L83-L93){.fileLink}

<!-- PD_END -->

Since this test passes, we can be pretty confident that the update function is correct.

## Generating Documentation

For generating the state diagram, we now add some more options to the `generate` function:

- `entryPoints`: The state machine will start in the `DoorOpen` state.
- `layout`: The state diagram will be displayed in landscape mode.

<!-- PD_START:purs { filePath: test/Examples/DoorPin.purs, pick: [generateGraphLight] } -->

```purescript
generateGraphLight :: Effect Unit
generateGraphLight = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate doorPinTransit \cfg -> cfg
      { theme = themeHarmonyLight
      , entryPoints = [ "DoorOpen" ]
      , layout = Landscape
      }

  FS.writeTextFile UTF8 "renders/door-pin_graph-light.dot" (Graphviz.toDotStr graph)
```

[test/Examples/DoorPin.purs (lines 103-113)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs#L103-L113){.fileLink}

<!-- PD_END -->

**Generated Output**: This creates the state diagram we saw earlier in this example.

🔗 <a href="https://dreampuf.github.io/GraphvizOnline/?url=https://m-bock.github.io/purescript-transit/renders/door-pin_graph-light.dot" target="_blank">View diagram on GraphvizOnline</a>

The generation of the transition table works exactly the same as in the previous example.

## Conclusion

This example demonstrates how **Transit** extends beyond simple state machines to handle real-world complexity:

- **States and messages with data**: Both states and messages can carry data (like `storedPin` in `DoorLocked` or `newPin` in `Lock`), and handlers receive this data.
- **Conditional transitions**: The DSL supports transitions with multiple possible outcomes using guard labels (`PinCorrect` and `PinIncorrect`). The type system ensures that conditional transitions can only return valid target states, and each outcome must be associated with its corresponding guard label.
