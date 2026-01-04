# Example 2: Door with Pin

> Full source code: _[test/Examples/DoorPin.purs](test/Examples/DoorPin.purs)_

<img src="assets/door-pin-header.jpg" width="450" />

Now let's extend our door to support PIN-based locking. In this enhanced version, you can lock the door with a PIN code, and then only unlock it by entering the correct PIN. This introduces two important concepts: **states with data** and **conditional transitions**.

## The State Machine

We add a new state `DoorLocked` and the new messages `Lock` and `Unlock`:

**State Diagram:** _Door with Pin_

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="renders/door-pin-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="renders/door-pin-light.svg">
  <img alt="Door with Pin state diagram" class="state-diagram" src="renders/door-pin-light.svg">
</picture>

Notice the diamond node in the state diagram — this represents a conditional transition where the outcome depends on runtime data: The unlock operation can succeed (transitioning to `DoorClosed`) if the condition `PinCorrect` is met - or fail (staying in `DoorLocked`) when the condition `PinIncorrect` is met.

In the transition table the conditional transitions are expressed by the new "Guard" column. For most transitions however, this column is empty — these are unconditional transitions that always succeed.

<!-- PD_START:raw
filePath: renders/door-pin.md
wrapNl: true
-->
| State      |       | Message |       | Guard        |       | State      |
| ---------- | ----- | ------- | ----- | ------------ | ----- | ---------- |
| DoorOpen   | **⟶** | Close   |       |              | **⟶** | DoorClosed |
| DoorClosed | **⟶** | Open    |       |              | **⟶** | DoorOpen   |
| DoorClosed | **⟶** | Lock    |       |              | **⟶** | DoorLocked |
| DoorLocked | **⟶** | Unlock  | **?** | PinIncorrect | **⟶** | DoorLocked |
| DoorLocked | **⟶** | Unlock  | **?** | PinCorrect   | **⟶** | DoorClosed |

<!-- PD_END -->

Guard labels are not strictly required as you can see in the Countdown example. But they can be useful to make the code and the state diagram more readable.

## The Classic Approach

Let's briefly recap how we would implement this using the classic approach.

### States and Message types

The PureScript types now include data in both states and messages:

<!-- PD_START:purs
filePath: test/Examples/Classic/DoorPin.purs
pick:
  - State
  - Msg
-->

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

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Classic/DoorPin.purs#L10-L27">test/Examples/Classic/DoorPin.purs L10-L27</a>
  </sup>
</p>

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

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Classic/DoorPin.purs#L29-L39">test/Examples/Classic/DoorPin.purs L29-L39</a>
  </sup>
</p>

<!-- PD_END -->

## The Transit Approach

Also in the **Transit** approach we define `State` and `Msg` types:

<!-- PD_START:purs
filePath: test/Examples/DoorPin.purs
pick:
  - State
  - Msg
-->

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

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs#L28-L39">test/Examples/DoorPin.purs L28-L39</a>
  </sup>
</p>

<!-- PD_END -->

In the DSL specification, we express conditional transitions by listing multiple possible target states:

<!-- PD_START:purs
filePath: test/Examples/DoorPin.purs
pick:
  - DoorPinTransit
-->

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

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs#L41-L50">test/Examples/DoorPin.purs L41-L50</a>
  </sup>
</p>

<!-- PD_END -->

The syntax `("PinCorrect" :? "DoorClosed") >| ("PinIncorrect" :? "DoorLocked")` indicates that the `Unlock` message from `DoorLocked` can transition to either state, depending on runtime conditions. The `:?` operator associates a condition label (like `"PinCorrect"`) with a target state, and `>|` chains multiple conditional outcomes together.

The update function now has access to both the current state and the message data, allowing you to implement the conditional logic:

<!-- PD_START:purs
filePath: test/Examples/DoorPin.purs
pick:
  - update
-->

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

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs#L52-L71">test/Examples/DoorPin.purs L52-L71</a>
  </sup>
</p>

<!-- PD_END -->

The match handlers receive both the current state and the message, giving you access to _only_ the data needed to make runtime decisions. The type system still ensures that only valid target states can be returned.

> **Important**: The order of match handlers in `mkUpdate` must match the order of transitions in the DSL specification.

> **Limitation**: The compiler cannot detect if an implementation forgets to return a possible case.
> For example, if a transition can return either `DoorClosed` or `DoorLocked`, your handler always returns `DoorClosed` then the compiler would not detect this error. Obviously the compiler cannot verify if your handler implements the conditional logic correctly, so missing a case is just one of many possible errors.

## Testing the update function

<!-- PD_START:purs
pick:
  - tag: signature_or_foreign
    name: assertWalk
    filePath: test/Examples/Common.purs
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

<!-- PD_START:purs
filePath: test/Examples/DoorPin.purs
pick:
  - assert1
-->

```purescript
assert1 :: Spec Unit
assert1 =
  it "should follow the walk and visit the expected intermediate states" do
    assertWalk update
      -- start in the open state
      (v @"DoorOpen")
      [
        -- close the door, then expect to transition to closed state
        v @"Close" ~> v @"DoorClosed"
      ,
        -- lock the door, then expect to transition to locked state with the given pin
        v @"Lock" { newPin: "1234" } ~> v @"DoorLocked" { storedPin: "1234" }
      ,
        -- unlock the door (with wrong pin), then expect to stay in locked state
        v @"Unlock" { enteredPin: "abcd" } ~> v @"DoorLocked" { storedPin: "1234" }
      ,
        -- unlock the door (with correct pin), then expect to transition to closed state
        v @"Unlock" { enteredPin: "1234" } ~> v @"DoorClosed"
      ,
        -- open the door, then expect to transition to open state
        v @"Open" ~> v @"DoorOpen"
      ]
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorPin.purs#L77-L98">test/Examples/DoorPin.purs L77-L98</a>
  </sup>
</p>

<!-- PD_END -->

## Conclusion

This example demonstrates how **Transit** extends beyond simple state machines to handle real-world complexity:

- **States and messages with data**: Both states and messages can carry data (like `activePin` in `DoorLocked` or `newPin` in `Lock`), and handlers receive this data.
- **Conditional transitions**: The DSL supports transitions with multiple possible outcomes using guard labels (`PinCorrect` and `PinIncorrect`). The type system ensures that conditional transitions can only return valid target states, and each outcome must be associated with its corresponding guard label.

By leveraging PureScript's `Variant` types to express subsets of possible states (which traditional ADTs cannot represent), **Transit** provides compile-time guarantees that your implementation matches your specification. The type system catches errors at compile time, ensuring that:

- You cannot return invalid target states
- You cannot return more cases than specified
- However, the compiler cannot detect if you forget to return a possible case (you can return fewer cases than specified)
