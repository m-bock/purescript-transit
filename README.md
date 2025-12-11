<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.png">
  <source media="(prefers-color-scheme: light)" srcset="assets/logo-light.png">
  <img alt="State diagram" src="assets/logo-light.png">
</picture>

Type-Safe State Machines.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Transit](#transit)
  - [Key Features](#key-features)
  - [About This Documentation](#about-this-documentation)
  - [Installation](#installation)
  - [Example1: Door](#example1-door)
    - [The Classic Approach](#the-classic-approach)
    - [The Transit Approach](#the-transit-approach)
    - [Compile-Time Safety](#compile-time-safety)
    - [[AI: find title]](#ai-find-title)
  - [Example2: Door with Lock](#example2-door-with-lock)
    - [The Classic Approach](#the-classic-approach-1)
    - [The Transit Approach](#the-transit-approach-1)
  - [Generate State Diagrams](#generate-state-diagrams)
  - [Generate Transition Tables](#generate-transition-tables)
  - [Example3: Door with Pin](#example3-door-with-pin)
    - [The Classic Approach](#the-classic-approach-2)
    - [The Transit Approach](#the-transit-approach-2)
  - [Example4: Door with Pin and Alarm](#example4-door-with-pin-and-alarm)
    - [The Classic Approach](#the-classic-approach-3)
    - [The Transit Approach](#the-transit-approach-3)
  - [Type signatures](#type-signatures)
  - [Variants](#variants)
  - [Monadic update functions](#monadic-update-functions)
  - [Example 6: Seven Bridges of Königsberg](#example-6-seven-bridges-of-k%C3%B6nigsberg)
    - [Graph Analysis](#graph-analysis)
  - [Example 7: This is the house of Santa Claus](#example-7-this-is-the-house-of-santa-claus)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

> [!NOTE]
> Useful information that users should know, even when skimming content.

> [!TIP]
> Helpful advice for doing things better or more easily.

> [!IMPORTANT]
> Key information users need to know to achieve their goal.

> [!WARNING]
> Urgent info that needs immediate user attention to avoid problems.

> [!CAUTION]
> Advises about risks or negative outcomes of certain actions.

# Transit

Transit is a PureScript library for building type-safe state machines. It provides a type-level DSL for specifying state transitions, ensuring that your state machine implementation is correct at compile time.

If you're familiar with [Servant](https://haskell-servant.readthedocs.io/) from Haskell, Transit follows a similar philosophy: just as Servant uses a REST API type-level specification to generate type-safe routing functions and OpenAPI documentation, Transit uses a state machine graph type-level specification to generate type-safe update functions and state diagrams.

## Key Features

- **Type-safe state transitions** - The compiler ensures all transitions are valid and complete
- **Automatic diagram generation** - Generate state diagrams and transition tables directly from your specification
- **Graph analysis** - Convert your state machine into a graph data structure for advanced analysis

## About This Documentation

All code examples in this documentation are extracted from actual, type-checked PureScript source files. At the top of each chapter, you'll find a link to the complete source file(s) used in that example.

## Installation

```bash
spago install transit
```

## Example1: Door

Full source code: _[test/Examples/Door.purs](test/Examples/Door.purs)_

Let's start with a simple door state machine. Here's its state diagram:

<img src="graphs/door.svg" />

This state machine has two states (`DoorOpen` and `DoorClosed`) and two messages (`Close` and `Open`). The initial state is `DoorOpen`, indicated by the grey arrow pointing to it.

Another way to represent this is a transition table:

<!-- PD_START:raw
filePath: graphs/door.html
--><table><caption>Door</caption><thead><tr><th>From State</th><th /><th>Message</th><th /><th>To State</th></tr></thead><tbody><tr><td>DoorOpen</td><td>⟶</td><td>Close</td><td>⟶</td><td>DoorClosed</td></tr><tr><td>DoorClosed</td><td>⟶</td><td>Open</td><td>⟶</td><td>DoorOpen</td></tr></tbody></table><!-- PD_END -->

In PureScript, we represent the states and messages with simple data types:

<!-- PD_START:purs
filePath: test/Examples/Door.purs
pick:
  - State
  - Msg
-->

```purescript
data State = DoorOpen | DoorClosed

data Msg = Close | Open
```

<p align="right"><sup>🗎 <a href="test/Examples/Door.purs#L26-L28">test/Examples/Door.purs</a></sup></p><!-- PD_END -->

### The Classic Approach

The traditional way to implement state transitions is to write an update function that takes a state and a message and returns a new state:

<!-- PD_START:purs
filePath: test/Examples/Door.purs
pick:
  - updateClassic
-->

```purescript
updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  _, _ -> state
```

<p align="right"><sup>🗎 <a href="test/Examples/Door.purs#L34-L38">test/Examples/Door.purs</a></sup></p><!-- PD_END -->

While this approach works, it has some drawbacks:

- The state diagram and implementation can easily get out of sync
- The compiler won't catch missing transitions or invalid state/message combinations
- You need to manually ensure all cases are handled correctly

### The Transit Approach

With the transit library, we take a different approach. First, we define a type-level specification of the state machine:

<!-- PD_START:purs
filePath: test/Examples/Door.purs
pick:
  - DoorDSL
-->

```purescript
type DoorDSL =
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
```

<p align="right"><sup>🗎 <a href="test/Examples/Door.purs#L50-L53">test/Examples/Door.purs</a></sup></p><!-- PD_END -->

This DSL syntax reads as: "From state `DoorOpen` on message `Close`, transition to state `DoorClosed`" and "From state `DoorClosed` on message `Open`, transition to state `DoorOpen`". The `Empty` starts the list, and `:*` adds each transition.

This type-level specification fully defines the state machine. Based on this spec, we can now create an update function that the compiler ensures only allows legal state transitions:

<!-- PD_START:purs
filePath: test/Examples/Door.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")
```

<p align="right"><sup>🗎 <a href="test/Examples/Door.purs#L55-L58">test/Examples/Door.purs</a></sup></p><!-- PD_END -->

Notice that the type signature is identical to the classic approach—`State -> Msg -> State`. The difference is that the compiler now enforces correctness at compile time.

### Compile-Time Safety

The type system ensures that your implementation matches the specification. The following will **not** compile:

- 🔴 Missing a match line for a state transition
- 🔴 Matching on illegal state/message combinations
- 🔴 Returning illegal states
- 🔴 Misspelled names of states and messages

Conversely, the compiler guarantees:

- 🟢 All matches are covered
- 🟢 Each match is on the correct state/message combination
- 🟢 Each match returns the correct state
- 🟢 All symbols (type-level strings) are spelled correctly

Later we'll see how to generate the state diagram directly from the spec, ensuring it always stays in sync with the code.

### [AI: find title]

As the equality of the type signatures of the classic and transit approaches update functions already suggest: Web can use them interchangeably. That means transit can be seamlessly integrated into existing codebases. Let's verify this.

We define an array of messages which we will apply to the state machine. We use the `foldl` function to apply the update functions consecutively until we reach a final state. We do this for both the classic and transit approaches. And then we check if the final states match our expectations.

<!-- PD_START:purs
filePath: test/Examples/Door.purs
pick:
  - checkSimpleWalk
-->

```purescript
checkSimpleWalk :: Aff Unit
checkSimpleWalk = do
  let
    initState = DoorOpen

    walk =
      [ Close, Open, Open, Close, Open, Close, Close ]

    finalStateA = foldl updateClassic initState walk
    finalStateB = foldl update initState walk

  finalStateA `shouldEqual` DoorClosed
  finalStateB `shouldEqual` DoorClosed
```

<p align="right"><sup>🗎 <a href="test/Examples/Door.purs#L67-L79">test/Examples/Door.purs</a></sup></p><!-- PD_END -->

This is not perfect yet. Because we only validate the final state. Instead we should check each intermediate state as well.
We'll use this helper function several times later. So let's also factor it out and make it work with any state and message types.

<!-- PD_START:purs
filePath: test/Examples/Common.purs
pick:
  - checkWalk
-->

```purescript
checkWalk
  :: forall msg state
   . Eq state
  => Show state
  => (state -> msg -> state)
  -> Walk msg state
  -> Aff Unit
checkWalk updateFn { initialState, steps } = do
  case Array.uncons steps of
    Just { head, tail } -> do
      let newState = updateFn initialState head.msg
      newState `shouldEqual` head.state
      checkWalk updateFn { initialState: newState, steps: tail }
    Nothing -> pure unit
```

<p align="right"><sup>🗎 <a href="test/Examples/Common.purs#L40-L53">test/Examples/Common.purs</a></sup></p><!-- PD_END -->

Now we can use the `checkWalk` function to test the state machine:

<!-- PD_START:purs
filePath: test/Examples/Door.purs
pick:
  - spec
-->

```purescript
spec :: Spec Unit
spec = do
  describe "Door" do
    it "should follow the simple walk" do
      checkSimpleWalk

    it "should follow the  walk" do
      let
        walk =
          { initialState: DoorOpen
          , steps:
              [ { msg: Close, state: DoorClosed }
              , { msg: Open, state: DoorOpen }
              , { msg: Open, state: DoorOpen }
              , { msg: Close, state: DoorClosed }
              , { msg: Open, state: DoorClosed }
              , { msg: Close, state: DoorClosed }
              , { msg: Close, state: DoorClosed }
              ]
          }

      checkWalk updateClassic walk
      checkWalk update walk
```

<p align="right"><sup>🗎 <a href="test/Examples/Door.purs#L81-L103">test/Examples/Door.purs</a></sup></p><!-- PD_END -->

## Example2: Door with Lock

Full source code: _[test/Examples/DoorWithLock.purs](test/Examples/DoorWithLock.purs)_

Now let's extend our door example by adding a lock mechanism. Here's the enhanced state diagram:

<img src="graphs/door-with-lock.svg" />

This state machine extends the simple door with a third state (`DoorLocked`) and two additional messages (`Lock` and `Unlock`). Notice that you can only lock the door when it's closed, and unlocking returns you to the closed state (not open). This is a common pattern in real-world state machines where certain operations are only valid in specific states.

The transition table shows all valid transitions:

<!-- PD_START:raw
filePath: graphs/door-with-lock.html
--><table><caption>Door with Lock</caption><thead><tr><th>From State</th><th /><th>Message</th><th /><th>To State</th></tr></thead><tbody><tr><td>DoorOpen</td><td>⟶</td><td>Close</td><td>⟶</td><td>DoorClosed</td></tr><tr><td>DoorClosed</td><td>⟶</td><td>Open</td><td>⟶</td><td>DoorOpen</td></tr><tr><td>DoorClosed</td><td>⟶</td><td>Lock</td><td>⟶</td><td>DoorLocked</td></tr><tr><td>DoorLocked</td><td>⟶</td><td>Unlock</td><td>⟶</td><td>DoorClosed</td></tr></tbody></table><!-- PD_END -->

The PureScript types extend the previous example:

<!-- PD_START:purs
filePath: test/Examples/DoorWithLock.purs
pick:
  - State
  - Msg
-->

```purescript
data State
  = DoorOpen
  | DoorClosed
  | DoorLocked

data Msg
  = Close
  | Open
  | Lock
  | Unlock
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithLock.purs#L27-L36">test/Examples/DoorWithLock.purs</a></sup></p><!-- PD_END -->

### The Classic Approach

The classic update function now handles more cases:

<!-- PD_START:purs
filePath: test/Examples/DoorWithLock.purs
pick:
  - updateClassic
-->

```purescript
updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  DoorClosed, Lock -> DoorLocked
  DoorLocked, Unlock -> DoorClosed
  _, _ -> state
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithLock.purs#L42-L48">test/Examples/DoorWithLock.purs</a></sup></p><!-- PD_END -->

As the state machine grows, the classic approach becomes more error-prone. You need to remember:

- Which messages are valid in which states
- What the next state should be for each transition
- To handle all edge cases (like trying to lock an open door)

### The Transit Approach

With transit, we extend the DSL specification to include the new transitions:

<!-- PD_START:purs
filePath: test/Examples/DoorWithLock.purs
pick:
  - DoorDSL
-->

```purescript
type DoorDSL =
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :* ("DoorLocked" :@ "Unlock" >| "DoorClosed")
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithLock.purs#L54-L59">test/Examples/DoorWithLock.purs</a></sup></p><!-- PD_END -->

The update function now includes all four transitions, and the compiler ensures each one is correctly implemented:

<!-- PD_START:purs
filePath: test/Examples/DoorWithLock.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")
  (match @"DoorClosed" @"Lock" \_ _ -> return @"DoorLocked")
  (match @"DoorLocked" @"Unlock" \_ _ -> return @"DoorClosed")
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithLock.purs#L61-L66">test/Examples/DoorWithLock.purs</a></sup></p><!-- PD_END -->

The type system prevents common mistakes:

- 🔴 Trying to match `DoorOpen` with `Lock` (invalid transition)
- 🔴 Returning `DoorOpen` from the `Unlock` handler (wrong target state)
- 🔴 Forgetting to handle the `Lock` transition

This becomes even more valuable as state machines grow in complexity.

## Generate State Diagrams

Full source code: _[test/Examples/GenerateStateDiagrams.purs](test/Examples/GenerateStateDiagrams.purs)_

One of the key benefits of transit is that you can generate state diagrams directly from your type-level specification. This ensures your diagrams always stay in sync with your code—no manual updates required.

To generate a state diagram, you use `reflectType` to convert your type-level DSL specification to a term-level equivalent, then write it to a Graphviz `.dot` file:

<!-- PD_START:purs
filePath: test/Examples/GenerateStateDiagrams.purs
pick:
  - main
-->

```purescript
main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @DoorDSL)

  TransitGraphviz.writeToFile "graphs/door.dot" transit _
    { title = "Door" }
```

<p align="right"><sup>🗎 <a href="test/Examples/GenerateStateDiagrams.purs#L11-L17">test/Examples/GenerateStateDiagrams.purs</a></sup></p><!-- PD_END -->

The process works in two steps:

1. `reflectType` converts your type-level DSL specification to a term-level equivalent
2. `TransitGraphviz.writeToFile` uses that to render a Graphviz `.dot` file

To convert the `.dot` file to an SVG (or other formats), use the Graphviz command-line tools:

```bash
dot -Tsvg graphs/door.dot -o graphs/door.svg
```

Or for PNG:

```bash
dot -Tpng graphs/door.dot -o graphs/door.png
```

Since the diagram is generated from the same DSL specification used to create the type-safe update function, any changes to your state machine are automatically reflected in both the code and the diagram. This eliminates the common problem of documentation getting out of sync with implementation.

## Generate Transition Tables

Full source code: _[test/Examples/GenerateTransitionTables.purs](test/Examples/GenerateTransitionTables.purs)_

In addition to state diagrams, you can also generate transition tables from the same graph data structure. This provides a tabular view of all state transitions, which can be easier to read for some use cases.

The process is identical to generating state diagrams—you use `reflectType` to convert your DSL specification, but then use `TransitTable.writeToFile` instead:

<!-- PD_START:purs
filePath: test/Examples/GenerateTransitionTables.purs
pick:
  - main
-->

```purescript
main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @DoorDSL)

  TransitTable.writeToFile "graphs/door.html" transit _
    { title = "Door" }
```

<p align="right"><sup>🗎 <a href="test/Examples/GenerateTransitionTables.purs#L11-L17">test/Examples/GenerateTransitionTables.purs</a></sup></p><!-- PD_END -->

<p align="right">

This generates an HTML file containing a table with columns for "From State", "Message", and "To State". The table can be embedded directly in documentation (as shown in the examples above) or viewed in a browser.

Since both the state diagram and transition table are generated from the same DSL specification, they're guaranteed to be consistent with each other and with your type-level specification.

## Example3: Door with Pin

Full source code: _[test/Examples/DoorWithPin.purs](test/Examples/DoorWithPin.purs)_

Now let's add a PIN code to our door lock. This introduces two important concepts: **states with data** and **conditional transitions**.

<img src="graphs/door-with-pin.svg" />

In this example, the `DoorLocked` state stores a PIN code, and the `Unlock` message includes the entered PIN. The unlock operation can succeed (transitioning to `DoorClosed`) or fail (staying in `DoorLocked`), depending on whether the entered PIN matches the stored one.

Notice the diamond node in the state diagram—this represents a conditional transition where the outcome depends on runtime data.

The transition table shows both possible outcomes:

<!-- PD_START:raw
filePath: graphs/door-with-pin.html
--><table><caption>Door with Pin</caption><thead><tr><th>From State</th><th /><th>Message</th><th /><th>To State</th></tr></thead><tbody><tr><td>DoorOpen</td><td>⟶</td><td>Close</td><td>⟶</td><td>DoorClosed</td></tr><tr><td>DoorClosed</td><td>⟶</td><td>Open</td><td>⟶</td><td>DoorOpen</td></tr><tr><td>DoorClosed</td><td>⟶</td><td>Lock</td><td>⟶</td><td>DoorLocked</td></tr><tr><td>DoorLocked</td><td>⟶</td><td>Unlock</td><td>⟶</td><td>DoorLocked</td></tr><tr><td>DoorLocked</td><td>⟶</td><td>Unlock</td><td>⟶</td><td>DoorClosed</td></tr></tbody></table><!-- PD_END -->

The PureScript types now include data in both states and messages:

<!-- PD_START:purs
filePath: test/Examples/DoorWithPin.purs
pick:
  - State
  - Msg
-->

```purescript
data State
  = DoorOpen
  | DoorClosed
  | DoorLocked { pin :: String }

data Msg
  = Close
  | Open
  | Lock { newPin :: String }
  | Unlock { enteredPin :: String }
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L21-L30">test/Examples/DoorWithPin.purs</a></sup></p><!-- PD_END -->

### The Classic Approach

The classic update function now needs to handle state and message data:

<!-- PD_START:purs
filePath: test/Examples/DoorWithPin.purs
pick:
  - updateClassic
-->

```purescript
updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  DoorClosed, Lock { newPin } -> DoorLocked { pin: newPin }
  DoorLocked { pin }, Unlock { enteredPin } ->
    if pin == enteredPin then
      DoorClosed
    else
      DoorLocked { pin }
  _, _ -> state
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L36-L46">test/Examples/DoorWithPin.purs</a></sup></p><!-- PD_END -->

<p align="right">
  <sup>🗎 <a href="test/Examples/DoorWithPin.purs">Examples/DoorWithPin.purs</a></sup>
</p>

### The Transit Approach

In the DSL specification, we express conditional transitions by listing multiple possible target states:

<!-- PD_START:purs
filePath: test/Examples/DoorWithPin.purs
pick:
  - DoorDSL
-->

```purescript
type DoorDSL =
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :*
      ( "DoorLocked" :@ "Unlock"
          >| "DoorClosed"
          >| "DoorLocked"
      )
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L52-L61">test/Examples/DoorWithPin.purs</a></sup></p><!-- PD_END -->

The syntax `>| "DoorClosed" >| "DoorLocked"` indicates that the `Unlock` message from `DoorLocked` can transition to either state, depending on runtime conditions.

The update function now has access to both the current state and the message data, allowing you to implement the conditional logic:

<!-- PD_START:purs
filePath: test/Examples/DoorWithPin.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
  ( match @"DoorClosed" @"Lock" \_ msg ->
      return @"DoorLocked" { pin: msg.newPin }
  )
  ( match @"DoorLocked" @"Unlock" \state msg ->
      if state.pin == msg.enteredPin then
        return @"DoorClosed"
      else
        return @"DoorLocked" { pin: state.pin }
  )
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L63-L79">test/Examples/DoorWithPin.purs</a></sup></p><!-- PD_END -->

The match handlers receive both the current state and the message, giving you access to all the data needed to make runtime decisions. The type system still ensures that:

- 🔴 You can only return states that are valid targets for that transition
- 🔴 You handle all required transitions
- 🟢 The conditional logic is type-safe

## Example4: Door with Pin and Alarm

Full source code: _[test/Examples/DoorWithAlarm.purs](test/Examples/DoorWithAlarm.purs)_

Now let's extend the door with PIN by adding an alarm system that triggers after too many failed unlock attempts. This introduces **labeled conditional transitions**, which allow you to document the different conditions that lead to different states.

<img src="graphs/door-with-alarm.svg" />

In this example, the `DoorLocked` state now tracks the number of failed unlock attempts. When unlocking:

- If the PIN is correct, the door transitions to `DoorClosed`
- If the PIN is incorrect but attempts < 3, it stays in `DoorLocked` with an incremented attempt counter
- If the PIN is incorrect and attempts >= 3, it transitions to `Alarm` state

The transition table shows all possible outcomes:

<!-- PD_START:raw
filePath: graphs/door-with-alarm.html
--><table><caption>Door with Alarm</caption><thead><tr><th>From State</th><th /><th>Message</th><th /><th>To State</th></tr></thead><tbody><tr><td>DoorOpen</td><td>⟶</td><td>Close</td><td>⟶</td><td>DoorClosed</td></tr><tr><td>DoorClosed</td><td>⟶</td><td>Open</td><td>⟶</td><td>DoorOpen</td></tr><tr><td>DoorClosed</td><td>⟶</td><td>Lock</td><td>⟶</td><td>DoorLocked</td></tr><tr><td>DoorLocked</td><td>⟶</td><td>Unlock ? TooManyAttempts</td><td>⟶</td><td>Alarm</td></tr><tr><td>DoorLocked</td><td>⟶</td><td>Unlock ? PinIncorrect</td><td>⟶</td><td>DoorLocked</td></tr><tr><td>DoorLocked</td><td>⟶</td><td>Unlock ? PinCorrect</td><td>⟶</td><td>DoorClosed</td></tr></tbody></table><!-- PD_END -->

The PureScript types extend the previous example with an alarm state and attempt tracking:

<!-- PD_START:purs
filePath: test/Examples/DoorWithAlarm.purs
pick:
  - State
  - Msg
-->

```purescript
data State
  = DoorOpen
  | DoorClosed
  | DoorLocked { pin :: String, attempts :: Int }
  | Alarm

data Msg
  = Close
  | Open
  | Lock { newPin :: String }
  | Unlock { enteredPin :: String }
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithAlarm.purs#L24-L34">test/Examples/DoorWithAlarm.purs</a></sup></p><!-- PD_END -->

### The Classic Approach

The classic update function now handles the attempt counter and alarm condition:

<!-- PD_START:purs
filePath: test/Examples/DoorWithAlarm.purs
pick:
  - updateClassic
-->

```purescript
updateClassic :: State -> Msg -> State
updateClassic state msg = case state, msg of
  DoorOpen, Close -> DoorClosed
  DoorClosed, Open -> DoorOpen
  DoorClosed, Lock { newPin } -> DoorLocked { pin: newPin, attempts: 0 }
  DoorLocked { pin, attempts }, Unlock { enteredPin } ->
    let
      pinCorrect = pin == enteredPin
      attemptsExceeded = attempts >= 3
    in
      case pinCorrect, attemptsExceeded of
        true, _ -> DoorClosed
        false, true -> DoorLocked { pin, attempts: attempts + 1 }
        false, false -> Alarm
  _, _ -> state
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithAlarm.purs#L40-L54">test/Examples/DoorWithAlarm.purs</a></sup></p><!-- PD_END -->

### The Transit Approach

With transit, we use **labeled conditional transitions** to document the different conditions. The `:?` operator allows you to label each possible outcome:

<!-- PD_START:purs
filePath: test/Examples/DoorWithAlarm.purs
pick:
  - DoorDSL
-->

```purescript
type DoorDSL =
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :*
      ( "DoorLocked" :@ "Unlock"
          >| ("PinCorrect" :? "DoorClosed")
          >| ("PinIncorrect" :? "DoorLocked")
          >| ("TooManyAttempts" :? "Alarm")
      )
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithAlarm.purs#L60-L70">test/Examples/DoorWithAlarm.purs</a></sup></p><!-- PD_END -->

The syntax `("PinCorrect" :? "DoorClosed")` labels the transition path, making it clear in the specification what condition leads to which state. This is especially useful when you have multiple conditional transitions from the same state/message pair.

The update function uses `returnVia` instead of `return` to specify which labeled path to take:

<!-- PD_START:purs
filePath: test/Examples/DoorWithAlarm.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
  ( match @"DoorClosed" @"Lock" \_ msg -> return @"DoorLocked"
      { pin: msg.newPin
      , attempts: 0
      }
  )
  ( match @"DoorLocked" @"Unlock" \state msg ->
      let
        pinCorrect = state.pin == msg.enteredPin
        attemptsExceeded = state.attempts >= 3
      in
        case pinCorrect, attemptsExceeded of
          true, _ -> returnVia @"PinCorrect" @"DoorClosed"
          false, true -> returnVia @"PinIncorrect" @"DoorLocked"
            { pin: state.pin
            , attempts: state.attempts + 1
            }
          false, false -> returnVia @"TooManyAttempts" @"Alarm"

  )
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithAlarm.purs#L72-L98">test/Examples/DoorWithAlarm.purs</a></sup></p><!-- PD_END -->

The `returnVia` function takes a label (like `@"PinCorrect"`) and a target state. The type system ensures that:

- 🔴 You can only use labels that are defined in the DSL specification
- 🔴 Each label must map to the correct target state
- 🟢 The labels make the code self-documenting—it's immediately clear which condition leads to which state

Labeled transitions are particularly valuable when you have complex conditional logic with multiple possible outcomes, as they provide both type safety and clear documentation of the state machine's behavior.

## Type signatures

Full source code: _[test/Examples/Signatures.purs](test/Examples/Signatures.purs)_

This chapter demonstrates the type signatures that transit enforces for your update functions. To show these signatures without implementing the actual logic, we use an `unimplemented` helper function that satisfies the type checker:

<!-- PD_START:purs
filePath: test/Examples/Signatures.purs
pick:
  - unimplemented
-->

```purescript
unimplemented :: forall a. a
unimplemented = unsafeCoerce "not yet implemented"
```

<p align="right"><sup>🗎 <a href="test/Examples/Signatures.purs#L15-L16">test/Examples/Signatures.purs</a></sup></p><!-- PD_END -->

The `update` function demonstrates the type signatures that transit enforces. The straightforward part is the `State` and `Msg` types—each match handler receives the exact state and message types for that transition. However, the return type is more complex: depending on the specification, a transition may allow multiple possible target states, so we need to return a subset of the state type.

Unfortunately, PureScript's ADTs (Algebraic Data Types) don't allow expressing a subset of cases from a union type. This is where `Variant` comes in—it's perfect for representing a subset of cases from a union type. Each match handler must return a `Variant` type that precisely matches the possible target states defined in the DSL specification.

This approach requires internal conversion between ADT and `Variant` representations. If you'd like to avoid this conversion overhead, you can define your `State` and `Msg` types as `Variant` directly from the start, as shown in the next chapter.

<!-- PD_START:purs
filePath: test/Examples/Signatures.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
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
            :: Variant ("DoorLocked" :: ReturnState { attempts :: Int, pin :: String })
      )
  )
  ( match @"DoorLocked" @"Unlock"
      ( \(state :: { attempts :: Int, pin :: String }) (msg :: { enteredPin :: String }) ->
          unimplemented
            :: Variant
                 ( "Alarm" :: ReturnStateVia "TooManyAttempts" Unit
                 , "DoorClosed" :: ReturnStateVia "PinCorrect" Unit
                 , "DoorLocked" :: ReturnStateVia "PinIncorrect" { attempts :: Int, pin :: String }
                 )
      )
  )
```

<p align="right"><sup>🗎 <a href="test/Examples/Signatures.purs#L18-L47">test/Examples/Signatures.purs</a></sup></p><!-- PD_END -->

## Variants

Full source code: _[test/Examples/Variants.purs](test/Examples/Variants.purs)_

Instead of using ADTs for `State` and `Msg`, you can define them directly as `Variant` types. This eliminates the conversion overhead between ADT and `Variant` representations, and you use `mkUpdate` instead of `mkUpdateGeneric`:

<!-- PD_START:purs
filePath: test/Examples/Variants.purs
pick:
  - State
  - Msg
  - update
-->

```purescript
type State = Variant
  ( "DoorOpen" :: Unit
  , "DoorClosed" :: Unit
  , "DoorLocked" :: { pin :: String }
  )

type Msg = Variant
  ( "Close" :: Unit
  , "Open" :: Unit
  , "Lock" :: { newPin :: String }
  , "Unlock" :: { enteredPin :: String }
  )

update :: State -> Msg -> State
update = mkUpdate @DoorDSL
  ( match @"DoorOpen" @"Close" \_ _ ->
      return @"DoorClosed"
  )
  ( match @"DoorClosed" @"Open" \_ _ ->
      return @"DoorOpen"
  )
  ( match @"DoorClosed" @"Lock" \_ msg ->
      return @"DoorLocked" { pin: msg.newPin }
  )
  ( match @"DoorLocked" @"Unlock" \state msg ->
      if state.pin == msg.enteredPin then
        return @"DoorClosed"
      else
        return @"DoorLocked" { pin: state.pin }
  )
```

<p align="right"><sup>🗎 <a href="test/Examples/Variants.purs#L9-L38">test/Examples/Variants.purs</a></sup></p><!-- PD_END -->

## Monadic update functions

Full source code: _[test/Examples/Monadic.purs](test/Examples/Monadic.purs)_

So far, all our examples have used pure update functions with the type signature `State -> Msg -> State`. However, sometimes you need to perform side effects during state transitions—such as logging, making HTTP requests, or interacting with external systems.

For these cases, transit provides `mkUpdateGenericM`, which creates update functions that operate in a monadic context. The type signature becomes `State -> Msg -> m State`, where `m` is any `Monad` (commonly `Effect`, `Aff`, or `ReaderT`).

The key differences from pure update functions are:

1. **Use `mkUpdateGenericM` instead of `mkUpdateGeneric`** - This tells transit you want a monadic update function
2. **Use `matchM` instead of `match`** - This allows your handlers to return values in the monadic context
3. **Type signature includes the monad** - Instead of `State -> Msg -> State`, you get `State -> Msg -> m State`

Here's an example that adds logging to state transitions:

<!-- PD_START:purs
filePath: test/Examples/Monadic.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> Effect State
update = mkUpdateGenericM @DoorDSL
  ( matchM @"DoorOpen" @"Close" \_ _ -> do
      Console.log "You just closed the door"
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ -> do
      Console.log "You just opened the door"
      pure $ return @"DoorOpen"
  )
  ( matchM @"DoorClosed" @"Lock" \_ _ -> do
      Console.log "You just locked the door"
      pure $ return @"DoorLocked"
  )
  ( matchM @"DoorLocked" @"Unlock" \_ _ -> do
      Console.log "You just unlocked the door"
      pure $ return @"DoorClosed"
  )
```

<p align="right"><sup>🗎 <a href="test/Examples/Monadic.purs#L10-L27">test/Examples/Monadic.purs</a></sup></p><!-- PD_END -->

Each handler can now perform side effects (like logging) before returning the new state. The `return` function still works the same way—you wrap your state value with it, and then wrap that in `pure` to lift it into the monadic context.

## Example 6: Seven Bridges of Königsberg

Full source code: _[test/Examples/BridgesKoenigsberg.purs](test/Examples/BridgesKoenigsberg.purs)_

So far, we've seen how transit helps you build type-safe state machines and generate state diagrams and transition tables. But the power of transit extends far beyond documentation generation. The reflected data structure—the term-level representation of your type-level DSL specification—can be converted into a general-purpose graph data structure, enabling sophisticated graph analysis.

This example demonstrates this capability using the famous [Seven Bridges of Königsberg](https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg) problem. In 1736, the mathematician Leonhard Euler was asked whether it was possible to walk through the city of Königsberg (now Kaliningrad) crossing each of its seven bridges exactly once and returning to the starting point. Euler's solution to this problem laid the foundation for graph theory.

The problem can be modeled as a graph where:

- **Nodes** represent the four land areas (A, B, C, and D)
- **Edges** represent the seven bridges connecting them

<img src="assets/bridges-koenigsberg.jpg" />

While transit is designed for directed state machines, we can model an undirected graph by defining bidirectional transitions for each bridge. The renderer can then summarize these complementary edges into a single undirected edge for visualization. Notice how each bridge has two transitions—one in each direction:

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - State
  - Msg
-->

```purescript
data State = LandA | LandB | LandC | LandD

data Msg
  = Cross_a
  | Cross_b
  | Cross_c
  | Cross_d
  | Cross_e
  | Cross_f
  | Cross_g
```

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L28-L37">test/Examples/BridgesKoenigsberg.purs</a></sup></p><!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - BridgesTransitions
-->

```purescript
type BridgesTransitions =
  Transit $ Empty
    :* ("LandA" :@ "Cross_a" >| "LandB")
    :* ("LandB" :@ "Cross_a" >| "LandA")

    :* ("LandA" :@ "Cross_b" >| "LandB")
    :* ("LandB" :@ "Cross_b" >| "LandA")

    :* ("LandA" :@ "Cross_c" >| "LandC")
    :* ("LandC" :@ "Cross_c" >| "LandA")

    :* ("LandA" :@ "Cross_d" >| "LandC")
    :* ("LandC" :@ "Cross_d" >| "LandA")

    :* ("LandA" :@ "Cross_e" >| "LandD")
    :* ("LandD" :@ "Cross_e" >| "LandA")

    :* ("LandB" :@ "Cross_f" >| "LandD")
    :* ("LandD" :@ "Cross_f" >| "LandB")

    :* ("LandC" :@ "Cross_g" >| "LandD")
    :* ("LandD" :@ "Cross_g" >| "LandC")
```

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L69-L90">test/Examples/BridgesKoenigsberg.purs</a></sup></p><!-- PD_END -->

<!-- PD_START:raw
filePath: graphs/bridges-koenigsberg.html
--><table><caption>Untitled</caption><thead><tr><th>From State</th><th /><th>Message</th><th /><th>To State</th></tr></thead><tbody><tr><td>LandB</td><td>⟵</td><td>Cross_a</td><td>⟶</td><td>LandA</td></tr><tr><td>LandB</td><td>⟵</td><td>Cross_b</td><td>⟶</td><td>LandA</td></tr><tr><td>LandC</td><td>⟵</td><td>Cross_c</td><td>⟶</td><td>LandA</td></tr><tr><td>LandC</td><td>⟵</td><td>Cross_d</td><td>⟶</td><td>LandA</td></tr><tr><td>LandD</td><td>⟵</td><td>Cross_e</td><td>⟶</td><td>LandA</td></tr><tr><td>LandD</td><td>⟵</td><td>Cross_f</td><td>⟶</td><td>LandB</td></tr><tr><td>LandD</td><td>⟵</td><td>Cross_g</td><td>⟶</td><td>LandC</td></tr></tbody></table><!-- PD_END -->

The transition table shows the undirected nature of the graph—each bridge can be crossed in both directions. When generating the visualization, the renderer summarizes these bidirectional edges into a single undirected edge:

<img src="graphs/bridges-koenigsberg.svg" />

### Graph Analysis

The real power of transit becomes apparent when we convert the reflected data structure into a general-purpose graph. Using `mkStateGraph`, we transform the transit specification into a `StateGraph`—a specialized `Graph` type configured with edge and node labels suitable for state machine analysis.

Once we have this graph data structure, we can perform sophisticated analysis using standard graph algorithms. For the Seven Bridges problem, we want to determine if the graph has an **Eulerian circuit** (a path that visits every edge exactly once and returns to the starting point) or an **Eulerian trail** (a path that visits every edge exactly once but doesn't necessarily return to the start).

Euler's theorem states that:

- An undirected graph has an Eulerian circuit if and only if it is connected and has zero vertices of odd degree
- An undirected graph has an Eulerian trail if and only if it is connected and has exactly zero or two vertices of odd degree

We can check these conditions using helper functions from the `Test.Examples.Common` module:

<!-- PD_START:purs
filePath: test/Examples/Common.purs
pick:
  - hasEulerCircle
  - hasEulerTrail
  - countOddOutgoingEdges
-->

```purescript
hasEulerCircle :: forall e n. Ord n => Ord e => Graph e n -> Boolean
hasEulerCircle g = true
  && Graph.isUndirected g
  && countOddOutgoingEdges g == 0

hasEulerTrail :: forall e n. Ord n => Ord e => Graph e n -> Boolean
hasEulerTrail g = true
  && Graph.isUndirected g
  && (countOddOutgoingEdges g == 0 || countOddOutgoingEdges g == 2)

countOddOutgoingEdges :: forall e n. Ord n => Ord e => Graph e n -> Int
countOddOutgoingEdges g =
  let
    nodes = Graph.getNodes g
  in
    Array.length $ Array.filter
      (\node -> Int.odd $ Set.size (Graph.getOutgoingEdges node g))
      (Set.toUnfoldable nodes)
```

<p align="right"><sup>🗎 <a href="test/Examples/Common.purs#L14-L31">test/Examples/Common.purs</a></sup></p><!-- PD_END -->

To perform the analysis, we convert the reflected transit specification into a graph and then check its properties:

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - spec
  - main
-->

```purescript
spec :: Spec Unit
spec = do
  describe "Dead ends" do
    it "should be empty" do
      let transit = reflectType (Proxy @BridgesTransitions)
      let graph = mkStateGraph transit
      Set.size (Graph.getOutgoingEdges "LandA" graph) `shouldEqual` 5
      Set.size (Graph.getOutgoingEdges "LandB" graph) `shouldEqual` 3
      Set.size (Graph.getOutgoingEdges "LandC" graph) `shouldEqual` 3
      Set.size (Graph.getOutgoingEdges "LandD" graph) `shouldEqual` 3
      hasEulerCircle graph `shouldEqual` false
      hasEulerTrail graph `shouldEqual` false

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @BridgesTransitions)

  TransitGraphviz.writeToFile "graphs/bridges-koenigsberg.dot" transit _
    { useUndirectedEdges = true }

  TransitTable.writeToFile "graphs/bridges-koenigsberg.html" transit _
    { useUndirectedEdges = true }

  runSpecAndExitProcess [ consoleReporter ] spec
```

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L119-L147">test/Examples/BridgesKoenigsberg.purs</a></sup></p><!-- PD_END -->

The key steps are:

1. **Reflect the type-level specification**: `reflectType (Proxy @BridgesTransitions)` converts the type-level DSL to a term-level representation
2. **Convert to a graph**: `mkStateGraph transit` transforms the transit specification into a `StateGraph`—a general-purpose graph data structure
3. **Perform analysis**: Use graph analysis functions like `hasEulerCircle` and `hasEulerTrail` to check properties

These functions check whether the graph is undirected and count how many vertices have an odd number of outgoing edges. For the Seven Bridges of Königsberg:

- **LandA** has 5 bridges (odd)
- **LandB** has 3 bridges (odd)
- **LandC** has 3 bridges (odd)
- **LandD** has 3 bridges (odd)

Since all four vertices have an odd degree, the graph has **4 vertices with odd degree**. According to Euler's theorem, this means:

- ❌ The graph does **not** have an Eulerian circuit (would require 0 odd-degree vertices)
- ❌ The graph does **not** have an Eulerian trail (would require 0 or 2 odd-degree vertices)

This confirms Euler's original conclusion: it's impossible to walk through Königsberg crossing each bridge exactly once.

This example demonstrates that transit's value extends far beyond state machine documentation. By reflecting the type-level specification to a term-level graph data structure, you gain access to a rich ecosystem of graph algorithms and analysis tools. The same DSL that ensures compile-time correctness for your state transitions can also power runtime graph analysis, pathfinding, cycle detection, and more.

In the next example, we'll see a graph that **does** have an Eulerian trail, demonstrating how transit can help verify and understand graph properties beyond simple state machines.

## Example 7: This is the house of Santa Claus

Full source code: _[test/Examples/HouseOfSantaClaus.purs](test/Examples/HouseOfSantaClaus.purs)_

[AI: in german, this is the house of Santa Claus, more common, thus explain]

<img src="assets/das-haus-vom-nikolaus-solution.webp" />

<table>
  <tr>
    <th>Syllable</th>
    <th>German</th>
    <th>English</th>
  </tr>
  <tr><td>1</td><td>das</td> <td>This</td></tr>
  <tr><td>2</td><td>ist</td> <td>is</td></tr>
  <tr><td>3</td><td>das</td> <td>the</td></tr>
  <tr><td>4</td><td>Haus</td> <td>house</td></tr>
  <tr><td>5</td><td>vom</td> <td>of</td></tr>
  <tr><td>6</td><td>Ni-</td> <td>San-</td></tr>
  <tr><td>7</td><td>ko-</td> <td>ta</td></tr>
  <tr><td>8</td><td>laus</td> <td>Claus</td></tr>
</table>

<!-- PD_START:purs
filePath: test/Examples/HouseOfSantaClaus.purs
pick:
  - TransitSantaClaus
-->

```purescript
type TransitSantaClaus =
  Transit $ Empty
    :* ("N_1" :@ "E_a" >| "N_2")
    :* ("N_2" :@ "E_a" >| "N_1")

    :* ("N_2" :@ "E_b" >| "N_3")
    :* ("N_3" :@ "E_b" >| "N_2")

    :* ("N_3" :@ "E_c" >| "N_5")
    :* ("N_5" :@ "E_c" >| "N_3")

    :* ("N_5" :@ "E_d" >| "N_4")
    :* ("N_4" :@ "E_d" >| "N_5")

    :* ("N_4" :@ "E_e" >| "N_1")
    :* ("N_1" :@ "E_e" >| "N_4")

    :* ("N_1" :@ "E_f" >| "N_3")
    :* ("N_3" :@ "E_f" >| "N_1")

    :* ("N_2" :@ "E_g" >| "N_4")
    :* ("N_4" :@ "E_g" >| "N_2")

    :* ("N_3" :@ "E_h" >| "N_4")
    :* ("N_4" :@ "E_h" >| "N_3")
```

<p align="right"><sup>🗎 <a href="test/Examples/HouseOfSantaClaus.purs#L75-L99">test/Examples/HouseOfSantaClaus.purs</a></sup></p><!-- PD_END -->

<!-- PD_START:raw
filePath: graphs/house-of-santa-claus.html
--><table><caption>Untitled</caption><thead><tr><th>From State</th><th /><th>Message</th><th /><th>To State</th></tr></thead><tbody><tr><td>N_2</td><td>⟵</td><td>E_a</td><td>⟶</td><td>N_1</td></tr><tr><td>N_3</td><td>⟵</td><td>E_b</td><td>⟶</td><td>N_2</td></tr><tr><td>N_5</td><td>⟵</td><td>E_c</td><td>⟶</td><td>N_3</td></tr><tr><td>N_5</td><td>⟵</td><td>E_d</td><td>⟶</td><td>N_4</td></tr><tr><td>N_4</td><td>⟵</td><td>E_e</td><td>⟶</td><td>N_1</td></tr><tr><td>N_3</td><td>⟵</td><td>E_f</td><td>⟶</td><td>N_1</td></tr><tr><td>N_4</td><td>⟵</td><td>E_g</td><td>⟶</td><td>N_2</td></tr><tr><td>N_4</td><td>⟵</td><td>E_h</td><td>⟶</td><td>N_3</td></tr></tbody></table><!-- PD_END -->

<img src="graphs/house-of-santa-claus.svg"  />
