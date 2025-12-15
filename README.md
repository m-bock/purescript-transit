<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="assets/logo-light.svg">
  <img alt="Transit logo" src="assets/logo-light.svg">
</picture>

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Transit - Type-Safe State Machines](#transit---type-safe-state-machines)
  - [Introduction](#introduction)
    - [Key Features](#key-features)
    - [About This Documentation](#about-this-documentation)
    - [Installation](#installation)
  - [Example 1: A Simple Door](#example-1-a-simple-door)
    - [The State Machine](#the-state-machine)
    - [States and Messages](#states-and-messages)
    - [State updates: The Classic Approach](#state-updates-the-classic-approach)
    - [State updates: The Transit Approach](#state-updates-the-transit-approach)
    - [Writing Tests for the update function](#writing-tests-for-the-update-function)
    - [Generating Diagrams and Tables](#generating-diagrams-and-tables)
    - [Conclusion](#conclusion)
  - [Example 2: Door with Pin](#example-2-door-with-pin)
    - [State updates: The Classic Approach](#state-updates-the-classic-approach-1)
    - [State updates: The Transit Approach](#state-updates-the-transit-approach-1)
    - [Type signatures](#type-signatures)
    - [Variants](#variants)
  - [Example 3: Seven Bridges of Königsberg](#example-3-seven-bridges-of-k%C3%B6nigsberg)
    - [Graph Analysis](#graph-analysis)
  - [Example 4: The house of Santa Claus](#example-4-the-house-of-santa-claus)
  - [More](#more)
    - [Monadic update functions](#monadic-update-functions)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Transit - Type-Safe State Machines

**Transit** is a PureScript library for building type-safe state machines. It provides a type-level DSL for specifying state transitions. You define your state machine once using a type-level specification, and the compiler ensures your implementation matches that specification—eliminating bugs from invalid transitions, missing cases, or documentation drift.

## Introduction

### Key Features

- **Type-safe state transitions** - The compiler ensures all transitions are valid and complete
- **Automatic diagram generation** - Generate state diagrams and transition tables directly from your specification
- **Graph analysis** - Convert your state machine into a graph data structure for advanced analysis

> If you're familiar with [Servant](https://haskell-servant.readthedocs.io/) from Haskell, **Transit** follows a similar philosophy: just as Servant uses a REST API type-level specification to generate type-safe routing functions and OpenAPI documentation, **Transit** uses a state machine graph type-level specification to generate type-safe update functions and state diagrams.

### About This Documentation

All code examples in this documentation are extracted from actual, type-checked PureScript source files. Also, whenever you find an assertion or a full unit test, it's ensured that it ran and passed. In this sense this text is not just documentation, but also a test suite. At the bottom of every code example you can find a link to the actual source file. So you can get a better picture of the context and get information about the imports used.

### Installation

Transit is published to Pursuit. You can install it with `spago`:

```bash
spago install transit
```

## Example 1: A Simple Door

Let's start with a simple door state machine to demonstrate **Transit**'s core concepts. This example will show you how to define a state machine using **Transit**'s type-level DSL, implement a type-safe update function, and generate documentation automatically. We'll compare the traditional approach with **Transit**'s approach to highlight the benefits of compile-time safety and automatic documentation generation.

### The State Machine

Think of a door that can be either open or closed. When it's open, you can close it. When it's closed, you can open it. That's it—no other actions make sense. You can't open a door that's already open, and you can't close a door that's already closed. This simple behavior is what we're modeling here.

Before diving into the code, let's visualize our simple door state machine. This will help you understand the structure we're about to implement.

#### State Diagram

The state diagram below shows all possible states and the valid transitions between them:

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="graphs/simple-door-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="graphs/simple-door-light.svg">
  <img alt="Simple Door state diagram" src="graphs/simple-door-light.svg">
</picture>

In this diagram, you can see:

- **Two states**: `DoorOpen` and `DoorClosed` (shown as rounded rectangles)
- **Two transitions**: The `Close` transition moves from `DoorOpen` to `DoorClosed`, and the `Open` transition moves from `DoorClosed` to `DoorOpen`
- **Arrows**: The direction of each arrow shows which state changes are valid

#### Transition Table

For a more structured view, here's the corresponding transition table:

<!-- PD_START:raw
filePath: graphs/simple-door.html
--><table><thead><tr><th>From State</th><th /><th>Transition</th><th /><th>To State</th></tr></thead><tbody><tr><td>DoorOpen</td><td>⟶</td><td>Close</td><td>⟶</td><td>DoorClosed</td></tr></tbody><tbody><tr><td>DoorClosed</td><td>⟶</td><td>Open</td><td>⟶</td><td>DoorOpen</td></tr></tbody></table><!-- PD_END -->

This table provides the same information in a structured format. Each row shows one valid transition: which state you start in, which action you take, and which state you end up in. Notice that invalid actions—like trying to open an already open door—simply don't appear in the table.

Now let's see how we represent this in PureScript code.

### States and Messages

To represent our door in code, we need two things: the states the door can be in, and the actions that can change those states. In PureScript, we define these as simple data types:

<!-- PD_START:purs
filePath: test/Examples/SimpleDoor.purs
pick:
  - State
  - Msg
-->

```purescript
data State = DoorOpen | DoorClosed

data Msg = Close | Open
```

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L27-L29">test/Examples/SimpleDoor.purs L27-L29</a></sup></p>
<!-- PD_END -->

The `State` type captures the two possible states we saw in the diagram: `DoorOpen` and `DoorClosed`. The `Msg` type represents the two actions: `Close` and `Open`. These correspond directly to what we visualized earlier—each state and each transition from the diagram has a corresponding value in these types.

With these types in place, we can now implement the logic that handles state transitions. Let's first look at the traditional approach, then see how **Transit** improves upon it.

### State updates: The Classic Approach

Now that we have our types, we need a function that takes the current state and a message, and returns the new state. The traditional way to implement this is with a pattern-matching function:

<!-- PD_START:purs
filePath: test/Examples/SimpleDoor.purs
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

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L35-L39">test/Examples/SimpleDoor.purs L35-L39</a></sup></p>
<!-- PD_END -->

This function handles the two valid transitions we saw in the diagram: closing an open door and opening a closed door. The catch-all case `_, _ -> state` handles any invalid combinations (like trying to open an already open door) by returning the current state unchanged.

While this approach works and is straightforward, it has some drawbacks:

- **No compile-time safety**: The compiler won't catch if you forget to handle a valid transition or if you add a new state but forget to update the function
- **Documentation drift**: If you update the state diagram, there's nothing ensuring the code stays in sync—you have to remember to update both manually
- **Manual maintenance**: You need to manually ensure all cases are handled correctly, and there's no way to verify completeness at compile time

### State updates: The Transit Approach

With the **Transit** library, we take a different approach that addresses the drawbacks of the classic method. Instead of writing the update function directly, we first define a type-level specification that describes our state machine. This specification serves as a single source of truth that the compiler can verify.

#### The Type-Level Specification

First, we define the state machine structure using **Transit**'s type-level DSL:

<!-- PD_START:purs
filePath: test/Examples/SimpleDoor.purs
pick:
  - SimpleDoorTransit
-->

```purescript
type SimpleDoorTransit =
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
```

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L45-L48">test/Examples/SimpleDoor.purs L45-L48</a></sup></p>
<!-- PD_END -->

Breaking down the syntax:

- `Empty` initializes an empty transition list
- `:*` is an infix operator that appends each transition to the list
- `"DoorOpen" :@ "Close" >| "DoorClosed"` means: in state `DoorOpen`, when receiving message `Close`, transition to state `DoorClosed`
- The `@` operator connects a state to a message, and `>|` indicates the target state

This type-level specification fully defines the state machine's structure. The compiler can now use this specification to ensure our implementation is correct.

#### The Update Function

Based on this specification, we create an update function using `mkUpdateGeneric`:

<!-- PD_START:purs
filePath: test/Examples/SimpleDoor.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @SimpleDoorTransit
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")
```

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L50-L53">test/Examples/SimpleDoor.purs L50-L53</a></sup></p>
<!-- PD_END -->

Here's how this works:

- `mkUpdateGeneric @SimpleDoorTransit` creates an update function based on the `SimpleDoorTransit` specification. The `@` symbol is type application, passing the specification to the function.
- Each `match` line handles one transition from the specification. The first two arguments (`@"DoorOpen"` and `@"Close"`) are type-level symbols (type applications) that specify which state and message to match on. The lambda function defines what happens when that transition occurs.
- `return @"DoorClosed"` specifies which state to transition to. The `return` function is part of **Transit**'s DSL for specifying the target state, and the `@` symbol again indicates a type-level symbol.

Notice that the type signature is identical to the classic approach—`State -> Msg -> State`. This means you can use **Transit**'s update function as a drop-in replacement without changing any calling code.

#### How This Solves the Classic Approach's Problems

This approach addresses all the drawbacks we saw earlier:

- **Compile-time safety**: The compiler verifies that your `match` lines exactly correspond to the specification. If you miss a transition or add an invalid one, the code won't compile.
- **No documentation drift**: The specification is the source of truth. If you change the spec, the compiler forces you to update the implementation to match.
- **Automatic verification**: You don't need to manually check completeness—the compiler does it for you. Every transition in the spec must have a corresponding `match` line, and you can't add extra matches that aren't in the spec.

### Writing Tests for the update function

Since both the classic and **Transit** approaches have the same type signature (`State -> Msg -> State`), they can be used interchangeably. Let's see how to write tests for the update function and verify that both approaches behave identically.

#### Testing State Transitions

To test our update function, we'll use two useful functions from the `Data.Array` module:

<!-- PD_START:purs
inline: true
pick:
  - tag: signature_or_foreign
    name: foldl
    filePath: .spago/p/arrays-7.3.0/src/Data/Array.purs
    prefix: '- '
  - tag: signature_or_foreign
    name: scanl
    filePath: .spago/p/arrays-7.3.0/src/Data/Array.purs
    prefix: '- '
split: true
-->

- `foldl :: forall a b. (b -> a -> b) -> b -> Array a -> b`
- `scanl :: forall a b. (b -> a -> b) -> b -> Array a -> Array b`

<!-- PD_END -->

The simplest way to test the update function is to use `foldl` to apply a sequence of messages and check if the final state matches what we expect:

<!-- PD_START:purs
filePath: test/Examples/SimpleDoor.purs
pick:
  - tag: value
    name: assert1
-->

```purescript
assert1 =
  (foldl update DoorOpen [ Close, Open, Close ])
    `shouldEqual`
      DoorClosed
```

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L60-L63">test/Examples/SimpleDoor.purs L60-L63</a></sup></p>
<!-- PD_END -->

This test starts with the door open, closes it, opens it, then closes it again. It checks that we end up with the door closed, as expected.

This test only checks the final result. To be more thorough, we should also verify that each step along the way works correctly. The `scanl` function is perfect for this—it shows us all the intermediate states, not just the final one.

<!-- PD_START:purs
filePath: test/Examples/SimpleDoor.purs
pick:
  - tag: value
    name: assert2
-->

```purescript
assert2 =
  (scanl update DoorOpen [ Close, Open, Close ])
    `shouldEqual`
      [ DoorClosed, DoorOpen, DoorClosed ]
```

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L66-L69">test/Examples/SimpleDoor.purs L66-L69</a></sup></p>
<!-- PD_END -->

This test does the same thing—starts with the door open, closes it, opens it, then closes it again. But instead of just checking the final result, it verifies each step along the way: after closing, the door is closed; after opening, the door is open; and after closing again, the door is closed. This makes sure each transition works correctly.

Since we'll want to write many of these tests, it's helpful to define a reusable helper function. The `assertWalk` function takes an update function, an initial state, and a list of message/state pairs representing the expected walk through the state machine:

<!-- PD_START:purs
pick:
  - tag: any
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
assertWalk updateFn initState walk = do
  let
    msgs :: Array msg
    msgs = map fst walk

    expectedStates :: Array state
    expectedStates = map snd walk

    actualStates :: Array state
    actualStates = scanl updateFn initState msgs

  actualStates `shouldEqual` expectedStates
```

<!-- PD_END -->

The function extracts the messages from the pairs, applies them sequentially using `scanl`, and verifies that the resulting states match the expected ones. Here's how we use it:

<!-- PD_START:purs
pick:
  - tag: value
    name: assert3
    filePath: test/Examples/SimpleDoor.purs
-->

```purescript
assert3 =
  assertWalk update
    DoorOpen
    [ Close ~> DoorClosed
    , Open ~> DoorOpen
    , Close ~> DoorClosed
    ]
```

<!-- PD_END -->

#### Verifying Interchangeability

Since both approaches have identical type signatures, we should verify they produce the same results. The following test runs the same sequence of messages through both `updateClassic` and `update`, checking that all intermediate states match:

<!-- PD_START:purs
filePath: test/Examples/SimpleDoor.purs
pick:
  - tag: value
    name: assert4
-->

```purescript
assert4 =
  for_ [ updateClassic, update ]
    \fn ->
      assertWalk fn
        DoorOpen
        [ Close ~> DoorClosed
        , Open ~> DoorOpen
        , Open ~> DoorOpen
        , Close ~> DoorClosed
        , Open ~> DoorOpen
        ]
```

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L81-L91">test/Examples/SimpleDoor.purs L81-L91</a></sup></p>
<!-- PD_END -->

### Generating Diagrams and Tables

**Transit** can generate both state diagrams and transition tables directly from your type-level specification. Both generation processes use the same approach: `reflectType` converts your type-level DSL specification to a term-level equivalent, which can then be used to generate the documentation.

#### State Diagrams

To generate a state diagram, you use `TransitGraphviz.writeToFile` to render a Graphviz `.dot` file:

<!-- PD_START:purs
filePath: src/Transit/Generators/Graphviz.purs
inline: true
pick:
  - tag: signature_or_foreign
    name: writeToFile
    prefix: '- '
split: true
-->

- `writeToFile :: FilePath -> TransitCore -> (Options -> Options) -> Effect Unit`

<p align="right"><sup>🗎 <a href="src/Transit/Generators/Graphviz.purs#L212-L212">src/Transit/Generators/Graphviz.purs L212-L212</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/SimpleDoor.purs
pick:
  - generateStateDiagram
-->

```purescript
generateStateDiagram :: Effect Unit
generateStateDiagram = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

  TransitGraphviz.writeToFile "graphs/simple-door-light.dot" transit _
    { theme = themeHarmonyLight
    }

  TransitGraphviz.writeToFile "graphs/simple-door-dark.dot" transit _
    { theme = themeHarmonyDark
    }
```

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L106-L117">test/Examples/SimpleDoor.purs L106-L117</a></sup></p>
<!-- PD_END -->

The process works in two steps:

1. `reflectType` converts your type-level DSL specification to a term-level equivalent
2. `TransitGraphviz.writeToFile` uses that to render a Graphviz `.dot` file

The `writeToFile` function accepts an options record that lets you customize the diagram. The `theme` option controls the color scheme. **Transit** provides six built-in themes:

- `themeHarmonyLight` and `themeHarmonyDark` - Harmonious color palettes
- `themeContrastLight` and `themeContrastDark` - High-contrast palettes for better visibility
- `themeGradientLight` and `themeGradientDark` - Gradient-based color schemes

To convert the `.dot` file to an SVG (or other formats), use the Graphviz command-line tools:

```bash
dot -Tsvg graphs/simple-door.dot -o graphs/simple-door.svg
```

Or for PNG:

```bash
dot -Tpng graphs/simple-door.dot -o graphs/simple-door.png
```

#### Transition Tables

In addition to state diagrams, you can also generate transition tables from the same specification. This provides a tabular view of all state transitions, which can be easier to read for some use cases.

The process is identical—you use `reflectType` to convert your DSL specification, but then use `TransitTable.writeToFile` instead:

<!-- PD_START:purs
filePath: test/Examples/SimpleDoor.purs
pick:
  - generateTransitionTable
-->

```purescript
generateTransitionTable :: Effect Unit
generateTransitionTable = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

  TransitTable.writeToFile "graphs/simple-door.html" transit identity
```

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L119-L124">test/Examples/SimpleDoor.purs L119-L124</a></sup></p>
<!-- PD_END -->

This generates an HTML file containing a table with columns for "From State", "Message", and "To State". The table can be embedded directly in documentation (as shown in the examples above) or viewed in a browser.

Since both the state diagram and transition table are generated from the same DSL specification, they're guaranteed to be consistent with each other and with your type-level specification.

### Conclusion

In this example, we've seen how **Transit** helps you build type-safe state machines. We started with a simple door that can be open or closed, and learned the core workflow:

1. **Define the state machine** using **Transit**'s type-level DSL specification
2. **Implement the update function** using `mkUpdateGeneric` with `match` clauses that the compiler verifies against the specification
3. **Generate documentation** automatically—both state diagrams and transition tables—from the same specification

The key advantage is that your specification, implementation, and documentation all stay in sync because they share the same source of truth. The compiler ensures your code matches your specification, and your documentation is generated directly from it.

While this example was simple, it demonstrates **Transit**'s fundamental approach. In the next example, we'll see how **Transit** handles more complex scenarios with states that contain data and conditional transitions.

## Example 2: Door with Pin

Full source code: _[test/Examples/DoorWithPin.purs](test/Examples/DoorWithPin.purs)_

Now let's add a PIN code to our door lock. This introduces two important concepts: **states with data** and **conditional transitions**.

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="graphs/door-with-pin-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="graphs/door-with-pin-light.svg">
  <img alt="Door with Pin state diagram" src="graphs/door-with-pin-light.svg">
</picture>

In this example, the `DoorLocked` state stores a PIN code, and the `Unlock` message includes the entered PIN. The unlock operation can succeed (transitioning to `DoorClosed`) or fail (staying in `DoorLocked`), depending on whether the entered PIN matches the stored one.

Notice the diamond node in the state diagram—this represents a conditional transition where the outcome depends on runtime data.

The transition table shows both possible outcomes:

<!-- PD_START:raw
filePath: graphs/door-with-pin.html
--><table><caption>Door with Pin</caption><thead><tr><th>From State</th><th /><th>Transition</th><th /><th>To State</th></tr></thead><tbody><tr><td>DoorOpen</td><td>⟶</td><td>Close</td><td>⟶</td><td>DoorClosed</td></tr></tbody><tbody><tr><td>DoorClosed</td><td>⟶</td><td>Open</td><td>⟶</td><td>DoorOpen</td></tr></tbody><tbody><tr><td>DoorClosed</td><td>⟶</td><td>Lock</td><td>⟶</td><td>DoorLocked</td></tr></tbody><tbody><tr><td>DoorLocked</td><td>⟶</td><td>Unlock ? PinIncorrect</td><td>⟶</td><td>DoorLocked</td></tr></tbody><tbody><tr><td>DoorLocked</td><td>⟶</td><td>Unlock ? PinCorrect</td><td>⟶</td><td>DoorClosed</td></tr></tbody></table><!-- PD_END -->

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

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L25-L34">test/Examples/DoorWithPin.purs L25-L34</a></sup></p>
<!-- PD_END -->

### State updates: The Classic Approach

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

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L40-L50">test/Examples/DoorWithPin.purs L40-L50</a></sup></p>
<!-- PD_END -->

<p align="right">
  <sup>🗎 <a href="test/Examples/DoorWithPin.purs">Examples/DoorWithPin.purs</a></sup>
</p>

### State updates: The Transit Approach

In the DSL specification, we express conditional transitions by listing multiple possible target states:

<!-- PD_START:purs
filePath: test/Examples/DoorWithPin.purs
pick:
  - DoorWithPinTransit
-->

```purescript
type DoorWithPinTransit =
  Transit $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
    :* ("DoorClosed" :@ "Lock" >| "DoorLocked")
    :*
      ( "DoorLocked" :@ "Unlock"
          >| ("PinCorrect" :? "DoorClosed")
          >| ("PinIncorrect" :? "DoorLocked")
      )
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L56-L65">test/Examples/DoorWithPin.purs L56-L65</a></sup></p>
<!-- PD_END -->

The syntax `("PinCorrect" :? "DoorClosed") >| ("PinIncorrect" :? "DoorLocked")` indicates that the `Unlock` message from `DoorLocked` can transition to either state, depending on runtime conditions. The `:?` operator associates a condition label (like `"PinCorrect"`) with a target state, and `>|` chains multiple conditional outcomes together.

The update function now has access to both the current state and the message data, allowing you to implement the conditional logic:

<!-- PD_START:purs
filePath: test/Examples/DoorWithPin.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @DoorWithPinTransit
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
        returnVia @"PinCorrect" @"DoorClosed"
      else
        returnVia @"PinIncorrect" @"DoorLocked" { pin: state.pin }
  )
```

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L67-L83">test/Examples/DoorWithPin.purs L67-L83</a></sup></p>
<!-- PD_END -->

The match handlers receive both the current state and the message, giving you access to all the data needed to make runtime decisions. The type system still ensures that:

- 🔴 You can only return states that are valid targets for that transition
- 🔴 You handle all required transitions
- 🟢 The conditional logic is type-safe

### Type signatures

Understanding the type signatures that **Transit** enforces helps clarify how the type system ensures correctness. This section demonstrates the exact types that each match handler must satisfy, showing how **Transit** uses `Variant` types to represent subsets of possible states.

Full source code: _[test/Examples/Signatures.purs](test/Examples/Signatures.purs)_

This chapter demonstrates the type signatures that **Transit** enforces for your update functions. To show these signatures without implementing the actual logic, we use an `unimplemented` helper function that satisfies the type checker:

<!-- PD_START:purs
filePath: test/Examples/Signatures.purs
pick:
  - unimplemented
-->

```purescript
unimplemented :: forall a. a
unimplemented = unsafeCoerce "not yet implemented"
```

<p align="right"><sup>🗎 <a href="test/Examples/Signatures.purs#L15-L16">test/Examples/Signatures.purs L15-L16</a></sup></p>
<!-- PD_END -->

The `update` function demonstrates the type signatures that **Transit** enforces. The straightforward part is the `State` and `Msg` types—each match handler receives the exact state and message types for that transition. However, the return type is more complex: depending on the specification, a transition may allow multiple possible target states, so we need to return a subset of the state type.

Unfortunately, PureScript's ADTs (Algebraic Data Types) don't allow expressing a subset of cases from a union type. This is where `Variant` comes in—it's perfect for representing a subset of cases from a union type. Each match handler must return a `Variant` type that precisely matches the possible target states defined in the DSL specification.

This approach requires internal conversion between ADT and `Variant` representations. If you'd like to avoid this conversion overhead, you can define your `State` and `Msg` types as `Variant` directly from the start, as shown in the next chapter.

<!-- PD_START:purs
filePath: test/Examples/Signatures.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @DoorWithPinTransit
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
            :: Variant ("DoorLocked" :: ReturnState { pin :: String })
      )
  )
  ( match @"DoorLocked" @"Unlock"
      ( \(state :: { pin :: String }) (msg :: { enteredPin :: String }) ->
          unimplemented
            :: Variant
                 ( "DoorClosed" :: ReturnStateVia "PinCorrect" Unit
                 , "DoorLocked" :: ReturnStateVia "PinIncorrect" { pin :: String }
                 )
      )
  )
```

<p align="right"><sup>🗎 <a href="test/Examples/Signatures.purs#L18-L46">test/Examples/Signatures.purs L18-L46</a></sup></p>
<!-- PD_END -->

### Variants

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
update = mkUpdate @DoorWithPinTransit
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
        returnVia @"PinCorrect" @"DoorClosed"
      else
        returnVia @"PinIncorrect" @"DoorLocked" { pin: state.pin }
  )
```

<p align="right"><sup>🗎 <a href="test/Examples/Variants.purs#L17-L76">test/Examples/Variants.purs L17-L76</a></sup></p>
<!-- PD_END -->

## Example 3: Seven Bridges of Königsberg

So far, we've seen how **Transit** helps you build type-safe state machines and generate state diagrams and transition tables. But the power of **Transit** extends far beyond documentation generation. The reflected data structure—the term-level representation of your type-level DSL specification—can be converted into a general-purpose graph data structure, enabling sophisticated graph analysis.

This example demonstrates this capability using the famous [Seven Bridges of Königsberg](https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg) problem. In 1736, the mathematician Leonhard Euler was asked whether it was possible to walk through the city of Königsberg crossing each of its seven bridges exactly once. Euler's solution to this problem laid the foundation for graph theory.

The problem can be modeled as a graph where:

- **Nodes** represent the four land areas (A, B, C, and D)
- **Edges** represent the seven bridges connecting them

The following picture shows roughly how the actual map looked like back then:

<img src="assets/bridges.png" width="450" />

<img src="assets/bridges-walk.png" width="450" />

Even not immediately obvious, this can be represented as a graph:

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="graphs/bridges-koenigsberg-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="graphs/bridges-koenigsberg-light.svg">
  <img alt="Seven Bridges of Königsberg graph" src="graphs/bridges-koenigsberg-light.svg">
</picture>

While **Transit** is designed for directed state machines, we can model an undirected graph by defining bidirectional transitions for each bridge. The renderer can then summarize these complementary edges into a single undirected edge for visualization. Notice how each bridge has two transitions—one in each direction:

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

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L29-L38">test/Examples/BridgesKoenigsberg.purs L29-L38</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - BridgesKoenigsbergTransit
-->

```purescript
type BridgesKoenigsbergTransit =
  Transit $ Empty
    :* ("LandA" |< "Cross_a" >| "LandB")
    :* ("LandA" |< "Cross_b" >| "LandB")
    :* ("LandA" |< "Cross_c" >| "LandC")
    :* ("LandA" |< "Cross_d" >| "LandC")
    :* ("LandA" |< "Cross_e" >| "LandD")
    :* ("LandB" |< "Cross_f" >| "LandD")
    :* ("LandC" |< "Cross_g" >| "LandD")
```

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L73-L81">test/Examples/BridgesKoenigsberg.purs L73-L81</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @BridgesKoenigsbergTransit
  (match @"LandA" @"Cross_a" \_ _ -> return @"LandB")
  (match @"LandB" @"Cross_a" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_b" \_ _ -> return @"LandB")
  (match @"LandB" @"Cross_b" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_c" \_ _ -> return @"LandC")
  (match @"LandC" @"Cross_c" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_d" \_ _ -> return @"LandC")
  (match @"LandC" @"Cross_d" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_e" \_ _ -> return @"LandD")
  (match @"LandD" @"Cross_e" \_ _ -> return @"LandA")

  (match @"LandB" @"Cross_f" \_ _ -> return @"LandD")
  (match @"LandD" @"Cross_f" \_ _ -> return @"LandB")

  (match @"LandC" @"Cross_g" \_ _ -> return @"LandD")
  (match @"LandD" @"Cross_g" \_ _ -> return @"LandC")
```

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L83-L104">test/Examples/BridgesKoenigsberg.purs L83-L104</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - assert1
-->

```purescript
assert1 :: Aff Unit
assert1 =
  for_ [ updateClassic, update ] \fn ->
    assertWalk fn
      LandA
      [ Cross_a /\ LandB
      , Cross_f /\ LandD
      , Cross_g /\ LandC
      , Cross_c /\ LandA
      , Cross_e /\ LandD
      , Cross_g /\ LandC
      , Cross_d /\ LandA
      , Cross_b /\ LandB
      ]
```

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L110-L123">test/Examples/BridgesKoenigsberg.purs L110-L123</a></sup></p>
<!-- PD_END -->

<!-- PD_START:raw
filePath: graphs/bridges-koenigsberg.html
--><table><thead><tr><th>From State</th><th /><th>Transition</th><th /><th>To State</th></tr></thead><tbody><tr><td>LandB</td><td>⟵</td><td>Cross_a</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandB</td><td>⟵</td><td>Cross_b</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandC</td><td>⟵</td><td>Cross_c</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandC</td><td>⟵</td><td>Cross_d</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandD</td><td>⟵</td><td>Cross_e</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandD</td><td>⟵</td><td>Cross_f</td><td>⟶</td><td>LandB</td></tr></tbody><tbody><tr><td>LandD</td><td>⟵</td><td>Cross_g</td><td>⟶</td><td>LandC</td></tr></tbody></table><!-- PD_END -->

The transition table shows the undirected nature of the graph—each bridge can be crossed in both directions. When generating the visualization, the renderer summarizes these bidirectional edges into a single undirected edge:

### Graph Analysis

The real power of **Transit** becomes apparent when we convert the reflected data structure into a general-purpose graph. Using `mkStateGraph`, we transform the **Transit** specification into a `StateGraph`—a specialized `Graph` type configured with edge and node labels suitable for state machine analysis.

Once we have this graph data structure, we can perform sophisticated analysis using standard graph algorithms. For the Seven Bridges problem, we want to determine if the graph has an **Eulerian circuit** (a path that visits every edge exactly once and returns to the starting point) or an **Eulerian trail** (a path that visits every edge exactly once but doesn't necessarily return to the start).

Euler's theorem states that:

- An undirected graph has an Eulerian trail if and only if it is connected and has exactly zero or two vertices of odd degree

We can check these conditions using helper functions from the `Test.Examples.Common` module:

<!-- PD_START:purs
filePath: test/Examples/Common.purs
pick:
  - nodeDegree
-->

```purescript
nodeDegree :: StateNode -> StateGraph -> Int
nodeDegree state graph = Set.size (Graph.getOutgoingEdges state graph)
```

<p align="right"><sup>🗎 <a href="test/Examples/Common.purs#L17-L18">test/Examples/Common.purs L17-L18</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/Common.purs
pick:
  - hasEulerTrail
-->

```purescript
hasEulerTrail :: StateGraph -> Boolean
hasEulerTrail graph =
  let
    nodes :: Array StateNode
    nodes = fromFoldable (Graph.getNodes graph)

    countEdgesByNode :: Array Int
    countEdgesByNode = map (\node -> Set.size (Graph.getOutgoingEdges node graph)) nodes

    sumOddEdges :: Int
    sumOddEdges = (Array.length <<< Array.filter Int.odd) countEdgesByNode
  in
    sumOddEdges == 2 || sumOddEdges == 0
```

<p align="right"><sup>🗎 <a href="test/Examples/Common.purs#L20-L32">test/Examples/Common.purs L20-L32</a></sup></p>
<!-- PD_END -->

To perform the analysis, we convert the reflected **Transit** specification into a graph and then check its properties:

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - spec
  - main
-->

```purescript
spec :: Spec Unit
spec = do
  describe ".." do
    it "should assert1" do
      assert1
      assert2

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @BridgesKoenigsbergTransit)

  for_
    [ { theme: themeHarmonyLight, file: "graphs/bridges-koenigsberg-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/bridges-koenigsberg-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { useUndirectedEdges = true
        , theme = opts.theme
        }

  TransitTable.writeToFile "graphs/bridges-koenigsberg.html" transit _
    { useUndirectedEdges = true }
```

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L135-L162">test/Examples/BridgesKoenigsberg.purs L135-L162</a></sup></p>
<!-- PD_END -->

The key steps are:

1. **Reflect the type-level specification**: `reflectType (Proxy @BridgesKoenigsbergTransit)` converts the type-level DSL to a term-level representation
2. **Convert to a graph**: `mkStateGraph transit` transforms the **Transit** specification into a `StateGraph`—a general-purpose graph data structure
3. **Perform analysis**: Use graph analysis functions like `hasEulerCircle` and `hasEulerTrail` to check properties

<!-- PD_START:purs
filePath: test/Examples/Common.purs
pick:
  - nodeDegree
-->

```purescript
nodeDegree :: StateNode -> StateGraph -> Int
nodeDegree state graph = Set.size (Graph.getOutgoingEdges state graph)
```

<p align="right"><sup>🗎 <a href="test/Examples/Common.purs#L17-L18">test/Examples/Common.purs L17-L18</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/Common.purs
pick:
  - hasEulerTrail
-->

```purescript
hasEulerTrail :: StateGraph -> Boolean
hasEulerTrail graph =
  let
    nodes :: Array StateNode
    nodes = fromFoldable (Graph.getNodes graph)

    countEdgesByNode :: Array Int
    countEdgesByNode = map (\node -> Set.size (Graph.getOutgoingEdges node graph)) nodes

    sumOddEdges :: Int
    sumOddEdges = (Array.length <<< Array.filter Int.odd) countEdgesByNode
  in
    sumOddEdges == 2 || sumOddEdges == 0
```

<p align="right"><sup>🗎 <a href="test/Examples/Common.purs#L20-L32">test/Examples/Common.purs L20-L32</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - assert1
-->

```purescript
assert1 :: Aff Unit
assert1 =
  for_ [ updateClassic, update ] \fn ->
    assertWalk fn
      LandA
      [ Cross_a /\ LandB
      , Cross_f /\ LandD
      , Cross_g /\ LandC
      , Cross_c /\ LandA
      , Cross_e /\ LandD
      , Cross_g /\ LandC
      , Cross_d /\ LandA
      , Cross_b /\ LandB
      ]
```

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L110-L123">test/Examples/BridgesKoenigsberg.purs L110-L123</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - assert2
-->

```purescript
assert2 :: Aff Unit
assert2 = do
  hasEulerTrail graph `shouldEqual` false
```

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L131-L133">test/Examples/BridgesKoenigsberg.purs L131-L133</a></sup></p>
<!-- PD_END -->

These functions check whether the graph is undirected and count how many vertices have an odd number of outgoing edges. For the Seven Bridges of Königsberg:

- **LandA** has 5 bridges (odd)
- **LandB** has 3 bridges (odd)
- **LandC** has 3 bridges (odd)
- **LandD** has 3 bridges (odd)

Since all four vertices have an odd degree, the graph has **4 vertices with odd degree**. According to Euler's theorem, this means:

- ❌ The graph does **not** have an Eulerian circuit (would require 0 odd-degree vertices)
- ❌ The graph does **not** have an Eulerian trail (would require 0 or 2 odd-degree vertices)

This confirms Euler's original conclusion: it's impossible to walk through Königsberg crossing each bridge exactly once.

This example demonstrates that **Transit**'s value extends far beyond state machine documentation. By reflecting the type-level specification to a term-level graph data structure, you gain access to a rich ecosystem of graph algorithms and analysis tools. The same DSL that ensures compile-time correctness for your state transitions can also power runtime graph analysis, pathfinding, cycle detection, and more.

In the next example, we'll see a graph that **does** have an Eulerian trail, demonstrating how **Transit** can help verify and understand graph properties beyond simple state machines.

## Example 4: The house of Santa Claus

Full source code: _[test/Examples/HouseOfSantaClaus.purs](test/Examples/HouseOfSantaClaus.purs)_

This example uses "Das Haus vom Nikolaus" (The house of Santa Claus), a well-known German drawing puzzle. The challenge is to draw a house shape in one continuous stroke without lifting the pen and without retracing any line. In German-speaking countries, this puzzle is commonly associated with Saint Nicholas (Nikolaus), hence the name. The puzzle is equivalent to finding an Eulerian trail in the graph representing the house's edges.

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
  - HouseOfSantaClausTransit
-->

```purescript
type HouseOfSantaClausTransit =
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

<p align="right"><sup>🗎 <a href="test/Examples/HouseOfSantaClaus.purs#L80-L104">test/Examples/HouseOfSantaClaus.purs L80-L104</a></sup></p>
<!-- PD_END -->

<!-- PD_START:raw
filePath: graphs/house-of-santa-claus.html
--><table><thead><tr><th>From State</th><th /><th>Transition</th><th /><th>To State</th></tr></thead><tbody><tr><td>N_2</td><td>⟵</td><td>E_a</td><td>⟶</td><td>N_1</td></tr></tbody><tbody><tr><td>N_3</td><td>⟵</td><td>E_b</td><td>⟶</td><td>N_2</td></tr></tbody><tbody><tr><td>N_5</td><td>⟵</td><td>E_c</td><td>⟶</td><td>N_3</td></tr></tbody><tbody><tr><td>N_5</td><td>⟵</td><td>E_d</td><td>⟶</td><td>N_4</td></tr></tbody><tbody><tr><td>N_4</td><td>⟵</td><td>E_e</td><td>⟶</td><td>N_1</td></tr></tbody><tbody><tr><td>N_3</td><td>⟵</td><td>E_f</td><td>⟶</td><td>N_1</td></tr></tbody><tbody><tr><td>N_4</td><td>⟵</td><td>E_g</td><td>⟶</td><td>N_2</td></tr></tbody><tbody><tr><td>N_4</td><td>⟵</td><td>E_h</td><td>⟶</td><td>N_3</td></tr></tbody></table><!-- PD_END -->

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="graphs/house-of-santa-claus-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="graphs/house-of-santa-claus-light.svg">
  <img alt="House of Santa Claus graph" src="graphs/house-of-santa-claus-light.svg">
</picture>

<!-- PD_START:purs
filePath: test/Examples/HouseOfSantaClaus.purs
pick:
  - spec
  - main
-->

```purescript
spec :: Spec Unit
spec = do
  describe "House of Santa Claus" do
    it "should have 8 states" do
      let transit = reflectType (Proxy @HouseOfSantaClausTransit)
      let graph = mkStateGraph transit

      let walk = [ E_f, E_h, E_g, E_a, E_e, E_d, E_c, E_b ]

      Array.length (Array.nub walk) `shouldEqual` 8

      foldl update N_1 walk `shouldEqual` N_2

      hasEulerTrail graph `shouldEqual` true
      pure unit

    describe "should follow the walk" do
      let
        initState = N_1

        walk =
          [ { msg: E_f, state: N_3 }
          , { msg: E_h, state: N_4 }
          , { msg: E_g, state: N_2 }
          , { msg: E_a, state: N_1 }
          , { msg: E_e, state: N_4 }
          , { msg: E_d, state: N_5 }
          , { msg: E_c, state: N_3 }
          , { msg: E_b, state: N_2 }
          ]

      let
        msgs = map _.msg walk
        expectedStates = map _.state walk

      describe "classic update" do
        it "should follow the walk" do
          let actualStates = scanl updateClassic initState msgs
          actualStates `shouldEqual` expectedStates

      describe "transit update" do
        it "should follow the walk" do
          let actualStates = scanl update initState msgs
          actualStates `shouldEqual` expectedStates

main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @HouseOfSantaClausTransit)
    nodeAttrs = Just \node -> case node of
      "N_1" -> "pos=\"0,0!\""
      "N_2" -> "pos=\"2,0!\""
      "N_3" -> "pos=\"2,2!\""
      "N_4" -> "pos=\"0,2!\""
      "N_5" -> "pos=\"1,3!\""
      _ -> ""
    globalAttrs = Just "layout=neato"

  for_
    [ { theme: themeHarmonyLight, file: "graphs/house-of-santa-claus-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/house-of-santa-claus-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { useUndirectedEdges = true
        , nodeAttrsRaw = nodeAttrs
        , globalAttrsRaw = globalAttrs
        , theme = opts.theme
        }

  TransitTable.writeToFile "graphs/house-of-santa-claus.html" transit _
    { useUndirectedEdges = true }
```

<p align="right"><sup>🗎 <a href="test/Examples/HouseOfSantaClaus.purs#L136-L211">test/Examples/HouseOfSantaClaus.purs L136-L211</a></sup></p>
<!-- PD_END -->

## More

### Monadic update functions

Full source code: _[test/Examples/Monadic.purs](test/Examples/Monadic.purs)_

So far, all our examples have used pure update functions with the type signature `State -> Msg -> State`. However, sometimes you need to perform side effects during state transitions—such as logging, making HTTP requests, or interacting with external systems.

For these cases, **Transit** provides `mkUpdateGenericM`, which creates update functions that operate in a monadic context. The type signature becomes `State -> Msg -> m State`, where `m` is any `Monad` (commonly `Effect`, `Aff`, or `ReaderT`).

The key differences from pure update functions are:

1. **Use `mkUpdateGenericM` instead of `mkUpdateGeneric`** - This tells **Transit** you want a monadic update function
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
update = mkUpdateGenericM @SimpleDoorTransit
  ( matchM @"DoorOpen" @"Close" \_ _ -> do
      Console.log "You just closed the door"
      pure $ return @"DoorClosed"
  )
  ( matchM @"DoorClosed" @"Open" \_ _ -> do
      Console.log "You just opened the door"
      pure $ return @"DoorOpen"
  )
```

<p align="right"><sup>🗎 <a href="test/Examples/Monadic.purs#L10-L19">test/Examples/Monadic.purs L10-L19</a></sup></p>
<!-- PD_END -->

Each handler can now perform side effects (like logging) before returning the new state. The `return` function still works the same way—you wrap your state value with it, and then wrap that in `pure` to lift it into the monadic context.

