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
    - [State updates: The Classic Approach](#state-updates-the-classic-approach)
    - [State updates: The Transit Approach](#state-updates-the-transit-approach)
    - [Compile-Time Safety](#compile-time-safety)
    - [Writing Tests for the update function](#writing-tests-for-the-update-function)
    - [Generate State Diagrams](#generate-state-diagrams)
    - [Generate Transition Tables](#generate-transition-tables)
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

Transit is a PureScript library for building type-safe state machines. It provides a type-level DSL for specifying state transitions, ensuring that your state machine implementation is correct at compile time. With Transit, you define your state machine once using a type-level specification, and the compiler ensures your implementation matches that specification—eliminating bugs from invalid transitions, missing cases, or documentation drift.

> If you're familiar with [Servant](https://haskell-servant.readthedocs.io/) from Haskell, Transit follows a similar philosophy: just as Servant uses a REST API type-level specification to generate type-safe routing functions and OpenAPI documentation, Transit uses a state machine graph type-level specification to generate type-safe update functions and state diagrams.

## Introduction

### Key Features

- **Type-safe state transitions** - The compiler ensures all transitions are valid and complete
- **Automatic diagram generation** - Generate state diagrams and transition tables directly from your specification
- **Graph analysis** - Convert your state machine into a graph data structure for advanced analysis

### About This Documentation

All code examples in this documentation are extracted from actual, type-checked PureScript source files. Also, whenever you find an assertion or a full unit test, it's ensured that it ran and passed. In this sense this text is not just documentation, but also a test suite.

### Installation

```bash
spago install transit
```

## Example 1: A Simple Door

Let's start with a simple door state machine. Here's its state diagram:

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="graphs/simple-door-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="graphs/simple-door-light.svg">
  <img alt="Simple Door state diagram" src="graphs/simple-door-light.svg">
</picture>

This state machine has two states (`DoorOpen` and `DoorClosed`) and two transitions (`Close` and `Open`).

Another way to represent this is a transition table:

<!-- PD_START:raw
filePath: graphs/simple-door.html
--><table><caption>Simple Door State Machine</caption><thead><tr><th>From State</th><th /><th>Transition</th><th /><th>To State</th></tr></thead><tbody><tr><td>DoorOpen</td><td>⟶</td><td>Close</td><td>⟶</td><td>DoorClosed</td></tr></tbody><tbody><tr><td>DoorClosed</td><td>⟶</td><td>Open</td><td>⟶</td><td>DoorOpen</td></tr></tbody></table><!-- PD_END -->

In PureScript, we represent those states and messages by simple data types:

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

### State updates: The Classic Approach

The traditional way to implement state transitions is to write an update function that takes a state and a message and returns a new state:

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

While this approach works, it has some drawbacks:

- The state diagram and implementation can easily get out of sync
- The compiler won't catch missing transitions or invalid state/message combinations
- You need to manually ensure all cases are handled correctly

### State updates: The Transit Approach

With the transit library, we take a different approach. First, we define a type-level specification of the state machine:

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

This DSL syntax reads as: "From state `DoorOpen` on message `Close`, transition to state `DoorClosed`" and "From state `DoorClosed` on message `Open`, transition to state `DoorOpen`". The `Empty` constructor initializes an empty transition list, and `:*` is an infix operator that appends each transition to the list, building up the complete state machine specification.

This type-level specification fully defines the state machine. Based on this spec, we can now create an update function that the compiler ensures only allows legal state transitions:

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

Notice that the type signature is identical to the classic approach—`State -> Msg -> State`.

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

### Writing Tests for the update function

As the equality of the type signatures of the classic and transit approaches update functions already suggest: We can use them interchangeably. That means transit can be seamlessly integrated into existing codebases. Let's verify this.

#### Testing State Transitions

We'll make use of the following functions from the `Data.Array` module of the `arrays` package:

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

Now, how can we test the update function? An easy way is to perform a fold over the messages and check if the final state is the expected one:

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

However, this only checks the final state. We should also check all intermediate states. For this purpose the scanl function is perfectly suited. It works like foldl, but it returns an array of all intermediate states.

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

Since we want to perform many of these tests, we'll define a convenient helper function `assertWalk` that performs the test and asserts the result:

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

We can use it like this:

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
    [ Close /\ DoorClosed
    , Open /\ DoorOpen
    , Close /\ DoorClosed
    ]
```

<!-- PD_END -->

#### Verifying Interchangeability

Of course we want to check this with both the classic and transit approaches. The following test checks all intermediate states for both approaches.

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
        [ Close /\ DoorClosed
        , Open /\ DoorOpen
        , Open /\ DoorOpen
        , Close /\ DoorClosed
        , Open /\ DoorOpen
        ]
```

<p align="right"><sup>🗎 <a href="test/Examples/SimpleDoor.purs#L81-L91">test/Examples/SimpleDoor.purs L81-L91</a></sup></p>
<!-- PD_END -->

### Generate State Diagrams

One of the key benefits of transit is that you can generate state diagrams directly from your type-level specification. This ensures your diagrams always stay in sync with your code—no manual updates required.

To generate a state diagram, you use `reflectType` to convert your type-level DSL specification to a term-level equivalent, then write it to a Graphviz `.dot` file:

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

<p align="right"><sup>🗎 <a href="src/Transit/Generators/Graphviz.purs#L211-L211">src/Transit/Generators/Graphviz.purs L211-L211</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: src/Transit/Generators/Graphviz.purs
pick:
  - Options
-->

```purescript
type Options =
  { title :: String
  , theme :: Theme
  , globalAttrsRaw :: Maybe String
  , nodeAttrsRaw :: Maybe (String -> String)
  , useDecisionNodes :: Boolean
  , useUndirectedEdges :: Boolean
  , entryPoints :: Array String
  }
```

<p align="right"><sup>🗎 <a href="src/Transit/Generators/Graphviz.purs#L190-L198">src/Transit/Generators/Graphviz.purs L190-L198</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/GenerateStateDiagrams.purs
pick:
  - main
-->

```purescript
main :: Effect Unit
main = do
  let
    transit = reflectType (Proxy @SimpleDoorTransit)

  for_
    [ { theme: themeHarmonyLight, file: "graphs/simple-door-light.dot" }
    , { theme: themeHarmonyDark, file: "graphs/simple-door-dark.dot" }
    ]
    \opts ->
      TransitGraphviz.writeToFile opts.file transit _
        { title = "Simple Door"
        , theme = opts.theme
        }
```

<p align="right"><sup>🗎 <a href="test/Examples/GenerateStateDiagrams.purs#L13-L26">test/Examples/GenerateStateDiagrams.purs L13-L26</a></sup></p>
<!-- PD_END -->

The process works in two steps:

1. `reflectType` converts your type-level DSL specification to a term-level equivalent
2. `TransitGraphviz.writeToFile` uses that to render a Graphviz `.dot` file

To convert the `.dot` file to an SVG (or other formats), use the Graphviz command-line tools:

```bash
dot -Tsvg graphs/simple-door.dot -o graphs/simple-door.svg
```

Or for PNG:

```bash
dot -Tpng graphs/simple-door.dot -o graphs/simple-door.png
```

Since the diagram is generated from the same DSL specification used to create the type-safe update function, any changes to your state machine are automatically reflected in both the code and the diagram. This eliminates the common problem of documentation getting out of sync with implementation.

### Generate Transition Tables

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
    transit = reflectType (Proxy @SimpleDoorTransit)

  TransitTable.writeToFile "graphs/simple-door.html" transit _
    { title = "Simple Door" }
```

<p align="right"><sup>🗎 <a href="test/Examples/GenerateTransitionTables.purs#L11-L17">test/Examples/GenerateTransitionTables.purs L11-L17</a></sup></p>
<!-- PD_END -->

<p align="right">

This generates an HTML file containing a table with columns for "From State", "Message", and "To State". The table can be embedded directly in documentation (as shown in the examples above) or viewed in a browser.

Since both the state diagram and transition table are generated from the same DSL specification, they're guaranteed to be consistent with each other and with your type-level specification.

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

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L23-L32">test/Examples/DoorWithPin.purs L23-L32</a></sup></p>
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

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L38-L48">test/Examples/DoorWithPin.purs L38-L48</a></sup></p>
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

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L54-L63">test/Examples/DoorWithPin.purs L54-L63</a></sup></p>
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

<p align="right"><sup>🗎 <a href="test/Examples/DoorWithPin.purs#L65-L81">test/Examples/DoorWithPin.purs L65-L81</a></sup></p>
<!-- PD_END -->

The match handlers receive both the current state and the message, giving you access to all the data needed to make runtime decisions. The type system still ensures that:

- 🔴 You can only return states that are valid targets for that transition
- 🔴 You handle all required transitions
- 🟢 The conditional logic is type-safe

### Type signatures

Understanding the type signatures that transit enforces helps clarify how the type system ensures correctness. This section demonstrates the exact types that each match handler must satisfy, showing how transit uses `Variant` types to represent subsets of possible states.

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

<p align="right"><sup>🗎 <a href="test/Examples/Signatures.purs#L15-L16">test/Examples/Signatures.purs L15-L16</a></sup></p>
<!-- PD_END -->

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

Full source code: _[test/Examples/BridgesKoenigsberg.purs](test/Examples/BridgesKoenigsberg.purs)_

So far, we've seen how transit helps you build type-safe state machines and generate state diagrams and transition tables. But the power of transit extends far beyond documentation generation. The reflected data structure—the term-level representation of your type-level DSL specification—can be converted into a general-purpose graph data structure, enabling sophisticated graph analysis.

This example demonstrates this capability using the famous [Seven Bridges of Königsberg](https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg) problem. In 1736, the mathematician Leonhard Euler was asked whether it was possible to walk through the city of Königsberg (now Kaliningrad) crossing each of its seven bridges exactly once and returning to the starting point. Euler's solution to this problem laid the foundation for graph theory.

The problem can be modeled as a graph where:

- **Nodes** represent the four land areas (A, B, C, and D)
- **Edges** represent the seven bridges connecting them

<img src="assets/bridges.png" />

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

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L30-L39">test/Examples/BridgesKoenigsberg.purs L30-L39</a></sup></p>
<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - BridgesKoenigsbergTransit
-->

```purescript
type BridgesKoenigsbergTransit =
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

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L71-L92">test/Examples/BridgesKoenigsberg.purs L71-L92</a></sup></p>
<!-- PD_END -->

<!-- PD_START:raw
filePath: graphs/bridges-koenigsberg.html
--><table><caption>Untitled</caption><thead><tr><th>From State</th><th /><th>Transition</th><th /><th>To State</th></tr></thead><tbody><tr><td>LandB</td><td>⟵</td><td>Cross_a</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandB</td><td>⟵</td><td>Cross_b</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandC</td><td>⟵</td><td>Cross_c</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandC</td><td>⟵</td><td>Cross_d</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandD</td><td>⟵</td><td>Cross_e</td><td>⟶</td><td>LandA</td></tr></tbody><tbody><tr><td>LandD</td><td>⟵</td><td>Cross_f</td><td>⟶</td><td>LandB</td></tr></tbody><tbody><tr><td>LandD</td><td>⟵</td><td>Cross_g</td><td>⟶</td><td>LandC</td></tr></tbody></table><!-- PD_END -->

The transition table shows the undirected nature of the graph—each bridge can be crossed in both directions. When generating the visualization, the renderer summarizes these bidirectional edges into a single undirected edge:

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="graphs/bridges-koenigsberg-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="graphs/bridges-koenigsberg-light.svg">
  <img alt="Seven Bridges of Königsberg graph" src="graphs/bridges-koenigsberg-light.svg">
</picture>

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

<p align="right"><sup>🗎 <a href="test/Examples/Common.purs#L17-L34">test/Examples/Common.purs L17-L34</a></sup></p>
<!-- PD_END -->

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
  describe ".." do
    it "..." do
      let transit = reflectType (Proxy @BridgesKoenigsbergTransit)
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

<p align="right"><sup>🗎 <a href="test/Examples/BridgesKoenigsberg.purs#L121-L154">test/Examples/BridgesKoenigsberg.purs L121-L154</a></sup></p>
<!-- PD_END -->

The key steps are:

1. **Reflect the type-level specification**: `reflectType (Proxy @BridgesKoenigsbergTransit)` converts the type-level DSL to a term-level representation
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
--><table><caption>Untitled</caption><thead><tr><th>From State</th><th /><th>Transition</th><th /><th>To State</th></tr></thead><tbody><tr><td>N_2</td><td>⟵</td><td>E_a</td><td>⟶</td><td>N_1</td></tr></tbody><tbody><tr><td>N_3</td><td>⟵</td><td>E_b</td><td>⟶</td><td>N_2</td></tr></tbody><tbody><tr><td>N_5</td><td>⟵</td><td>E_c</td><td>⟶</td><td>N_3</td></tr></tbody><tbody><tr><td>N_5</td><td>⟵</td><td>E_d</td><td>⟶</td><td>N_4</td></tr></tbody><tbody><tr><td>N_4</td><td>⟵</td><td>E_e</td><td>⟶</td><td>N_1</td></tr></tbody><tbody><tr><td>N_3</td><td>⟵</td><td>E_f</td><td>⟶</td><td>N_1</td></tr></tbody><tbody><tr><td>N_4</td><td>⟵</td><td>E_g</td><td>⟶</td><td>N_2</td></tr></tbody><tbody><tr><td>N_4</td><td>⟵</td><td>E_h</td><td>⟶</td><td>N_3</td></tr></tbody></table><!-- PD_END -->

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

      hasEulerCircle graph `shouldEqual` false
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

<p align="right"><sup>🗎 <a href="test/Examples/HouseOfSantaClaus.purs#L136-L212">test/Examples/HouseOfSantaClaus.purs L136-L212</a></sup></p>
<!-- PD_END -->

## More

### Monadic update functions

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
