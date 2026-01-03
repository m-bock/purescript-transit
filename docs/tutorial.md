# Introduction

**Transit** is a PureScript library for building type-safe state machines. It provides a type-level DSL for specifying state transitions. You define your state machine once using this specification, and the compiler ensures your implementation matches it — eliminating bugs from invalid transitions, missing cases, or documentation drift.

This tutorial will guide you through the basics of **Transit** by showing it's main features based on examples. We'll start with a simple door state machine and gradually add more complexity.

**About This Documentation**

All code examples in this documentation are extracted from actual, type-checked PureScript source files. Also, whenever you find an assertion or a full unit test, it's ensured that it ran and passed. In this sense this text is not just documentation, but also a test suite. At the bottom of every code example you can find a link to the actual source file. So you can get a better picture of the context and get information about the imports used.

> If you're familiar with Servant[^servant] from Haskell, **Transit** follows a similar philosophy: just as Servant uses a REST API type-level specification to ensure type-safe routing functions and generate OpenAPI documentation, **Transit** uses a state machine graph type-level specification to ensure type-safe update functions and generate state diagrams.

# Example 1: A Simple Door

> Full source code: _[test/Examples/DoorSimple.purs](test/Examples/DoorSimple.purs)_

<img src="assets/door-simple-header.jpg" width="450" />

Let's start with a simple door state machine to demonstrate **Transit**'s core concepts. This example will show you how to define a state machine using **Transit**'s type-level DSL, implement a type-safe update function, and generate documentation automatically. We'll compare the traditional approach with **Transit**'s approach to highlight the benefits of the latter.

## The State Machine

Think of a door that can be either open or closed. When it's open, you can close it. When it's closed, you can open it. That's it — no other actions make sense. You can't open a door that's already open, and you can't close a door that's already closed. This simple behavior is what we're modeling here.

Before diving into the code, let's visualize our simple door state machine. This will help you understand the structure we're about to implement.

### State Diagram

The state diagram below shows all possible states and the valid transitions between them:

**Door State Diagram**

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="renders/door-simple-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="renders/door-simple-light.svg">
  <img class="state-diagram" alt="Simple Door state diagram" src="renders/door-simple-light.svg">
</picture>

In this diagram, you can see:

- **Two states**: `DoorOpen` and `DoorClosed` (shown as rectangles)
- **Two transitions**: The `Close` transition moves from `DoorOpen` to `DoorClosed`, and the `Open` transition moves from `DoorClosed` to `DoorOpen`
- **Arrows**: The direction of each arrow shows which state changes are valid

**Terminology note**: Throughout this documentation, the terms _message_, _action_, and _transition_ are used interchangeably to refer to the events that trigger state changes[^terminology]. In **Transit**'s type system, these correspond to the message types in your `Msg` variant.

### Transition Table

For a more structured view, here's the corresponding transition table:

<!-- PD_START:raw
filePath: renders/door-simple.md
wrapNl: true
-->
| State      |       | Message |       | State      |
| ---------- | ----- | ------- | ----- | ---------- |
| DoorOpen   | **⟶** | Close   | **⟶** | DoorClosed |
| DoorClosed | **⟶** | Open    | **⟶** | DoorOpen   |

<!-- PD_END -->

Each row shows one valid transition: which state you start in, which action you take, and which state you end up in. Notice that invalid actions — like trying to open an already open door — simply don't appear in the table.

Now let's see how we represent this in PureScript code.

## Classic Approach

Before diving into **Transit**, let's first look at how state machines are typically implemented in PureScript using pattern matching. This classic approach is familiar to most PureScript developers and serves as a baseline for understanding what **Transit** improves upon.

### States and Message types

To represent our door in code, we need two major types: the states the door can be in, and the actions that can change those states. In PureScript, we define these as simple data types. We are using the suffix `D` to denote the traditional approach (D = data).

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - StateD
  - MsgD
-->

```purescript
data StateD
  = DoorOpen
  | DoorClosed

data MsgD
  = Close
  | Open
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L30-L36">test/Examples/DoorSimple.purs L30-L36</a>
  </sup>
</p>

<!-- PD_END -->

The `State` type captures the two possible states we saw in the diagram: `DoorOpen` and `DoorClosed`. The `Msg` type represents the two actions: `Close` and `Open`. These correspond directly to what we visualized earlier — each state and each transition from the diagram has a corresponding value in these types.

### The update function

Now that we have our types, we need a function that takes the current state and a message, and returns the new state. The traditional way to implement this is with a pattern-matching function:

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - updateD
-->

```purescript
updateD :: StateD -> MsgD -> StateD
updateD state msg =
  case state, msg of
    DoorOpen, Close -> DoorClosed
    DoorClosed, Open -> DoorOpen
    _, _ -> state
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L38-L43">test/Examples/DoorSimple.purs L38-L43</a>
  </sup>
</p>

<!-- PD_END -->

We pattern match on both the current state and the message at once. It could also be written as a nested pattern match.

This function handles the two valid transitions we saw in the diagram: closing an open door and opening a closed door. The catch-all case `_, _ -> state` handles any invalid combinations (like trying to open an already open door) by returning the current state unchanged.

While this approach works and is straightforward, it has some drawbacks:

- **Implicit state machine specification**: The state machine's structure is only defined implicitly within the update function's pattern matching and return values.

- **Documentation drift**: If you maintain a state diagram for documentation purposes, there's nothing ensuring the code stays in sync — you have to remember to update both manually.

- **Limited analysis capabilities**: There's no way to analyze the state machine's structure or behavior statically — you can only understand it by running the code.

## Transit Approach

With the **Transit** library, we take a different approach that addresses the drawbacks of the classic method. Instead of writing the update function directly, we first define a type-level specification that describes our state machine. This specification serves as a single source of truth that the compiler can verify against your implementation.

### The Type-Level Specification

First, we define the state machine structure using **Transit**'s type-level DSL:

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - DoorSimpleTransit
-->

```purescript
type DoorSimpleTransit =
  Transit
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L59-L62">test/Examples/DoorSimple.purs L59-L62</a>
  </sup>
</p>

<!-- PD_END -->

Breaking down the syntax:

- `Transit` initializes an empty transition list
- `:*` is an infix operator that appends each transition to the list
- `"DoorOpen" :@ "Close" >| "DoorClosed"` means: in state `DoorOpen`, when receiving message `Close`, transition to state `DoorClosed`
- The `@` operator connects a state to a message, and `>|` indicates the target state

This type-level specification fully defines the state machine's structure. The compiler can now use this specification to ensure our implementation is correct.

### State and Message Types

**Transit** uses `Variant` types (from `purescript-variant`)[^variant] for both `State` and `Msg` instead of traditional ADTs. Variants are open sum types where each constructor is labeled with a type-level symbol (like `"DoorOpen"` or `"Close"`).

This design choice is crucial for **Transit**'s type-level machinery. The key advantage is that **Transit** can filter the possible cases (both input states/messages and output states) for each handler function. Variants are perfect for this. There is no way to express a subset of cases from a traditional ADT.

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - State
  - Msg
-->

```purescript
type State = Variant
  ( "DoorOpen" :: {}
  , "DoorClosed" :: {}
  )

type Msg = Variant
  ( "Close" :: {}
  , "Open" :: {}
  )
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L49-L57">test/Examples/DoorSimple.purs L49-L57</a>
  </sup>
</p>

<!-- PD_END -->

### The Update Function

Based on this specification, we create an update function using `mkUpdate`:

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdate @DoorSimpleTransit
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L64-L67">test/Examples/DoorSimple.purs L64-L67</a>
  </sup>
</p>

<!-- PD_END -->

Here's how this works:

- `mkUpdate @DoorSimpleTransit` creates an update function based on the `DoorSimpleTransit` specification. The `@` symbol is type application[^type-app], passing the specification to the function.

- Each `match` line handles one transition from the specification. The first two arguments (`@"DoorOpen"` and `@"Close"`) are type-level symbols (type applications) that specify which state and message to match on. The lambda function defines what happens when that transition occurs.

- `return @"DoorClosed"` specifies which state to transition to. The `return` function is part of **Transit**'s DSL for specifying the target state, and the `@` symbol again indicates a type-level symbol.

- **Important**: The order of match handlers must match the order of transitions in the DSL specification. In this example, the handlers are provided in the same order as they appear in `DoorSimpleTransit`: `DoorOpen :@ Close`, then `DoorClosed :@ Open`.

### How This Solves the Classic Approach's Problems

This approach addresses all the drawbacks we saw earlier:

- **Explicit state machine specification**: The state machine's structure is defined explicitly in the type-level DSL. This specification serves as a single source of truth that is accessible for various purposes.

- **No documentation drift**: Documentation such as state diagrams and transition tables can be generated directly from the specification, ensuring they always stay in sync with the code.

- **Static analysis capabilities**: The specification can be converted into a graph data structure, enabling sophisticated static analysis of the state machine's properties without running the code.

## Testing the update function

Before we move further, let's actually verify that our implementation of the update function works as we expect it to. We'll do this by writing some tests.

### Creating Variant Values

To create values of type `Variant`, **Transit** provides the `v` function from `Transit.VariantUtils`. It's a convenience wrapper around `Variant`'s `inj` function that uses type application (no Proxy needed) and allows omitting empty record arguments:

- Transit record payload (argument can be omitted)

  ```purescript
  v @"DoorOpen"
  ```

- Non-empty payload (must provide the data)
  ```purescript
  v @"DoorLocked" { activePin: "1234" }
  ```

This is more ergonomic than using `V.inj (Proxy :: _ "DoorOpen") {}` directly.

### Testing State Transitions

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
filePath: test/Examples/DoorSimple.purs
pick:
  - tag: value
    name: spec1
-->

```purescript
spec1 =
  it "follows the walk and ends in expected final state" do
    foldl update (v @"DoorOpen") [ v @"Close", v @"Open", v @"Close" ]
      `shouldEqual`
        (v @"DoorClosed")
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L77-L81">test/Examples/DoorSimple.purs L77-L81</a>
  </sup>
</p>

<!-- PD_END -->

This test starts with the door open, closes it, opens it, then closes it again. It checks that we end up with the door closed, as expected.

This test only checks the final result. To be more thorough, we should also verify that each step along the way works correctly. The `scanl` function is perfect for this — it shows us all the intermediate states, not just the final one.

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - tag: value
    name: spec2
-->

```purescript
spec2 =
  it "follows the walk and visits the expected intermediate states" do
    scanl update (v @"DoorOpen") [ v @"Close", v @"Open", v @"Close" ]
      `shouldEqual`
        [ v @"DoorClosed", v @"DoorOpen", v @"DoorClosed" ]
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L84-L88">test/Examples/DoorSimple.purs L84-L88</a>
  </sup>
</p>

<!-- PD_END -->

This test does the same thing — starts with the door open, closes it, opens it, then closes it again. But instead of just checking the final result, it verifies each step along the way: after closing, the door is closed; after opening, the door is open; and after closing again, the door is closed. This makes sure each transition works correctly.

Since we'll want to write more of these tests for further examples, it's helpful to define a reusable helper function. The `assertWalk` function takes an update function, an initial state, and a list of message/state pairs representing the expected walk through the state machine:

<!-- PD_START:purs
filePath: test/Examples/Common.purs
pick:
  - assertWalk
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

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Common.purs#L40-L59">test/Examples/Common.purs L40-L59</a>
  </sup>
</p>

<!-- PD_END -->

The function extracts the messages from the pairs, applies them sequentially using `scanl`, and verifies that the resulting states match the expected ones. Here's how we use it:

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - tag: value
    name: spec3
-->

```purescript
spec3 =
  it "follows the walk and visits the expected intermediate states" do
    assertWalk update
      (v @"DoorOpen")
      [ v @"Close" ~> v @"DoorClosed"
      , v @"Open" ~> v @"DoorOpen"
      , v @"Close" ~> v @"DoorClosed"
      , v @"Close" ~> v @"DoorClosed"
      , v @"Open" ~> v @"DoorOpen"
      , v @"Open" ~> v @"DoorOpen"
      , v @"Open" ~> v @"DoorOpen"
      ]
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L91-L102">test/Examples/DoorSimple.purs L91-L102</a>
  </sup>
</p>

<!-- PD_END -->

The `~>` operator is an infix alias for `Tuple`. So `v @"Close" ~> v @"DoorClosed"` is equivalent to `Tuple (v @"Close") (v @"DoorClosed")`.

We read it like: Starting from state `DoorOpen`, when receiving message `Close`, we expect the next state to be `DoorClosed`. From there, when receiving message `Open`, we expect the next state to be `DoorOpen`. And so on.

## Generating Documentation

**Transit** can generate both state diagrams and transition tables directly from your type-level specification. Both generation processes use the same approach: `reflectType` converts your type-level DSL specification to a term-level equivalent, which can then be used to generate the documentation.

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - doorSimpleTransit
-->

```purescript
doorSimpleTransit :: TransitCore
doorSimpleTransit = reflectType (Proxy @DoorSimpleTransit)
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L69-L70">test/Examples/DoorSimple.purs L69-L70</a>
  </sup>
</p>

<!-- PD_END -->

### State Diagrams

To generate a state diagram, you use `TransitGraphviz.writeToFile` to render a Graphviz `.dot` file:

<!-- PD_START:purs
inline: true
pick:
  - tag: signature_or_foreign
    name: generate
    filePath: src/Transit/Render/Graphviz.purs
split: true
-->

`generate :: TransitCore -> (Options -> Options) -> String`

<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - generateStateDiagram
-->

```purescript
generateStateDiagram :: Effect Unit
generateStateDiagram = do
  FS.writeTextFile UTF8 "renders/door-simple-light.dot"
    ( TransitGraphviz.generate doorSimpleTransit _
        { theme = themeHarmonyLight
        }
    )

  FS.writeTextFile UTF8 "renders/door-simple-dark.dot"
    ( TransitGraphviz.generate doorSimpleTransit _
        { theme = themeHarmonyDark
        }
    )
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L115-L127">test/Examples/DoorSimple.purs L115-L127</a>
  </sup>
</p>

<!-- PD_END -->

The process works in two steps:

1. `reflectType` converts your type-level DSL specification to a term-level equivalent of type `TransitCore`
2. `TransitGraphviz.writeToFile` uses that to render a Graphviz `.dot` file

The `writeToFile` function accepts an options record that lets you customize the diagram. E.g. the `theme` option which we're using above controls the color scheme. **Transit** provides a couple of built-in themes. But you can also provide your own. See [themes.md](docs/themes.md) for more details.

To convert the `.dot` file to an SVG (or other formats), use the Graphviz[^graphviz] command-line tools:

```bash
dot -Tsvg renders/door-simple.dot -o renders/door-simple.svg
```

Or for PNG:

```bash
dot -Tpng renders/door-simple.dot -o renders/door-simple.png
```

### Transition Tables

In addition to state diagrams, you can also generate transition tables from the same specification. This provides a tabular view of all state transitions, which can be easier to read for some use cases.

The process is identical — you use `reflectType` to convert your DSL specification, but then use `TransitTable.writeToFile` instead:

<!-- PD_START:purs
filePath: test/Examples/DoorSimple.purs
pick:
  - generateTransitionTable
-->

```purescript
generateTransitionTable :: Effect Unit
generateTransitionTable = do
  FS.writeTextFile UTF8 "renders/door-simple.md"
    (TransitTable.generate doorSimpleTransit _ { outputFormat = TransitTable.Markdown })
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/DoorSimple.purs#L129-L132">test/Examples/DoorSimple.purs L129-L132</a>
  </sup>
</p>

<!-- PD_END -->

This generates a Markdown file containing a table with columns for "From State", "Message", and "To State".

Since both the state diagram and transition table are generated from the same DSL specification, they're guaranteed to be consistent with each other and with your type-level specification.

## Conclusion

In this example, we've seen how **Transit** helps you build type-safe state machines. We started with a simple door that can be open or closed, and learned the core workflow:

1. **Define the state machine** using **Transit**'s type-level DSL specification

2. **Implement the update function** using `mkUpdate` with `match` clauses that the compiler verifies against the specification

3. **Generate documentation** automatically — both state diagrams and transition tables — from the same specification

The key advantage is that your specification, implementation, and documentation all stay in sync because they share the same source of truth. The compiler ensures your code matches your specification, and your documentation is generated directly from it.

While this example was simple, it demonstrates **Transit**'s fundamental approach. In the next example, we'll see how **Transit** handles more complex scenarios with states that contain data and conditional transitions.

[^servant]: [Servant](https://haskell-servant.readthedocs.io/) is a Haskell library for building type-safe web APIs.
