<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="assets/logo-light.svg">
  <img alt="Transit logo" src="assets/logo-light.svg">
</picture>

A library for building type-safe state machines.

# purescript-transit

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

**Table of Contents**

- [Features](#features)
- [Documentation](#documentation)
- [Installation](#installation)
- [Minimal Example: A Count Down State Machine](#minimal-example-a-count-down-state-machine)
  - [Types](#types)
  - [Update Function](#update-function)
  - [Generate State Diagram](#generate-state-diagram)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

## Features

- State Transitions are specified with **type level DSL**.
- **Compile time guarantees** that state update functions are complete and valid.
- Automatic **state diagram generation**
- State machine **graph analysis**
- Optimized for **speed**

## Documentation

- [API Reference](https://pursuit.purescript.org/packages/purescript-transit/docs/Transit)
- [Tutorial](https://m-bock.github.io/purescript-transit)

## Installation

(once published to package set:)

```bash
spago install transit
```

## Minimal Example: A Count Down State Machine

> Full source code: _[test/Examples/CountDown.purs](test/Examples/CountDown.purs)_

Let's consider a simple count down state machine which is described by the following state diagram:

<img alt="Count Down state diagram" src="renders/count-down.svg">

### Types

To implement this state machine with **Transit** first we need to define the state and message types as Variants:

<!-- PD_START:purs
pick:
  - tag: any
    name: State
    filePath: test/Examples/CountDown.purs
  - tag: any
    name: Msg
    filePath: test/Examples/CountDown.purs
-->

```purescript
type State = Variant
  ( "Idle" :: {}
  , "Counting" :: { count :: Int }
  , "Done" :: {}
  )

type Msg = Variant
  ( "Start" :: { initialCount :: Int }
  , "Tick" :: {}
  , "Reset" :: {}
  )
```

<!-- PD_END -->

Then we need to define the state machine transitions as follows. Note that the last transition has 2 possible return states.

<!-- PD_START:purs
pick:
  - tag: any
    name: CountDownTransit
    filePath: test/Examples/CountDown.purs
-->

```purescript
type CountDownTransit =
  Transit
    :* ("Idle" :@ "Start" >| "Counting")
    :* ("Done" :@ "Reset" >| "Idle")
    :*
      ( "Counting" :@ "Tick"
          >| "Counting"
          >| "Done"
      )
```

<!-- PD_END -->

### Update Function

Finally, we write the update function that is checked at compile against the state machine specification:

<!-- PD_START:purs
pick:
  - tag: any
    name: update
    filePath: test/Examples/CountDown.purs
-->

```purescript
update :: State -> Msg -> State
update = mkUpdate @CountDownTransit
  ( match @"Idle" @"Start" \_ msg ->
      return @"Counting" { count: msg.initialCount }
  )
  ( match @"Done" @"Reset" \_ _ ->
      return @"Idle"
  )
  ( match @"Counting" @"Tick" \state _ ->
      let
        nextCount = state.count - 1
      in
        if nextCount == 0 then
          return @"Done"
        else
          return @"Counting" { count: nextCount }
  )
```

<!-- PD_END -->

### Generate State Diagram

Reflect type level state machine specification to a term level representation:

<!-- PD_START:purs
pick:
  - tag: any
    name: countDownTransit
    filePath: test/Examples/CountDown.purs
-->

```purescript
countDownTransit :: TransitCore
countDownTransit = reflectType (Proxy @CountDownTransit)
```

<!-- PD_END -->

Generate state diagram or perform other analysis on the state machine's runtime representation:

<!-- PD_START:purs
pick:
  - tag: any
    name: main
    filePath: test/Examples/CountDown.purs
-->

```purescript
main :: Effect Unit
main = do
  let
    graph :: String
    graph = TransitGraphviz.generate_ countDownTransit

  FS.writeTextFile UTF8 "renders/count-down.dot" graph
```

<!-- PD_END -->
