<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [purescript-transit](#purescript-transit)
  - [Install](#install)
  - [Minimal Example](#minimal-example)
  - [Documentation](#documentation)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# purescript-transit

A library for building type-safe state machines.

- State Transitions are specified with type level DSL.
- Compile time guarantees that state update functions are complete and valid.
- Automatic state diagram generation
- State machine graph analysis
- Optimized for speed

## Install

(once published to package set:)

```bash
spago install transit
```

## Minimal Example

Define a state machine with a type-level DSL:

<!-- PD_START:purs
filePath: test/Examples/DoorSimpleReadme.purs
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
    <a href="test/Examples/DoorSimpleReadme.purs#L24-L27"
      >test/Examples/DoorSimpleReadme.purs L24-L27</a
    >
  </sup>
</p>

<!-- PD_END -->

Define the state and message types:

<!-- PD_START:purs
filePath: test/Examples/DoorSimpleReadme.purs
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
    <a href="test/Examples/DoorSimpleReadme.purs#L14-L22"
      >test/Examples/DoorSimpleReadme.purs L14-L22</a
    >
  </sup>
</p>

<!-- PD_END -->

Write update function that must match the state machine specification:

<!-- PD_START:purs
filePath: test/Examples/DoorSimpleReadme.purs
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
    <a href="test/Examples/DoorSimpleReadme.purs#L29-L32"
      >test/Examples/DoorSimpleReadme.purs L29-L32</a
    >
  </sup>
</p>

<!-- PD_END -->

Generate state diagram or perform other analysis on the state machine's runtime representation:

<!-- PD_START:purs
filePath: test/Examples/DoorSimpleReadme.purs
pick:
  - main
-->

```purescript
main :: Effect Unit
main = do
  let
    transit :: TransitCore
    transit = reflectType (Proxy @DoorSimpleTransit)

  FS.writeTextFile UTF8 "renders/door-simple-readme.dot"
    (TransitGraphviz.generate_ transit)
```

<p align="right">
  <sup
    >🗎
    <a href="test/Examples/DoorSimpleReadme.purs#L34-L41"
      >test/Examples/DoorSimpleReadme.purs L34-L41</a
    >
  </sup>
</p>

<!-- PD_END -->

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="renders/door-simple-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="renders/door-simple-light.svg">
  <img alt="Simple Door state diagram" src="renders/door-simple-light.svg">
</picture>

## Documentation

- [API Reference](https://pursuit.purescript.org/packages/purescript-transit/docs/Transit)

<!--
- Tutorial
-->
