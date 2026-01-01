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

> Full source code: _[test/Examples/DoorReadme.purs](test/Examples/DoorReadme.purs)_

Define a state machine with a type-level DSL:

<!-- PD_START:purs
pick:
  - tag: any
    name: DoorTransit
    filePath: test/Examples/DoorReadme.purs
-->

```purescript
type DoorTransit =
  Transit
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
```

<!-- PD_END -->

Define the state and message types:

<!-- PD_START:purs
pick:
  - tag: any
    name: State
    filePath: test/Examples/DoorReadme.purs
  - tag: any
    name: Msg
    filePath: test/Examples/DoorReadme.purs
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

<!-- PD_END -->

Write update function that must match the state machine specification:

<!-- PD_START:purs
pick:
  - tag: any
    name: update
    filePath: test/Examples/DoorReadme.purs
-->

```purescript
update :: State -> Msg -> State
update = mkUpdate @DoorTransit
  (match @"DoorOpen" @"Close" \_ _ -> return @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return @"DoorOpen")
```

<!-- PD_END -->

Retrieve runtime representation of the state machine:

<!-- PD_START:purs
pick:
  - tag: any
    name: doorTransit
    filePath: test/Examples/DoorReadme.purs
-->

```purescript
doorTransit :: TransitCore
doorTransit = reflectType (Proxy @DoorTransit)
```

<!-- PD_END -->

Generate state diagram or perform other analysis on the state machine's runtime representation:

<!-- PD_START:purs
pick:
  - tag: any
    name: main
    filePath: test/Examples/DoorReadme.purs
-->

```purescript
main :: Effect Unit
main =
  FS.writeTextFile UTF8 "renders/door-readme.dot"
    (TransitGraphviz.generate_ doorTransit)
```

<!-- PD_END -->

The result will look like:

<img alt="Simple Door state diagram" src="renders/door-readme.svg">

## Documentation

- [API Reference](https://pursuit.purescript.org/packages/purescript-transit/docs/Transit)

<!--
- Tutorial
-->
