<picture>
  <source media="(prefers-color-scheme: dark)" srcset="assets/logo-dark.png">
  <source media="(prefers-color-scheme: light)" srcset="assets/logo-light.png">
  <img alt="State diagram" src="assets/logo-light.png">
</picture>

Type-Safe State Machines.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Transit](#transit)
  - [Installation](#installation)
  - [Example1: Door](#example1-door)
  - [Example2: Door with Lock](#example2-door-with-lock)
  - [Example3: Door with Pin](#example3-door-with-pin)
  - [Example4: Espresso Machine](#example4-espresso-machine)
  - [Tests](#tests)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Transit

## Installation

```bash
spago install transit
```

## Example1: Door

Let's have a look at the following state diagram:

<img src="graphs/door.svg" />

It has two states (`DoorOpen` and `DoorClosed`) and two messages (`Close` and `Open`). Initial state is `DoorOpen` indicated by the grey arrow pointing to it. In PureScript types, we can represent this with the following data types:

<!-- PD_START:raw
filePath: graphs/door.html
--><table >
<thead >
<tr >
<th >
From State
</th>
<th >
Message
</th>
<th >
Guard
</th>
<th >
To State
</th>
</tr>
</thead>
<tr >
<td >
DoorOpen
</td>
<td >
Close
</td>
<td >

</td>
<td >
DoorClosed
</td>
</tr>
<tr >
<td >
DoorClosed
</td>
<td >
Open
</td>
<td >

</td>
<td >
DoorOpen
</td>
</tr>
</table><!-- PD_END -->

<!-- PD_START:purs
filePath: test/Test/Examples/Door.purs
pick:
  - State
  - Msg
-->

```purescript
data State = DoorOpen | DoorClosed

data Msg = Close | Open
```

<!-- PD_END -->

the classic approach to implement the state machine in pure functional programming is to write an update function that takes a state and a message and returns a new state. For example:

<!-- PD_START:purs
filePath: test/Test/Examples/Door.purs
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

<!-- PD_END -->

The state diagram shows clearly the characteristics of the state machine. E.g. we see right away that the door can be opened and closed infinitely. In other words: There are no unwanted dead ends.

Unfortunately the state diagram and the actual implementation can easily get out of sync.

With the transit library we take a slightly different approach. We define first a type level specification of the state machine. It looks like this:

<!-- PD_START:purs
filePath: test/Test/Examples/Door.purs
pick:
  - DoorDSL
-->

```purescript
type DoorDSL =
  Wrap $ Empty
    :* ("DoorOpen" :@ "Close" >| "DoorClosed")
    :* ("DoorClosed" :@ "Open" >| "DoorOpen")
```

<!-- PD_END -->

This fully specifies the state machine. Based on this spec we can now an update function which only allows implementations legal state transitions. For example:

<!-- PD_END -->

<!-- PD_START:purs
filePath: test/Test/Examples/Door.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdateGeneric @DoorDSL
  (match @"DoorOpen" @"Close" \_ _ -> return' @"DoorClosed")
  (match @"DoorClosed" @"Open" \_ _ -> return' @"DoorOpen")
```

<!-- PD_END -->

As you can see the type of the update function is exactly the same as the type of the update function we wrote in the classic approach. The most interesting part here is what would _not_ compile:

- Missing a match line for a state transition
- Returning illegal states
- Missing names of states and messages

Indeed this is the only possible implementation of this particular state machine. Like for example there is only one possible way to implement the identity function.
Later we see examples where there are multiple possible implementations.

Moreover we can now generate the state diagram from the spec:

<!-- PD_START:purs
filePath: test/Test/Examples/Door.purs
pick:
  - main
-->

```purescript
main :: Effect Unit
main = do
  TransitGraphviz.writeToFile_ (reflectType (Proxy @DoorDSL)) "graphs/door.dot"
  TransitTable.writeToFile_ (reflectType (Proxy @DoorDSL)) "graphs/door.html"
```

<!-- PD_END -->

## Example2: Door with Lock

<img src="graphs/door-with-lock.svg" />

## Example3: Door with Pin

<img src="graphs/door-with-pin.svg" />

## Example4: Espresso Machine

<img src="graphs/espresso-machine-state-diagram.svg" alt="Transit" />

## Tests

We can be much more confident now that the state machine is correct.

We can even go one step further and write tests to verify certain properties of the state machine. For example we can verify that there are no dead ends in the state machine:

<!-- PD_START:purs
filePath: test/Test/Examples/Door.purs
pick:
  - spec
-->

```purescript
spec :: Spec Unit
spec = do
  describe "Dead ends" do
    it "should be empty" do
      let r = reflectType (Proxy @DoorDSL)
      let states = R.getStates r
      let deadEnds = Array.filter (\x -> R.getOutgoing x r == []) states
      deadEnds `shouldEqual` []
```

<!-- PD_END -->

```purescript


type Temperature = Number
type WaterLevel = Number

data State
  = Idle
  | Heating { targetTemp :: Temperature }
  | Ready   { waterLevel :: WaterLevel, temp :: Temperature }
  | Brewing { seconds :: Int }
  | Error { message :: String }
  | Done



data Msg
  = PowerOn { targetTemp :: Temperature }
  | TempReached { temp :: Temperature }
  | StartBrew { pumpOK :: Boolean, waterLevel :: WaterLevel }
  | BrewTick { deltaSeconds :: Int }   -- <— new
  | BrewComplete
  | SensorErrorDetected { message :: String }
  | RefillDone { waterAdded :: WaterLevel }
  | PowerOff
  | Reset


```
