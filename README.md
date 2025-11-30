![Light](assets/logo-light.png#gh-light-mode-only)
![Dark](assets/logo-dark.png#gh-dark-mode-only)

Type-Safe State Machines.

<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->

- [Transit](#transit)
  - [A](#a)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Transit

## A

<img src="graphs/door-graph.svg" />

-

<img src="graphs/espresso-machine-state-diagram.svg" alt="Transit" />

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
