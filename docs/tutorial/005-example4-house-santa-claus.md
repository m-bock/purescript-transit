# Example 4: House of Santa Claus

Full source code: _[test/Examples/HouseOfSantaClaus.purs](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseOfSantaClaus.purs)_

![House of Santa Claus drawing game](assets/house-santa-claus-solution.png){width=350}

Do you remember the puzzle where you try to draw the house of Santa Claus in one continuous line — without lifting your pen and without retracing any line?
And while doing that you were supposed to say out loud the 8 syllables "This-Is-The-House-Of-San-Ta-Claus" — one syllable for each line.

## The State Machine

Of course we can model this puzzle as a state machine. We have states (1 through 5) and 8 transitions (a through h). Again, this is an undirected graph:

![House of Santa Claus state diagram](renders/house-santa-claus_graph-dark.svg){.dark-light}

Accordingly, the transition table looks like this:

<!-- PD_START:raw { filePath: renders/house-santa-claus_table.md, wrapNl: true } -->

| State |       | Message |       | State |
| ----- | ----- | ------- | ----- | ----- |
| 1     | **⟵** | a       | **⟶** | 2     |
| 2     | **⟵** | b       | **⟶** | 3     |
| 3     | **⟵** | c       | **⟶** | 5     |
| 5     | **⟵** | d       | **⟶** | 4     |
| 4     | **⟵** | e       | **⟶** | 1     |
| 1     | **⟵** | f       | **⟶** | 3     |
| 2     | **⟵** | g       | **⟶** | 4     |
| 3     | **⟵** | h       | **⟶** | 4     |

<!-- PD_END -->

## Implementation using Transit

### State and message types

Nothing special here. We define the state and message types like we did in the previous examples. By using Variants, we have no constraint on how the labels are named; we can use any type-level string we want: numbers, lowercase letters, etc. Traditional ADTs wouldn't give us this flexibility.

<!-- PD_START:purs { filePath: test/Examples/HouseSantaClaus.purs, pick: [State, Msg] } -->

```purescript
type State = Variant
  ( "1" :: {}
  , "2" :: {}
  , "3" :: {}
  , "4" :: {}
  , "5" :: {}
  )

type Msg = Variant
  ( "a" :: {}
  , "b" :: {}
  , "c" :: {}
  , "d" :: {}
  , "e" :: {}
  , "f" :: {}
  , "g" :: {}
  , "h" :: {}
  )
```

[test/Examples/HouseSantaClaus.purs (lines 27-44)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L27-L44){.fileLink}


<!-- PD_END -->

### Type-level specification

The transit specification follows the same pattern as in the previous example.

<!-- PD_START:purs { filePath: test/Examples/HouseSantaClaus.purs, pick: [SantaTransit] } -->

```purescript
type SantaTransit =
  Transit
    :* ("1" |< "a" >| "2")
    :* ("2" |< "b" >| "3")
    :* ("3" |< "c" >| "5")
    :* ("5" |< "d" >| "4")
    :* ("4" |< "e" >| "1")
    :* ("1" |< "f" >| "3")
    :* ("2" |< "g" >| "4")
    :* ("3" |< "h" >| "4")
```

[test/Examples/HouseSantaClaus.purs (lines 46-55)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L46-L55){.fileLink}


<!-- PD_END -->

### The Update Function

Until now, we have always manually defined the update function. In most cases this will be the way to go. But you may have noticed that in some cases this is sheer boilerplate. We can let the compiler generate the update function for us by using the `mkUpdateAuto` function. This works if the following conditions are met:

- There are no conditional transitions in the state machine.
- State transitions don't change the type of the state payload.

Both conditions are met in our case, so we can use `mkUpdateAuto` to generate the update function for us. We could have used it in the Door example and the Bridges of Königsberg example as well.

<!-- PD_START:purs { filePath: test/Examples/HouseSantaClaus.purs, pick: [update] } -->

```purescript
update :: State -> Msg -> State
update =
  mkUpdateAuto @SantaTransit
```

[test/Examples/HouseSantaClaus.purs (lines 57-59)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L57-L59){.fileLink}


<!-- PD_END -->

## Testing the state machine

At the beginning of this chapter, we saw an image of one possible solution to the puzzle. Let's write a test to verify that the update function follows this solution:

<!-- PD_START:purs { filePath: test/Examples/HouseSantaClaus.purs, pick: [specWalk] } -->

```purescript
specWalk :: Spec Unit
specWalk =
  it "should follow the walk and visit the expected intermediate states" do
    assertWalk update
      (v @"1")
      [ v @"f" ~> v @"3"
      , v @"h" ~> v @"4"
      , v @"g" ~> v @"2"
      , v @"a" ~> v @"1"
      , v @"e" ~> v @"4"
      , v @"d" ~> v @"5"
      , v @"c" ~> v @"3"
      , v @"b" ~> v @"2"
      ]
```

[test/Examples/HouseSantaClaus.purs (lines 68-81)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L68-L81){.fileLink}


<!-- PD_END -->

Since this test passes, we know that the state machine has an Eulerian trail. We can also assert that with the `hasEulerTrail` function we defined earlier:

<!-- PD_START:purs { filePath: test/Examples/HouseSantaClaus.purs, pick: [specEulerTrail] } -->

```purescript
specEulerTrail :: Spec Unit
specEulerTrail =
  it "should have an Eulerian trail" do
    let
      graph :: StateGraph
      graph = mkStateGraph santaTransit

    hasEulerTrail graph `shouldEqual` true
```

[test/Examples/HouseSantaClaus.purs (lines 83-90)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L83-L90){.fileLink}


<!-- PD_END -->

## Generating documentation

Until now we always used automatic layouts for the state diagram. This is super convenient because you don't have to worry about the layout at all. Sometimes you want more control over the layout. Luckily we can also position the nodes manually by using the `Manual` layout. We'll do this here to make our state diagram look exactly like the drawing of the house of Santa Claus.

Let's do a quick sketch in a 2D grid. The nodes are positioned at the following coordinates:

![House of Santa Claus layout positions](assets/house-santa-claus_layout-positions.svg){width=30%}

For historical reasons, the Graphviz renderer wants positions to be defined in inches. We want to use 0.6 inches as the base unit and have all positions be a multiple of that. This number is an arbitrary choice and you can adjust it to increase or decrease the size of the graph.

<!-- PD_START:purs { filePath: test/Examples/HouseSantaClaus.purs, pick: [baseUnit] } -->

```purescript
baseUnit :: Inch
baseUnit = Inch 0.6
```

[test/Examples/HouseSantaClaus.purs (lines 102-103)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L102-L103){.fileLink}


<!-- PD_END -->

Since we'll need vectors of inches to position the nodes, we define a helper function to create them as multiples of the base unit.

<!-- PD_START:purs { filePath: test/Examples/HouseSantaClaus.purs, pick: [units2D] } -->

```purescript
units2D :: Int -> Int -> { x :: Inch, y :: Inch }
units2D x y =
  { x: Inch (Int.toNumber x * unwrap baseUnit)
  , y: Inch (Int.toNumber y * unwrap baseUnit)
  }
```

[test/Examples/HouseSantaClaus.purs (lines 105-109)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L105-L109){.fileLink}


<!-- PD_END -->

Finally we can generate the graph with the `generateGraphDark` function and define the nodes positions in the options of the `Manual` layout. The `exact` boolean means that the position is exact and not meant as just a hint for the layout algorithm.

<!-- PD_START:purs { filePath: test/Examples/HouseSantaClaus.purs, pick: [generateGraphDark] } -->

```purescript
generateGraphDark :: Effect Unit
generateGraphDark = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate santaTransit _
      { undirectedEdges = true
      , theme = themeHarmonyDark
      , layout = Manual
          [ { node: "1", pos: units2D 0 0, exact: true }
          , { node: "2", pos: units2D 2 0, exact: true }
          , { node: "3", pos: units2D 2 2, exact: true }
          , { node: "4", pos: units2D 0 2, exact: true }
          , { node: "5", pos: units2D 1 4, exact: true }
          ]
      , fixedNodeSize = pure $ units2D 1 1
      , fontSize = 14.0
      }

  FS.writeTextFile UTF8
    "renders/house-santa-claus_graph-dark.dot"
    (Graphviz.toDotStr graph)
```

[test/Examples/HouseSantaClaus.purs (lines 133-153)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L133-L153){.fileLink}


<!-- PD_END -->

**Generated Output**: This generates the diagram we saw at the beginning of this example.

🔗 <a href="https://dreampuf.github.io/GraphvizOnline/?url=https://m-bock.github.io/purescript-transit/renders/house-santa-claus_graph-dark.dot" target="_blank">View diagram on GraphvizOnline</a>

Note that the size of the nodes is also fixed to a multiple of the base unit. In this way we can better control the position in the grid we drew at the beginning.

## Conclusion

This example demonstrated several advanced features of **Transit**:

- **Automatic update function generation**: When your state machine has no conditional transitions and preserves state payload types, you can use `mkUpdateAuto` to let the compiler generate the update function for you, eliminating boilerplate code.

- **Manual layout control**: Unlike the previous examples that used automatic layouts, we showed how to precisely position nodes using the `Manual` layout option, giving you complete control over the visual representation of your state machine.

- **Graph analysis verification**: We verified that this graph has an Eulerian trail using the same `hasEulerTrail` function from the previous example, demonstrating how **Transit**'s graph analysis capabilities work consistently across different state machines.

Together with the previous examples, we've seen how **Transit** provides a comprehensive solution for building type-safe state machines, generating documentation, and performing graph analysis — all from a single type-level specification.
