# Example 4: House of Santa Claus [unfinished]

> Full source code: _[test/Examples/HouseOfSantaClaus.purs](test/Examples/HouseOfSantaClaus.purs)_

<img src="assets/house-santa-claus-solution.webp" />

Do you remember the puzzle where you try to draw the house of Santa Claus in one continuous line - without lifting your pen and without retracing any line?
And while doing that you where ought to say out loud the 8 syllables "This-Is-The-House-Of-San-Ta-Claus" - one syllable for each line.

## The State Machine

Of course we can model this puzzle as a state machine. We have states (1 through 5) and 8 transitions (a through h). Again, this is an undirected graph:

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="renders/house-santa-claus_graph-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="renders/house-santa-claus_graph-light.svg">
  <img class="state-diagram" alt="House of Santa Claus graph" src="renders/house-santa-claus_graph-light.svg">
</picture>

Accordingly, the transition table looks like this:

<!-- PD_START:raw
filePath: renders/house-santa-claus_table.md
wrapNl: true
-->

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

## Transit Approach

### State and message types

Nothing special here. We define the state and message types like we did in the previous examples. By using Variants we have no constraint on how the labels are named, we can use any type level string we want: numbers, lower case letters, etc. Traditional ADT wouldn't give us this flexibility.

<!-- PD_START:purs
filePath: test/Examples/HouseSantaClaus.purs
pick:
  - State
  - Msg
-->

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

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L27-L44">test/Examples/HouseSantaClaus.purs L27-L44</a>
  </sup>
</p>

<!-- PD_END -->

### Type level specification

The transit specification follows the same pattern as in the previous example.

<!-- PD_START:purs
filePath: test/Examples/HouseSantaClaus.purs
pick:
  - SantaTransit
-->

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

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L46-L55">test/Examples/HouseSantaClaus.purs L46-L55</a>
  </sup>
</p>

<!-- PD_END -->

### Update function

Until now we always manually defined the update function. In most cases this will be the way to go. But you may have noticed that in some cases this is sheer boilerplate. We can let the compiler generate the update function for us by using the `mkUpdateAuto` function. This works if the following conditions are met:

- There are no conditional transitions in the state machine.
- State transitions don't change the type of the state payload.

Both conditions are met in our case, so we can use `mkUpdateAuto` to generate the update function for us. We could have used it in the Door example and the Bridges of Konigsberg example as well.

<!-- PD_START:purs
filePath: test/Examples/HouseSantaClaus.purs
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update =
  mkUpdateAuto @SantaTransit
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L57-L59">test/Examples/HouseSantaClaus.purs L57-L59</a>
  </sup>
</p>

<!-- PD_END -->

## Testing the state machine

In the beginning of the chapter we've already seen an image of one possible solution to the puzzle. Let's write a test to verify that the update function follows this solution:

<!-- PD_START:purs
filePath: test/Examples/HouseSantaClaus.purs
pick:
  - specWalk
-->

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

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L68-L81">test/Examples/HouseSantaClaus.purs L68-L81</a>
  </sup>
</p>

<!-- PD_END -->

Since this test passes, we know that the state machine has an Eulerian trail. We can also assert that with the `hasEulerTrail` function we defined earlier:

<!-- PD_START:purs
filePath: test/Examples/HouseSantaClaus.purs
pick:
  - specEulerTrail
-->

```purescript
specEulerTrail :: Spec Unit
specEulerTrail =
  it "should have an Eulerian trail" do
    let
      graph :: StateGraph
      graph = mkStateGraph santaTransit

    hasEulerTrail graph `shouldEqual` true
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L83-L90">test/Examples/HouseSantaClaus.purs L83-L90</a>
  </sup>
</p>

<!-- PD_END -->

## Generating documentation

Until now we always used automatic layouts for the state diagram. This is uper convenient because you don't have to worry about the layout at all. Sometimes you want more control over the layout. Luckily we can also position the nodes manually by using the `Manual` layout. We''l do this here to make our state diagram look exaclty like the drawing of the house of Santa Claus.

Let's do a quick sketch in a 2D grid. The nodes are positioned at the following coordinates:

<img src="assets/house-santa-claus_layout-positions.svg" />

For historical reasons the Graphviz renderer want positions being defined in inches. We want to use 0.6 inches as the base unit and have all positions be a multiple of that. This number is an arbitrary choice and you can adjust it to increase or decrease the size of the graph.

<!-- PD_START:purs
filePath: test/Examples/HouseSantaClaus.purs
pick:
  - baseUnit
-->

```purescript
baseUnit :: Inch
baseUnit = Inch 0.6
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L126-L127">test/Examples/HouseSantaClaus.purs L126-L127</a>
  </sup>
</p>

<!-- PD_END -->

Since well need vectors of inches to position the nodes, we define a helper function to create them as multiple of the base unit.

<!-- PD_START:purs
filePath: test/Examples/HouseSantaClaus.purs
pick:
  - units2D
-->

```purescript
units2D :: Int -> Int -> Vec2D Inch
units2D x y = Vec2D
  { x: Inch (Int.toNumber x * unwrap baseUnit)
  , y: Inch (Int.toNumber y * unwrap baseUnit)
  }
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L129-L133">test/Examples/HouseSantaClaus.purs L129-L133</a>
  </sup>
</p>

<!-- PD_END -->

Finally we can generate the graph with the `generateGraphDark` function and define the nodes positions in the options of the `Manual` layout. The `exact` boolean means that the position is exact and not meant as just a hint for the layout algorithm.

<!-- PD_START:purs
filePath: test/Examples/HouseSantaClaus.purs
pick:
  - generateGraphDark
-->

```purescript
units2D :: Int -> Int -> Vec2D Inch
units2D x y = Vec2D
  { x: Inch (Int.toNumber x * unwrap baseUnit)
  , y: Inch (Int.toNumber y * unwrap baseUnit)
  }
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L129-L133">test/Examples/HouseSantaClaus.purs L129-L133</a>
  </sup>
</p>

<!-- PD_END -->

Not that also the size of the nodes is fixed to a multiple of the base unit. In this way we can better control the position in the grid we draw in the beginning.

## Conclusion

[todo]
