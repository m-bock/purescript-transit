# Example 4: House of Santa Claus [unfinished]

[todo]

> Full source code: _[test/Examples/HouseOfSantaClaus.purs](test/Examples/HouseOfSantaClaus.purs)_

<img src="assets/house-santa-claus-solution.webp" />

## The State Machine

[todo]

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="renders/house-santa-claus_graph-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="renders/house-santa-claus_graph-light.svg">
  <img class="state-diagram" alt="House of Santa Claus graph" src="renders/house-santa-claus_graph-light.svg">
</picture>

[todo]

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

[todo]

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
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L25-L42">test/Examples/HouseSantaClaus.purs L25-L42</a>
  </sup>
</p>

<!-- PD_END -->

### Type level specification

[todo]

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
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L44-L53">test/Examples/HouseSantaClaus.purs L44-L53</a>
  </sup>
</p>

<!-- PD_END -->

### Update function

[todo]

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
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L55-L57">test/Examples/HouseSantaClaus.purs L55-L57</a>
  </sup>
</p>

<!-- PD_END -->

## Testing the state machine

[todo]

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
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L66-L79">test/Examples/HouseSantaClaus.purs L66-L79</a>
  </sup>
</p>

<!-- PD_END -->

[todo]

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
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L81-L88">test/Examples/HouseSantaClaus.purs L81-L88</a>
  </sup>
</p>

<!-- PD_END -->

## Generating documentation

[todo]

<img src="assets/house-santa-claus_layout-positions.svg" />

[todo]

<!-- PD_START:purs
filePath: test/Examples/HouseSantaClaus.purs
pick:
  - generateGraphDark
-->

```purescript
generateGraphDark :: Effect Unit
generateGraphDark = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate santaTransit _
      { useUndirectedEdges = true
      , theme = themeHarmonyDark
      , layout = TransitGraphviz.Manual
          [ pos 0 0 "1" # exact
          , pos 2 0 "2" # exact
          , pos 2 2 "3" # exact
          , pos 0 2 "4" # exact
          , pos 1 4 "5" # exact
          ]
      , fixedNodeSize = pure $ nodeSize 1 1
      , fontSize = 16.0
      }

  FS.writeTextFile UTF8
    "renders/house-santa-claus_graph-dark.dot"
    (Graphviz.toDotStr graph)
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/HouseSantaClaus.purs#L122-L142">test/Examples/HouseSantaClaus.purs L122-L142</a>
  </sup>
</p>

<!-- PD_END -->

## Conclusion

[todo]
