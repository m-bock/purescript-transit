# Example 3: Bridges of Königsberg [unfinished]

> Full source code: _[test/Examples/BridgesKoenigsberg.purs](test/Examples/BridgesKoenigsberg.purs)_

<img src="assets/bridges-koenigsberg.png" width="450" />

So far, we've seen how **Transit** helps you build type-safe state machines and generate state diagrams and transition tables. But the power of **Transit** extends far beyond documentation generation. The reflected data structure — the term-level representation of your type-level DSL specification — can be converted into a general-purpose graph data structure, enabling sophisticated graph analysis.

This example demonstrates this capability using the famous Seven Bridges of Königsberg problem. In 1736, the mathematician Leonhard Euler was asked whether it was possible to walk through the city of Königsberg crossing each of its seven bridges exactly once.[^koenigsberg]

In the picture above you see the topography of the historic city of Königsberg. The city is divided into four land areas (A, B, C, and D) and seven bridges (a, b, c, d, e, f, and g) connect them.

## The State Machine

Even not immediately obvious, the map of the city can be represented as a graph:

- **Nodes** represent the four land areas
- **Edges** represent the seven bridges connecting them

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="renders/bridges-koenigsberg-dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="renders/bridges-koenigsberg-light.svg">
  <img alt="Seven Bridges of Königsberg graph" src="renders/bridges-koenigsberg-light.svg">
</picture>

Note that we drew an undirected graph here. This is due to the fact that the bridges are bidirectional. We could also have drawn a directed graph with two edges for each bridge, but this would look more cluttered.

The same is true for the transition table. Instead of two rows for each bridge, we have one row for each bridge:

<!-- PD_START:raw
filePath: renders/bridges-koenigsberg.md
wrapNl: true
-->
| State |       | Message |       | State |
| ----- | ----- | ------- | ----- | ----- |
| A     | **⟵** | a       | **⟶** | B     |
| A     | **⟵** | b       | **⟶** | B     |
| A     | **⟵** | c       | **⟶** | C     |
| A     | **⟵** | d       | **⟶** | C     |
| A     | **⟵** | e       | **⟶** | D     |
| B     | **⟵** | f       | **⟶** | D     |
| C     | **⟵** | g       | **⟶** | D     |

<!-- PD_END -->

[^koenigsberg]: The [Seven Bridges of Königsberg](https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg) problem was solved by Leonhard Euler (1707–1783), a Swiss mathematician, physicist, and engineer who made fundamental contributions to mathematics and physics. His work on this problem is considered the foundation of graph theory.

## Transit Approach

### State and message types

The state machine represents the four land areas as states (uppercase letters `A`, `B`, `C`, and `D`) and the seven bridges as messages (lowercase letters `a` through `g`). Each message represents crossing a specific bridge, which transitions between the corresponding land areas.

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - State
  - Msg
-->

```purescript
type State = Variant
  ( "A" :: {}
  , "B" :: {}
  , "C" :: {}
  , "D" :: {}
  )

type Msg = Variant
  ( "a" :: {}
  , "b" :: {}
  , "c" :: {}
  , "d" :: {}
  , "e" :: {}
  , "f" :: {}
  , "g" :: {}
  )
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L25-L40">test/Examples/BridgesKoenigsberg.purs L25-L40</a>
  </sup>
</p>

<!-- PD_END -->

### Type level specification

Since bridges can be crossed in both directions, each bridge creates a bidirectional connection between two land areas. In the type-level specification, we define transitions using the syntax `"State1" |< "Message" >| "State2"`, which effectively defines two transitions: one from `State1` to `State2` and one from `State2` to `State1`.

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - BridgesTransit
-->

```purescript
type BridgesTransit =
  Transit
    :* ("A" |< "a" >| "B")
    :* ("A" |< "b" >| "B")
    :* ("A" |< "c" >| "C")
    :* ("A" |< "d" >| "C")
    :* ("A" |< "e" >| "D")
    :* ("B" |< "f" >| "D")
    :* ("C" |< "g" >| "D")
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L42-L50">test/Examples/BridgesKoenigsberg.purs L42-L50</a>
  </sup>
</p>

<!-- PD_END -->

### Update function

However, in the update function we need to explicitly handle both directions of each bridge as shown below.

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
maxLines: 10
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdate @BridgesTransit
  (match @"A" @"a" \_ _ -> return @"B")
  (match @"B" @"a" \_ _ -> return @"A")

  (match @"A" @"b" \_ _ -> return @"B")
  (match @"B" @"b" \_ _ -> return @"A")

  (match @"A" @"c" \_ _ -> return @"C")
  (match @"C" @"c" \_ _ -> return @"A")

-- And so on ... (13 lines omitted)
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L52-L73">test/Examples/BridgesKoenigsberg.purs L52-L73</a>
  </sup>
</p>

<!-- PD_END -->

## Testing the update function

<img src="assets/bridges-koenigsberg-walk.svg" width="450" />

The picture shows one randomly chosen walk through the city of Königsberg. Unfortunately, it does not visit all bridges exactly once as required by Euler. The red circle indicates where bridge `g` is crossed twice. But let's test the walk anyway before we move on.

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - specSampleWalk
-->

```purescript
specSampleWalk :: Spec Unit
specSampleWalk =
  it "should follow the sample walk and visit the expected intermediate states" do
    assertWalk update
      (v @"A")
      [ v @"a" ~> v @"B"
      , v @"f" ~> v @"D"
      , v @"g" ~> v @"C"
      , v @"c" ~> v @"A"
      , v @"e" ~> v @"D"
      , v @"g" ~> v @"C"
      , v @"d" ~> v @"A"
      , v @"b" ~> v @"B"
      ]
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L82-L95">test/Examples/BridgesKoenigsberg.purs L82-L95</a>
  </sup>
</p>

<!-- PD_END -->

We could try many other walks the same way. But - spoiler alert - none of them will visit all bridges exactly once. And in the next section we'll see a way to proof this for our state machine.

## Graph Analysis

The real power of **Transit** becomes apparent when we convert the reflected data structure into a general-purpose graph. Using `mkStateGraph`, we transform the **Transit** specification into a `StateGraph` — a specialized `Graph` type configured with edge and node labels suitable for state machine analysis.

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - bridgesGraph
-->

```purescript
bridgesGraph :: StateGraph
bridgesGraph = mkStateGraph bridgesTransit
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L97-L98">test/Examples/BridgesKoenigsberg.purs L97-L98</a>
  </sup>
</p>

<!-- PD_END -->

Once we have this graph data structure, we can perform sophisticated analysis using standard graph algorithms. For the Seven Bridges problem, we want to determine if the graph has an **Eulerian circuit** (a path that visits every edge exactly once and returns to the starting point) or an **Eulerian trail** (a path that visits every edge exactly once but doesn't necessarily return to the start).

Euler's theorem states that an undirected graph has an Eulerian trail if and only if it _is connected_ and has exactly _zero or two_ vertices of odd degree.

We can check these conditions using helper functions from the `Examples.Common` module:

<!-- PD_START:purs
filePath: test/Examples/Common.purs
pick:
  - nodeDegree
-->

```purescript
nodeDegree :: StateGraph -> StateNode -> Int
nodeDegree graph node = Set.size (Graph.getOutgoingEdges node graph)
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Common.purs#L21-L22">test/Examples/Common.purs L21-L22</a>
  </sup>
</p>

<!-- PD_END -->

[todo]

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - specNodeDegree
-->

```purescript
specNodeDegree :: Spec Unit
specNodeDegree = do
  it "should each node have the expected degree" do
    nodeDegree bridgesGraph "A" `shouldEqual` 5
    nodeDegree bridgesGraph "B" `shouldEqual` 3
    nodeDegree bridgesGraph "C" `shouldEqual` 3
    nodeDegree bridgesGraph "D" `shouldEqual` 3
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L100-L106">test/Examples/BridgesKoenigsberg.purs L100-L106</a>
  </sup>
</p>

<!-- PD_END -->

[todo]

<!-- PD_START:purs
filePath: test/Examples/Common.purs
pick:
  - hasEulerTrail
-->

```purescript
hasEulerTrail :: StateGraph -> Boolean
hasEulerTrail graph =
  let
    nodes :: Array StateNode
    nodes = fromFoldable (Graph.getNodes graph)

    countEdgesByNode :: Array Int
    countEdgesByNode = map (nodeDegree graph) nodes

    sumOddEdges :: Int
    sumOddEdges = Array.length (Array.filter Int.odd countEdgesByNode)
  in
    sumOddEdges == 2 || sumOddEdges == 0
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Common.purs#L24-L36">test/Examples/Common.purs L24-L36</a>
  </sup>
</p>

<!-- PD_END -->

[todo]

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - specEulerTrail
-->

```purescript
specEulerTrail :: Spec Unit
specEulerTrail = do
  it "should not have an Eulerian trail" do
    hasEulerTrail bridgesGraph `shouldEqual` false
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L108-L111">test/Examples/BridgesKoenigsberg.purs L108-L111</a>
  </sup>
</p>

<!-- PD_END -->

I we wanted to perform similar analysis with the classic state machine approach, we would need to generate all possible 5040 walks and empirically check if any of them visit all bridges exactly once.
This is due to the fact that the specification has of state machine transitions is burried inside the update function.

## Generating Documentation

For generating the state diagram we add some more options to the `generate` function:

- `useUndirectedEdges`: The state diagram will be displayed as an undirected graph.

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - generateStateDiagramLight
-->

```purescript
generateStateDiagramLight :: Effect Unit
generateStateDiagramLight = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate bridgesTransit _
      { theme = themeHarmonyLight
      , useUndirectedEdges = true
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg-light.dot" (Graphviz.toDotStr graph)
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L124-L132">test/Examples/BridgesKoenigsberg.purs L124-L132</a>
  </sup>
</p>

<!-- PD_END -->

[todo]

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - generateTransitionTable
-->

```purescript
generateTransitionTable :: Effect Unit
generateTransitionTable = do
  let
    table :: Table
    table = TransitTable.generate bridgesTransit _
      { useUndirectedEdges = true
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg.md" (Table.toMarkdown table)
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L144-L151">test/Examples/BridgesKoenigsberg.purs L144-L151</a>
  </sup>
</p>

<!-- PD_END -->

## Conclusion

This example demonstrates that **Transit**'s value extends far beyond state machine documentation. By reflecting the type-level specification to a term-level graph data structure, you gain access to a rich ecosystem of third-party graph algorithms and analysis tools.

In the next example, we'll see a graph that **does** have an Eulerian trail.
