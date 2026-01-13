# Example 3: Bridges of Königsberg

Full source code: _[test/Examples/BridgesKoenigsberg.purs](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs)_

![Map of Königsberg](assets/bridges-koenigsberg.png){width=450}

So far, we've seen how **Transit** helps you build type-safe state machines and generate state diagrams and transition tables. But the power of **Transit** extends far beyond documentation generation. The reflected data structure — the term-level representation of your type-level DSL specification — can be converted into a general-purpose graph data structure, enabling sophisticated graph analysis.

This example demonstrates this capability using the famous Seven Bridges of Königsberg problem. In 1736, the mathematician Leonhard Euler was asked whether it was possible to walk through the city of Königsberg crossing each of its seven bridges exactly once.[^koenigsberg]

In the picture above you see the topography of the historic city of Königsberg. The city is divided into four land areas (A, B, C, and D) and seven bridges (a, b, c, d, e, f, and g) connect them.

## The State Machine

While not immediately obvious, the map of the city can be represented as a graph:

- **Nodes** represent the four land areas
- **Edges** represent the seven bridges connecting them

![Seven Bridges of Königsberg state diagram](renders/bridges-koenigsberg_graph-dark.svg){.dark-light}

Note that we drew an undirected graph here. This is due to the fact that the bridges are bidirectional. We could also have drawn a directed graph with two edges for each bridge, but this would look more cluttered.

The same is true for the transition table. Instead of two rows for each bridge, we have one row for each bridge:

<!-- PD_START:raw { filePath: renders/bridges-koenigsberg_table.md, wrapNl: true } -->

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

## Implementation using Transit

### State and message types

The state machine represents the four land areas as states (uppercase letters `A`, `B`, `C`, and `D`) and the seven bridges as messages (lowercase letters `a` through `g`). Each message represents crossing a specific bridge, which transitions between the corresponding land areas.

<!-- PD_START:purs { filePath: test/Examples/BridgesKoenigsberg.purs, pick: [State, Msg] } -->

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

[test/Examples/BridgesKoenigsberg.purs (lines 25-40)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L25-L40){.fileLink}

<!-- PD_END -->

### Type-level specification

Since bridges can be crossed in both directions, each bridge creates a bidirectional connection between two land areas. In the type-level specification, we define transitions using the syntax `"State1" |< "Message" >| "State2"`, which effectively defines two transitions: one from `State1` to `State2` and one from `State2` to `State1`.

<!-- PD_START:purs { filePath: test/Examples/BridgesKoenigsberg.purs, pick: [BridgesTransit] } -->

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

[test/Examples/BridgesKoenigsberg.purs (lines 42-50)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L42-L50){.fileLink}

<!-- PD_END -->

### The Update Function

However, in the update function we need to explicitly handle both directions of each bridge as shown below.

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
maxLines: 10
pick: ["update"]
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

[test/Examples/BridgesKoenigsberg.purs (lines 52-73)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L52-L73){.fileLink}

<!-- PD_END -->

## Testing the update function

<img src="assets/bridges-koenigsberg-walk.svg" width="450" />

The picture shows one randomly chosen walk through the city of Königsberg. Unfortunately, it does not visit all bridges exactly once as required by Euler. The red circle indicates where bridge `g` is crossed twice. Let's test the walk anyway before we move on.

<!-- PD_START:purs { filePath: test/Examples/BridgesKoenigsberg.purs, pick: [specSampleWalk] } -->

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

[test/Examples/BridgesKoenigsberg.purs (lines 82-95)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L82-L95){.fileLink}

<!-- PD_END -->

We could try many other walks the same way. But — spoiler alert — none of them will visit all bridges exactly once. And in the next section we'll see a way to prove this for our state machine.

## Graph Analysis

The real power of **Transit** becomes apparent when we convert the reflected data structure into a general-purpose graph. Using `mkStateGraph`, we transform the **Transit** specification into a `StateGraph` — a specialized `Graph` type configured with edge and node labels suitable for state machine analysis.

<!-- PD_START:purs { filePath: test/Examples/BridgesKoenigsberg.purs, pick: [bridgesGraph] } -->

```purescript
bridgesGraph :: StateGraph
bridgesGraph = mkStateGraph bridgesTransit
```

[test/Examples/BridgesKoenigsberg.purs (lines 97-98)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L97-L98){.fileLink}

<!-- PD_END -->

### Eulerian trail

Once we have this graph data structure, we can perform sophisticated analysis using standard graph algorithms. For the Seven Bridges problem, we want to determine if the graph has an **Eulerian trail**: A path that visits every edge exactly once but doesn't necessarily return to the start.

Let's assume our trail would start and end at the same node. Is there some property that must hold for each node? As we know, a bridge can only be crossed once, so we can conclude that whenever we visit a piece of land via a bridge we must leave it via a _different_ bridge again. That means that the number of bridges connected to each land must be 2, or 4, or 6, or 2000, ... in other words, an even number.

However, our trail does not have to start and end at the same piece of land. If this is the case, the start and end nodes can have an odd number of bridges connected to them. This is because we never enter the start node, and we never leave the end node.

This is what Euler formalized in his theorem: An undirected graph has an Eulerian trail if and only if it _is connected_ and has exactly _zero or two_ vertices of odd degree.

### Degree of a node

For simplicity we'll assume that our graphs are always connected. The degree of a node is the number of edges connected to it. Since our graph is undirected, we can obtain the degree of a node by counting the number of outgoing edges:

<!-- PD_START:purs { filePath: test/Examples/Common.purs, pick: [nodeDegree] } -->

```purescript
nodeDegree :: StateGraph -> StateNode -> Int
nodeDegree graph node = Set.size (Graph.getOutgoingEdges node graph)
```

[test/Examples/Common.purs (lines 21-22)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Common.purs#L21-L22){.fileLink}

<!-- PD_END -->

And we can easily see that all of our nodes have odd degree:

<!-- PD_START:purs { filePath: test/Examples/BridgesKoenigsberg.purs, pick: [specNodeDegree] } -->

```purescript
specNodeDegree :: Spec Unit
specNodeDegree = do
  it "should each node have the expected degree" do
    nodeDegree bridgesGraph "A" `shouldEqual` 5
    nodeDegree bridgesGraph "B" `shouldEqual` 3
    nodeDegree bridgesGraph "C" `shouldEqual` 3
    nodeDegree bridgesGraph "D" `shouldEqual` 3
```

[test/Examples/BridgesKoenigsberg.purs (lines 100-106)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L100-L106){.fileLink}

<!-- PD_END -->

And this already shows that our graph does _not_ have an Eulerian trail.

### Checking for Eulerian trail

But we want to create a function that can tell this for any state graph. Let's do that now, we'll use this function in the next example again:

<!-- PD_START:purs { filePath: test/Examples/Common.purs, pick: [hasEulerTrail] } -->

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

[test/Examples/Common.purs (lines 24-36)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/Common.purs#L24-L36){.fileLink}

<!-- PD_END -->

The implementation is pretty straightforward. We get all nodes, count the number of edges connected to each node, filter the odd ones and count them. If the count is 2 or 0, we have an Eulerian trail, otherwise we don't. We can use this function to test our graph:

<!-- PD_START:purs { filePath: test/Examples/BridgesKoenigsberg.purs, pick: [specEulerTrail] } -->

```purescript
specEulerTrail :: Spec Unit
specEulerTrail = do
  it "should not have an Eulerian trail" do
    hasEulerTrail bridgesGraph `shouldEqual` false
```

[test/Examples/BridgesKoenigsberg.purs (lines 108-111)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L108-L111){.fileLink}

<!-- PD_END -->

### Graph analysis with the classic state machine approach

If we wanted to perform similar analysis with the classic state machine approach, we would need to generate all possible 5040 walks and empirically check if any of them visit all bridges exactly once.
This is due to the fact that the specification of state machine transitions is buried inside the update function.

## Generating Documentation

For generating the state diagram we add some more options to the `generate` function:

- `undirectedEdges`: The state diagram will be displayed as an undirected graph.
- `fontSize`: Since our edges and labels consist of only single characters, we can make their font size a bit larger.

<!-- PD_START:purs { filePath: test/Examples/BridgesKoenigsberg.purs, pick: [generateGraphLight] } -->

```purescript
generateGraphLight :: Effect Unit
generateGraphLight = do
  let
    graph :: GraphvizGraph
    graph = TransitGraphviz.generate bridgesTransit _
      { theme = themeHarmonyLight
      , undirectedEdges = true
      , fontSize = 14.0
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg_graph-light.dot" (Graphviz.toDotStr graph)
```

[test/Examples/BridgesKoenigsberg.purs (lines 124-133)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L124-L133){.fileLink}

<!-- PD_END -->

**Generated Output**: This produces the graph visualization we examined earlier in this example.

🔗 <a href="https://dreampuf.github.io/GraphvizOnline/?url=https://m-bock.github.io/purescript-transit/renders/bridges-koenigsberg_graph-light.dot" target="_blank">View diagram on GraphvizOnline</a>

The transition table is generated the same way as before, but we also add the `undirectedEdges` option to the options:

<!-- PD_START:purs { filePath: test/Examples/BridgesKoenigsberg.purs, pick: [generateTable] } -->

```purescript
generateTable :: Effect Unit
generateTable = do
  let
    table :: Table
    table = TransitTable.generate bridgesTransit _
      { undirectedEdges = true
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg_table.md" (Table.toMarkdown table)
```

[test/Examples/BridgesKoenigsberg.purs (lines 146-153)](https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L146-L153){.fileLink}

<!-- PD_END -->

**Generated Output**: This creates the transition table in the undirected format, showing all possible bridge crossings between the land areas.

## Conclusion

This example demonstrates that **Transit**'s value extends far beyond state machine documentation. By reflecting the type-level specification to a term-level graph data structure, you gain access to a rich ecosystem of third-party graph algorithms and analysis tools.

In the next example, we'll see a graph that **does** have an Eulerian trail.
