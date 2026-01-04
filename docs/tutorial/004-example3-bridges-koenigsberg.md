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
| LandA | **⟵** | Cross_a | **⟶** | LandB |
| LandA | **⟵** | Cross_b | **⟶** | LandB |
| LandA | **⟵** | Cross_c | **⟶** | LandC |
| LandA | **⟵** | Cross_d | **⟶** | LandC |
| LandA | **⟵** | Cross_e | **⟶** | LandD |
| LandB | **⟵** | Cross_f | **⟶** | LandD |
| LandC | **⟵** | Cross_g | **⟶** | LandD |

<!-- PD_END -->

[^koenigsberg]: The [Seven Bridges of Königsberg](https://en.wikipedia.org/wiki/Seven_Bridges_of_K%C3%B6nigsberg) problem was solved by Leonhard Euler (1707–1783), a Swiss mathematician, physicist, and engineer who made fundamental contributions to mathematics and physics. His work on this problem is considered the foundation of graph theory.

## Transit Approach

### State and message types

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - State
  - Msg
-->

```purescript
type State = Variant
  ( "LandA" :: {}
  , "LandB" :: {}
  , "LandC" :: {}
  , "LandD" :: {}
  )

type Msg = Variant
  ( "Cross_a" :: {}
  , "Cross_b" :: {}
  , "Cross_c" :: {}
  , "Cross_d" :: {}
  , "Cross_e" :: {}
  , "Cross_f" :: {}
  , "Cross_g" :: {}
  )
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L23-L38">test/Examples/BridgesKoenigsberg.purs L23-L38</a>
  </sup>
</p>

<!-- PD_END -->

### Type level specification

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - BridgesKoenigsbergTransit
-->

```purescript
type BridgesKoenigsbergTransit =
  Transit
    :* ("LandA" |< "Cross_a" >| "LandB")
    :* ("LandA" |< "Cross_b" >| "LandB")
    :* ("LandA" |< "Cross_c" >| "LandC")
    :* ("LandA" |< "Cross_d" >| "LandC")
    :* ("LandA" |< "Cross_e" >| "LandD")
    :* ("LandB" |< "Cross_f" >| "LandD")
    :* ("LandC" |< "Cross_g" >| "LandD")
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L40-L48">test/Examples/BridgesKoenigsberg.purs L40-L48</a>
  </sup>
</p>

<!-- PD_END -->

### Update function

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
maxLines: 10
pick:
  - update
-->

```purescript
update :: State -> Msg -> State
update = mkUpdate @BridgesKoenigsbergTransit
  (match @"LandA" @"Cross_a" \_ _ -> return @"LandB")
  (match @"LandB" @"Cross_a" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_b" \_ _ -> return @"LandB")
  (match @"LandB" @"Cross_b" \_ _ -> return @"LandA")

  (match @"LandA" @"Cross_c" \_ _ -> return @"LandC")
  (match @"LandC" @"Cross_c" \_ _ -> return @"LandA")

-- And so on ... (13 lines omitted)
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L50-L71">test/Examples/BridgesKoenigsberg.purs L50-L71</a>
  </sup>
</p>

<!-- PD_END -->

## Testing the update function

<img src="assets/bridges-koenigsberg-walk.svg" width="450" />

<!-- PD_START:purs
filePath: test/Examples/BridgesKoenigsberg.purs
pick:
  - assert1
-->

```purescript
assert1 :: Spec Unit
assert1 =
  it "should follow the walk and visit the expected intermediate states" do
    assertWalk update
      (v @"LandA")
      [ v @"Cross_a" ~> v @"LandB"
      , v @"Cross_f" ~> v @"LandD"
      , v @"Cross_g" ~> v @"LandC"
      , v @"Cross_c" ~> v @"LandA"
      , v @"Cross_e" ~> v @"LandD"
      , v @"Cross_g" ~> v @"LandC"
      , v @"Cross_d" ~> v @"LandA"
      , v @"Cross_b" ~> v @"LandB"
      ]
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L80-L93">test/Examples/BridgesKoenigsberg.purs L80-L93</a>
  </sup>
</p>

<!-- PD_END -->

## Graph Analysis

The real power of **Transit** becomes apparent when we convert the reflected data structure into a general-purpose graph. Using `mkStateGraph`, we transform the **Transit** specification into a `StateGraph` — a specialized `Graph` type configured with edge and node labels suitable for state machine analysis.

Once we have this graph data structure, we can perform sophisticated analysis using standard graph algorithms. For the Seven Bridges problem, we want to determine if the graph has an **Eulerian circuit** (a path that visits every edge exactly once and returns to the starting point) or an **Eulerian trail** (a path that visits every edge exactly once but doesn't necessarily return to the start).

Euler's theorem[^euler-theorem] states that:

- An undirected graph has an Eulerian trail if and only if it is connected and has exactly zero or two vertices of odd degree

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

To perform the analysis, we convert the reflected **Transit** specification into a graph and then check its properties:

The key steps are:

1. **Reflect the type-level specification**: `reflectType (Proxy @BridgesKoenigsbergTransit)` converts the type-level DSL to a term-level representation
2. **Convert to a graph**: `mkStateGraph transit` transforms the **Transit** specification into a `StateGraph` — a general-purpose graph data structure
3. **Perform analysis**: Use graph analysis functions like `hasEulerCircle` and `hasEulerTrail` to check properties

This confirms Euler's original conclusion: it's impossible to walk through Königsberg crossing each bridge exactly once.

## Generating Documentation

To generate a state diagram we'll use the following function:

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
    graph = TransitGraphviz.generate bridgesKoenigsbergTransit _
      { theme = themeHarmonyLight
      , useUndirectedEdges = true
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg-light.dot" (Graphviz.toDotStr graph)
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L128-L136">test/Examples/BridgesKoenigsberg.purs L128-L136</a>
  </sup>
</p>

<!-- PD_END -->

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
    table = TransitTable.generate bridgesKoenigsbergTransit _
      { useUndirectedEdges = true
      }
  FS.writeTextFile UTF8 "renders/bridges-koenigsberg.md" (Table.toMarkdown table)
```

<p align="right">
  <sup
    >🗎
    <a href="https://github.com/m-bock/purescript-transit/blob/main/test/Examples/BridgesKoenigsberg.purs#L148-L155">test/Examples/BridgesKoenigsberg.purs L148-L155</a>
  </sup>
</p>

<!-- PD_END -->

## Conclusion

This example demonstrates that **Transit**'s value extends far beyond state machine documentation. By reflecting the type-level specification to a term-level graph data structure, you gain access to a rich ecosystem of third-party graph algorithms and analysis tools.

In the next example, we'll see a graph that **does** have an Eulerian trail.
