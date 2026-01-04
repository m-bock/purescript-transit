# Example 3: Bridges of Königsberg

> Full source code: _[test/Examples/BridgesKoenigsberg.purs](test/Examples/BridgesKoenigsberg.purs)_

<img src="assets/bridges-koenigsberg.png" width="450" />

So far, we've seen how **Transit** helps you build type-safe state machines and generate state diagrams and transition tables. But the power of **Transit** extends far beyond documentation generation. The reflected data structure — the term-level representation of your type-level DSL specification — can be converted into a general-purpose graph data structure, enabling sophisticated graph analysis.

This example demonstrates this capability using the famous Seven Bridges of Königsberg problem. In 1736, the mathematician Leonhard Euler was asked whether it was possible to walk through the city of Königsberg crossing each of its seven bridges exactly once.[^koenigsberg]

In the picture above you see the topography of the historic city of Königsberg. The city is divided into four land areas (A, B, C, and D) and seven bridges (a, b, c, d, e, f, and g) connect them.

## State diagram

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
