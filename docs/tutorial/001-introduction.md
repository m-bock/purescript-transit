# Introduction

**Transit** is a PureScript library for building type-safe state machines. It provides a type-level DSL for specifying state transitions. You define your state machine once using this specification, and the compiler ensures your implementation matches it — eliminating bugs from invalid transitions, missing cases, or documentation drift.

This tutorial will guide you through the basics of **Transit** by showing its main features based on examples. We'll start with a simple door state machine and gradually add more complexity.

**About This Documentation**

All code examples in this documentation are extracted from actual, type-checked PureScript source files. Also, whenever you find an assertion or a full unit test, we ensure that it ran and passed. In this sense, this text is not just documentation, but also a test suite. At the bottom of every code example you can find a link to the actual source file, so you can get a better picture of the context and see information about the imports used.

**Similarities to Haskell's Servant library**

If you're familiar with Servant[^servant] from Haskell, **Transit** follows a similar philosophy: just as Servant uses a REST API type-level specification to ensure type-safe routing functions and generate OpenAPI documentation, **Transit** uses a state machine graph type-level specification to ensure type-safe update functions and generate state diagrams.

[^servant]: [Servant](https://haskell-servant.readthedocs.io/) is a Haskell library for building type-safe web APIs.
