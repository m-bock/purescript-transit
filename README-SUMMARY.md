# Transit - Summary

## Introduction

- **Transit** is a PureScript library for building type-safe state machines
- Provides a type-level DSL for specifying state transitions
- Compiler ensures implementation matches specification
- Eliminates bugs from invalid transitions, missing cases, or documentation drift

### Key Features

- Type-safe state transitions - Compiler ensures all transitions are valid and complete
- Automatic diagram generation - Generate state diagrams and transition tables directly from specification
- Graph analysis - Convert state machine into graph data structure for advanced analysis
- Performance - Faster runtime and compile time performance compared to classic implementations

### About This Documentation

- All code examples are extracted from actual, type-checked PureScript source files
- Assertions and unit tests are ensured to have run and passed
- Documentation serves as both documentation and test suite
- Links to source files provided at bottom of each code example

### Installation

- Published to Pursuit
- Install with `spago install transit`

## Example 1: A Simple Door

- Demonstrates core concepts of Transit
- Shows how to define state machine using type-level DSL
- Implements type-safe update function
- Generates documentation automatically
- Compares traditional approach with Transit's approach

### The State Machine

- Door can be either open or closed
- When open, can be closed
- When closed, can be opened
- No other actions make sense

#### State Diagram

- Visual representation of all possible states and valid transitions

#### Transition Table

- Tabular view of all state transitions

### State Machine Implementation I: The Classic Approach

- Traditional pattern matching approach
- Manual state and message type definitions
- Manual update function implementation
- No compile-time guarantees

#### States and Message types

- Define data types for states and messages
- Pattern matching for state transitions

#### The update function

- Manual case-by-case implementation
- No type safety for transitions
- Easy to miss cases or make mistakes

### State Machine Implementation II: The Transit Approach

- Type-level specification using Transit DSL
- Compiler-enforced type safety
- Automatic update function generation

#### The Type-Level Specification

- Define state machine using Transit type-level DSL
- Uses operators like `:*`, `:@`, `>|` to specify transitions

#### State and Message Types

- Define as Variant types
- Type-safe representation of states and messages

#### The Update Function

- Generated using `mkUpdate` from type-level specification
- Type-safe and complete by construction

#### How This Solves the Classic Approach's Problems

- Compile-time guarantees
- No missing cases
- No invalid transitions
- Documentation always matches implementation

### Writing Tests for the update function

- Use Variant utilities to create test values
- Test state transitions systematically

#### Creating Variant Values

- Use `v` helper function to create variant values
- Type-safe value construction

#### Testing State Transitions

- Use `assertWalk` to test sequences of transitions
- Verify expected state changes

### Generating Diagrams and Tables

- Use `reflectType` to convert type-level specification to term-level
- Generate documentation from same specification

#### State Diagrams

- Use `TransitGraphviz.writeToFile` to generate Graphviz `.dot` files
- Convert to SVG or PNG using Graphviz command-line tools
- Customizable themes for light/dark modes

#### Transition Tables

- Use `TransitTable.writeToFile` to generate Markdown tables
- Shows all state transitions in tabular format
- Supports guard conditions and undirected edges

### Conclusion

- Transit provides type-safe, maintainable state machine implementation
- Automatic documentation generation
- Better than classic approach in multiple ways

## Example 2: Door with Pin

- Extends simple door example with pin-based locking
- Demonstrates guard conditions using `returnVia`
- Shows conditional state transitions

### State machine implementation I: The Classic Approach

- Manual implementation with pin checking logic
- Pattern matching for different scenarios

#### States and Message types

- Adds `DoorLocked` state with stored pin
- Adds `Lock` and `Unlock` messages

#### The update function

- Manual pin validation logic
- Conditional state transitions based on pin correctness

### State machine implementation II: The Transit Approach

- Type-level specification with guard conditions
- Uses `:?` operator for conditional transitions
- `returnVia` for guard-based state changes

### Conclusion

- Guard conditions enable conditional transitions
- Type-safe way to handle conditional logic

## Example 3: Seven Bridges of Königsberg

- Graph theory problem: can you cross all bridges exactly once?
- Demonstrates graph analysis capabilities
- Shows complex state machine with many states

### Problem Description

- Historical graph theory problem
- Modeling as state machine traversal problem

### State diagram

- Visual representation of the bridge crossing problem

### Transition table

- Tabular view of all possible transitions

### Type level specification

- Complex Transit specification with many states
- Demonstrates scalability of the approach

### State and message types

- Many states representing different bridge crossing configurations
- Messages represent bridge crossings

### Update function

- Generated from type-level specification
- Handles all possible transitions

### A sample walk

- Example traversal through the state machine
- Demonstrates valid path through the problem space

### Graph Analysis

- Convert state machine to graph structure
- Analyze graph properties
- Answer the original problem using graph algorithms

## Example 4: The house of Santa Claus

- Another graph traversal problem
- Demonstrates undirected edges in state machines
- Shows bidirectional transitions

### Problem Description

- Drawing problem: can you draw a house without lifting pen?
- Modeled as graph traversal with undirected edges

### Type level specification

- Transit specification with undirected edge support
- Uses `useUndirectedEdges` option

### A sample walk

- Example path through the drawing
- Shows valid traversal

### Graph Analysis

- Graph-based analysis of the problem
- Determines if solution exists

### Conclusion

- Undirected edges useful for bidirectional transitions
- Graph analysis powerful tool for state machine problems

## Benchmarks

- Performance comparisons between Transit and classic approaches
- Both compile-time and runtime benchmarks

### Compile-time benchmarks

- Measures compilation time for different state machine sizes
- Shows Transit's compile-time performance

### JS Compiler Backend

- Runtime performance on JavaScript backend
- Comparison of update function performance

### ES Compiler Backend

- Runtime performance on ES backend
- Shows performance characteristics

## More

### Monadic update functions

- Support for monadic contexts in update functions
- Use `Effect`, `Aff`, `Maybe`, etc. in state transitions
- Type-safe monadic state machines

### Error handling

- Error handling patterns in state machines
- Using `Maybe` or `Either` for error states
- Type-safe error propagation
