# Todo by README Chapter

> **Note:** Items indented under chapter headings are chapter sub-sections. Items under "Documentation Completeness" and "Minor Polish" are general todo sub-sections not tied to a specific chapter.

- [ ] ⭐ Introduction

  - [ ] ⭐ Key Features
  - [ ] ⭐ About This Documentation
  - [ ] ⭐ Installation
  - [ ] Quick Start section (minimal before Example 1)
  - [ ] API Reference (link to or mention where to find full API docs)
  - [ ] Comparison table (classic vs Transit approaches)
  - [ ] Troubleshooting (common issues or gotchas section)

- [ ] ⭐ Example 1: A Simple Door

  - [ ] ⭐ State updates: The Classic Approach
  - [ ] ⭐ State updates: The Transit Approach
  - [ ] ⭐ Compile-Time Safety
  - [ ] ⭐ Writing Tests for the update function
    - [ ] Show imports for foldl or note it's from Data.Array (Line 196-200)
    - [ ] Explain what scanl/foldl functions do (Line 221-222)
    - [ ] Show import for for\_ or explain it's from Data.Foldable (Line 273)
  - [ ] ⭐ Generate State Diagrams
  - [ ] ⭐ Generate Transition Tables

- [ ] ⭐ Example 2: Door with Pin

  - [ ] ⭐ State updates: The Classic Approach
  - [ ] ⭐ State updates: The Transit Approach
  - [ ] ⭐ Type signatures
    - [ ] Clarify what "this approach" refers to (ADT vs Variant) - Line 570
  - [ ] ⭐ Variants
    - [ ] Fix syntax explanation to match actual code (Line 509) - mention :? for conditional labels

- [ ] ⭐ Example 3: Seven Bridges of Königsberg

  - [ ] ⭐ Graph Analysis
    - [ ] Clarify what bidirectional arrows mean (Line 743)
    - [ ] Emphasize why odd degree matters (Line 855)
    - [ ] Replace placeholder test descriptions with meaningful ones (Line 808)

- [ ] ⭐ Example 4: The house of Santa Claus

  - [ ] ⭐ (main content)
    - [ ] Remove AI note and add proper explanation about "Das Haus vom Nikolaus" (Line 869)

- [ ] ⭐ More

  - [ ] ⭐ Monadic update functions
    - [ ] Give concrete example of when side effects are needed (Line 1028)

### Documentation Completeness

- [ ] Error handling: What happens if you try to transition from an invalid state?
- [ ] Performance considerations: Quantify ADT vs Variant conversion overhead
- [ ] Limitations or known issues section
- [ ] Contributing guidelines or where to report issues

### Minor Polish

- [ ] Remove or fix empty `<p align="right">` tag (Line 399)
- [ ] Add conclusion or "Next Steps" section (Line 1062)
