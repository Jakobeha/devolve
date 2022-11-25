[![version](https://img.shields.io/crates/v/devolve)](https://crates.io/crates/devolve)
[![documentation](https://docs.rs/devolve/badge.svg)](https://docs.rs/devolve)
![LICENSE](https://img.shields.io/crates/l/devolve)

# devolve: node-based DSL

for rapid iteration and reactive programming.

*Obligitory disclaimer: this is an experimental and early project*

## What?

Devolve is a **node-based DSL** like [Node-RED](https://nodered.org/) or  [Unreal's Blueprints](https://docs.unrealengine.com/5.0/en-US/blueprints-visual-scripting-in-unreal-engine/). Also known as [Flow-based Programming](https://blog.kodigy.com/post/state-of-flow-based-programming/).

Each node is an effectful computation (function), and nodes are defined in Rust using macros. `.dvl` files can also be converted into Rust functions using macros.

Each Devolve program is a function: it takes a set of inputs and a context, and produce a set of outputs. Devolve programs are **instant**: they have no `poll` or `sleep` and only iterate over collections, though they are not 100% pure. Devolve programs are strongly-typed and the type system interfaces with Rust's, although there are no linear types so everything must be `Copy` or a shared reference. Devolve programs are typically edited in the devolve IDE though they may also be edited directly.

## Why?

devolve facilitates **rapid iteration**. A big issue with Rust is that you must recompile and rerun your program every time you want to make changes. `.dvl` files can be live-reloaded.

devolve facilitates **high-level thinking**. Visual languages are [*terrible*](https://blog.kodigy.com/post/state-of-flow-based-programming/#granularity-of-dataflow-systems) for representing complex computations, but better than text-based languages at representing simple higher-level computations, as they are also diagrams.

devolve *may* facilitate **collaboration with non-programmers**, as visual languages may be more familiar to them.

### Use cases:

- UI (reactive)
- Heuristics
- Shaders
- Low-code or high-level

Scenarios:

- Prototyping
- Collaboration with non-programmers

## devolve-ui

The primary use case for devolve scripts is [devolve-ui](https://github.com/Jakobeha/devolve-ui),  a UI framework. devolve-ui is designed to rapidly-iterable, low-code, functional, accessible to non-programmers - all use cases of the devolve language.

devolve-ui is more than devolve: it also includes the UI nodes, and a separate framework for building asynchronous UIs in Python. But devolve's design is tightly integrated with devolve-ui.