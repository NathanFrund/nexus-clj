# nexus-clj

**nexus-clj** is the Clojure implementation of the [Nexus](https://github.com/NathanFrund/nexus) spatial engine for Pharo Smalltalk.
It models space as a graph of meaningful nodes connected by traversable edges -- a
pointcrawl architecture that puts presence, witnessing, and path hazards at the centre.

All maps are plain JSON files, and the engine is a small set of pure functions
with zero external dependencies beyond a JSON parser.

## Philosophy

- Nodes are places that matter. A village square, a blacksmith, a forest clearing.
- Edges are paths. Roads, trails, rivers, one‑way cliffs, secret tunnels.
- Agents are always at a node. Movement means following an edge.
- Events happen at nodes. Everyone there can witness them.
- The same JSON maps drive both the Pharo and Clojure implementations.

## Features

- Graph‑based spatial model with bidirectional and one‑way edges
- Presence queries: "Who is at this location?"
- Witness‑event generation for social diffusion
- Movement with direction checks and random path hazards
- JSON map loading (identical to the Pharo Nexus engine)
- Pure functions, no mutable state, easy to test and compose

## Installation

Add the library to your `deps.edn`:

```clojure
{:deps {nexus-clj/nexus-clj {:git/url "https://github.com/NathanFrund/nexus-clj"
                              :sha "95eea0b"}}}'
```

## Quick Start

```clojure
(require '[nexus-clj.core :as nx])

;; Load a map from JSON (village.json)
(def graph (nx/load-graph "village.json"))

;; Define agents
(def world
  {:graph graph
   :agents [{:name "Elder" :location "elderHut"}
            {:name "Thug"  :location "village"}]})

;; Who is at the village?
(nx/agents-at-node world "village")
;; => ({:name "Thug", :location "village"})
```

## Running the Demo

```bash
clj -M -m nexus-clj.demo
```

## Running Tests

```bash
clj -X:test
```

## Design

nexus-clj is intentionally decoupled from any specific agent‑based model. It doesn't
know about personality traits, conversion rules, or game mechanics. It just produces
spatial events (witnessing, path hazards) that any simulation engine can consume.

The library is built from small, pure functions that operate on immutable maps.
This makes it trivial to test, easy to compose with other Clojure libraries
(core.async, XTDB, Datastar), and a direct translation of the Pharo Nexus
implementation -- the same JSON map works in both languages without modification.

## License

MIT
