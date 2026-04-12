# rdf-query-dsl-interpreter
A domain-specific language and interpreter in Haskell for querying and transforming RDF graphs using a subset of Turtle.

The current executable on this branch runs the Turtle loading pipeline: it reads a Turtle file and prints the canonical N-Triples rendering of the resulting graph.

## Run

```bash
cabal test
cabal run rdf-query-dsl-interpreter -- fixtures/smoke/tiny.ttl
```
