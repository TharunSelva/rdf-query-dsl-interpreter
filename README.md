# rdf-query-dsl-interpreter

A domain-specific language and interpreter in Haskell for querying and transforming RDF graphs using a subset of Turtle.

## Status of Role 1 branch

**Completed:**
- Turtle lexer (Alex)
- Turtle parser (Happy)
- Turtle AST
- Lowering from parsed Turtle syntax to Graph
- Canonical graph renderer
- Turtle smoke tests

**Not yet completed in this branch:**
- Final .rql interpreter entrypoint
- DSL lexer/parser (Role 2 work)
- Evaluator/runtime (Role 3 work)
- End-to-end execution of .rql programs

> This branch is a Turtle-front-end completion branch. It is a safe base for Role 2 and Role 3 only after the renderer ordering fix and fixture fix are applied.

## Build & test

```bash
stack build
stack test
```

## Run the Turtle-only loader (temporary)

```bash
stack exec -- rdf-query-dsl-interpreter fixtures/smoke/tiny.ttl
```
