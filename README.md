# rdf-query-dsl-interpreter

A domain-specific language and interpreter in Haskell for querying and transforming RDF graphs using a restricted subset of Turtle.

## Project structure

- `app/` - executable entry point
- `src/RQL/Common/` - shared types, errors, and contracts
- `src/RQL/Turtle/` - Turtle front end: lexer, parser, lowering, loader, renderer
- `src/RQL/Syntax/` - DSL front end: lexer, parser, AST
- `src/RQL/Eval/` - evaluator/runtime
- `test/` - smoke and parser tests
- `fixtures/` - Turtle and `.rql` smoke fixtures
- `docs/` - report notes and source list

## Current status

### Completed
- Turtle lexer and parser
- Turtle lowering, loading, and canonical rendering
- DSL lexer and parser
- DSL AST and `parseProgramFile`
- smoke tests for Turtle rendering/loading and DSL parsing
- Part 1 program files:
  - `t1.rql`
  - `t2.rql`
  - `t3.rql`
  - `t4.rql`
  - `t5.rql`

### Still to do
- Runtime/evaluator implementation in `src/RQL/Eval/`
- Final `.rql` interpreter wiring in `app/Main.hs`

## Build and test

```bash
stack clean
stack build
stack test
```

## Current executable status

`app/Main.hs` is still a temporary harness and is not yet the final `.rql` interpreter entry point.

The intended final run style for the coursework is:

```bash
stack exec -- rdf-query-dsl-interpreter t1.rql
```

where the `.rql` program names the input Turtle graphs internally.

## DSL overview

The language is command-oriented and graph-focused.

Supported front-end constructs currently include:

- `let`
- `print`
- `load`
- `union`
- `intersection`
- `difference`
- `match`
- `filter`
- `construct`
- `groupmax`
- `typeInfer`

Predicate expressions currently support:

- `==`
- `>=`
- `&&`
- `||`

## Example programs

### Task 1
```rql
let foo = load "foo"
let bar = load "bar"
let both = union foo bar
print both
```

### Task 2
```rql
let baz = load "baz"
let m = match {
in baz { ?s <http://example.org/ont/hasAge> ?o }
}
let a = filter m where (?o >= 21)
let g = construct {
?s <http://example.org/ont/hasAge> ?o
} from a
print g
```

### Task 3
```rql
let qux = load "qux"
let m = match {
in qux { ?s ?p ?o }
}
let a = filter m where ((?p == <http://example.org/ont/studiesAt> || ?p == <http://example.org/ont/worksFor>) && ?o == <http://example.org/uos>)
let g = construct { ?s ?p ?o } from a
print g
```

### Task 4
```rql
let quux = load "quux"
let g = groupmax quux ?s <http://example.org/ont/price>
print g
```

### Task 5
```rql
let xyzzy = load "xyzzy"
let plugh = load "plugh"
let g = typeInfer(xyzzy, plugh)
print g
```

## Notes

- The interpreter is designed around graphs as outputs.
- Duplicate triples are ignored because graphs are represented as sets of triples.
- The front end already includes some orthogonal extensions beyond the bare minimum for Part 1, to better support Part 2.