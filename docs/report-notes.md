# Report Notes

## Design Decisions

The language is a small command-oriented DSL for querying and transforming RDF graphs. It is inspired by SPARQL concepts, but does not copy SPARQL surface syntax directly. The goal was to keep the language simpler and more learnable while still supporting the kinds of graph operations needed by the coursework.

The design treats graphs as the main runtime values. Input files are loaded from Turtle, queries manipulate graphs or pattern-match over them, and final outputs are always graphs.

A key design goal was orthogonality. Instead of creating one-off commands for each task, the language includes a small set of reusable operations that can be combined:
- graph loading
- graph union/intersection/difference
- graph pattern matching
- filtering over bindings
- graph construction
- aggregation
- Task 5 graph-edit sugar through `typeInfer`

This was done because the FAQ says Part 2 tasks will be extensions, combinations, or generalisations of Part 1 features, and that the interpreter code itself must stay the same.

The Turtle support is deliberately limited to the coursework subset rather than full Turtle. This keeps the implementation aligned with the specification and avoids over-implementing features that will not appear in the tests.

Graphs are represented as sets of triples. This matches RDF’s graph interpretation and automatically removes duplicate triples.

## Syntax Examples

### Loading and printing a graph

```rql
let g = load "tiny"
print g
```

### Graph union

```rql
let foo = load "foo"
let bar = load "bar"
let both = union foo bar
print both
```

### Pattern matching and filtering

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

### Pattern matching with boolean composition

```rql
let qux = load "qux"
let m = match {
in qux { ?s ?p ?o }
}
let a = filter m where ((?p == <http://example.org/ont/studiesAt> || ?p == <http://example.org/ont/worksFor>) && ?o == <http://example.org/uos>)
let g = construct { ?s ?p ?o } from a
print g
```

### Aggregate operation

```rql
let quux = load "quux"
let g = groupmax quux ?s <http://example.org/ont/price>
print g
```

### Graph edit / inference-style operation

```rql
let xyzzy = load "xyzzy"
let plugh = load "plugh"
let g = typeInfer(xyzzy, plugh)
print g
```

## Built-ins and Core Operations

### `load "name"`
Loads the Turtle file `name.ttl`.

### `print expr`
Evaluates `expr` and prints the resulting graph to stdout.

### `union e1 e2`
Graph union.

### `intersection e1 e2`
Graph intersection.

### `difference e1 e2`
Graph difference.

### `match { ... }`
Matches graph patterns against named graphs.

### `filter expr where (...)`
Filters bindings produced by a previous expression using predicate conditions.

### `construct { ... } from expr`
Builds graph output from bindings.

### `groupmax expr ?var <iri>`
Implements the Task 4 grouped maximum over integer objects for a given predicate.

### `typeInfer(e1, e2)`
Implements the Task 5 graph edit/join-style operation as a compact surface construct.

## Predicates

The current predicate language supports:
- equality with `==`
- numeric greater-than-or-equal with `>=`
- conjunction with `&&`
- disjunction with `||`

The parser encodes precedence so that `&&` binds more tightly than `||`.

## Rationale

The language was designed to balance simplicity and future extensibility.

A direct SPARQL clone would have been more complex than needed for the coursework. A very task-specific mini-language would have been risky for Part 2. The chosen design sits in between:
- simple command syntax
- SPARQL-inspired internal meaning
- reusable operations that combine cleanly

This gives a smaller and more coherent language while still supporting the required tasks and likely Part 2 extensions.

The front end also includes `intersection` and `difference` even though Part 1 does not strictly require both in the visible tasks. They were included because the FAQ explicitly points to them as natural generalisations of graph union.

## Testing Evidence

Testing currently covers:
- Turtle renderer behaviour
- Turtle loader behaviour
- duplicate elimination
- canonical graph ordering
- DSL lexer tokenisation
- trivial DSL parse cases
- direct parsing of Task 1 to Task 5 program shapes
- parsing of `.rql` files from disk
- negative parsing using malformed syntax

Current passing summary in the repo:
- RenderSpec: 6/6
- TurtleSpec: 11/11
- SyntaxSpec: 9/9

## Bugs Fixed / Important Adjustments

- fixed parser exception handling in `parseProgramFile`
- resolved GHCi ambiguity between Turtle and Syntax lexers by using qualified imports
- corrected task programs so they match the coursework spec rather than the broken examples in the handover PDF
- enforced unsigned integer lexing with no leading zeros except `0`
- kept keyword rules above identifier rules in the Alex lexer
- encoded boolean predicate precedence using layered non-terminals in Happy

## Extensions Beyond the Bare Minimum

The current front end already goes beyond the absolute minimum needed for the five visible Part 1 tasks:

- `intersection`
- `difference`
- boolean predicate composition with `&&` and `||`
- explicit graph `construct`
- `groupmax`
- `typeInfer`

These are intentional extensions to improve orthogonality and Part 2 readiness.

## Current Project State

At the time of writing:
- Turtle front end is implemented
- DSL front end is implemented
- runtime/evaluator is still to be completed
- `app/Main.hs` is still a temporary harness rather than the final `.rql` interpreter entry point

## References

### Coursework documents
- `Spec.pdf`
- `NEW FAQ Updated.pdf`
- `Manual.txt`
- `Overview-of-coursework.txt`

### Tooling / setup
- `AlexHappyStackV4.pdf`
- `guide for a good quality plc project.pdf`

### Design / research support
- `Turtle Lexer and Parser with Alex and Happy Plus Interpreter Design for an RDF Coursework Project - deep research.pdf`

### External language influence
- SPARQL 1.1 Query Language