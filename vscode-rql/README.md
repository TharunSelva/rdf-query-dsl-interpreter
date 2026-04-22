# VS Code RQL Syntax Extension

This folder contains a minimal VS Code extension for the RQL language used in this repository. It adds `.rql` file association, TextMate syntax highlighting, `#` line comments, and basic delimiter auto-closing.

The extension intentionally does not provide parsing, linting, semantic analysis, completions, or language server support.

## Included Files

- `package.json`
- `language-configuration.json`
- `syntaxes/rql.tmLanguage.json`
- `examples/highlight-sample.rql`

## Supported Syntax

- Keywords: `let`, `match`, `filter`, `where`, `construct`, `from`, `in`
- Built-ins: `load`, `print`, `union`, `intersection`, `difference`, `groupmax`, `groupmin`, `chain`, `typeInfer`
- Variables: `?name`
- Identifiers: `[A-Za-z][A-Za-z0-9_]*`
- Integers: `0`, `18`, `42`
- Strings: double quoted, single-line, no escapes
- IRIs: full `<...>` form only
- Comments: `#` to end of line

The grammar intentionally does not treat Turtle-only prefixed names such as `rdf:type` or `ont:age` as valid RQL syntax. RQL uses full IRIs in angle brackets.

## Local Testing

1. From the repository root, start an Extension Development Host:

```powershell
code --extensionDevelopmentPath "$PWD\vscode-rql"
```

2. In the new VS Code window, open `vscode-rql/examples/highlight-sample.rql` or any other `.rql` file.
3. Confirm the language mode is `RQL`.
4. Check that comments, strings, IRIs, variables, numbers, keywords, built-ins, operators, and delimiters are highlighted.
5. Compare the sample against `src/RQL/Syntax/Lexer.x` if you want to verify the token classes manually.

## Packaging

1. From `vscode-rql/`, create a VSIX package:

```powershell
npx @vscode/vsce package
```

2. Install the packaged extension locally:

```powershell
code --install-extension .\vscode-rql-0.0.1.vsix
```

## Notes

- The grammar is aligned with the audited RQL syntax implemented in `src/RQL/Syntax/Lexer.x`.
- The extension is metadata-only: no runtime JavaScript or npm dependencies are bundled.
- String literals do not support escapes.
- Integer literals are unsigned only, with no negative literal token and no leading zero form other than `0`.
- IRIs must use angle-bracketed full forms.
- Angle brackets are not configured as auto-closing pairs because `<` and `>` are also comparison operators in RQL.
