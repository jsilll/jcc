# jcc

A C compiler written in Rust, targeting AMD64.

Implements the full compilation pipeline from source text to native assembly:
lexing, parsing, semantic analysis, SSA construction, and code generation.
Preprocessing is delegated to GCC.

---

## Requirements

- Rust 1.70 or later
- GCC (used for preprocessing, assembling, and linking)

---

## Build
```bash
cargo build --release
```

The compiler binary is written to `target/release/jcc`.

---

## Usage
```bash
jcc [OPTIONS] <file.c>
```

By default, `jcc` preprocesses, compiles, assembles, and links the input file,
producing an executable in the same directory.

### Options

| Flag | Description |
|---|---|
| `--lex` | Stop after lexing |
| `--parse` | Stop after parsing |
| `--validate` | Stop after semantic analysis |
| `--tacky` | Stop after SSA generation |
| `--codegen` | Stop after instruction selection |
| `-S`, `--assembly` | Emit assembly (`.s`) without assembling |
| `-c`, `--no-link` | Emit an object file (`.o`) without linking |
| `--emit-ast-graphviz` | Write the AST to a Graphviz DOT file |
| `--verbose` | Print tokens, SSA, and assembly to stdout |
| `--profile` | Print per-stage timing after compilation |

---

## Examples

Compile and run a program:
```bash
jcc examples/hello.c && ./examples/hello
```

Emit assembly only:
```bash
jcc examples/hello.c -S
```

Visualize the AST:
```bash
jcc examples/hello.c --emit-ast-graphviz --parse
dot -Tpng examples/hello.dot -o ast.png
```

Profile compilation stages:
```bash
jcc examples/hello.c --profile
```

---

## Architecture

| Directory | Contents |
|---|---|
| `tok/` | Lexer and token definitions |
| `ast/` | Parser, AST definitions, Graphviz emission |
| `sema/` | Semantic analysis: name resolution, type checking, control flow |
| `lower/` | AST lowering passes |
| `ssa/` | SSA construction and optimization |
| `amd64/` | Instruction selection, assembly emission, and fixup passes |
| `sourcemap/` | Source locations and diagnostic reporting |
| `profile/` | Stage timing utilities |

---

## Status

Work in progress. Language coverage, optimizations, and backend targets are under active development.

---

## License

MIT