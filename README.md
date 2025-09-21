# JCC – A C Compiler in Rust

`jcc` is an experimental **C compiler written in Rust**, targeting **AMD64**.
It implements the traditional stages of a C compiler, from **lexing** to **semantic analysis**, **SSA construction**, and **assembly generation**.
The project is modular and instrumented with profiling support, making it suitable for experimentation and learning about compiler internals.

---

## ✨ Features

* **Preprocessing**
  Uses `gcc -E -P` for preprocessing source files before compilation.

* **Lexing**
  Converts source text into tokens with diagnostics and error reporting.

* **Parsing**
  Builds an Abstract Syntax Tree (AST) from tokens.
  Supports optional **Graphviz DOT** output for visualizing the AST.

* **Semantic Analysis (Sema)**

  * Name resolution (`ResolverPass`)
  * Control flow analysis (`ControlPass`)
  * Type checking (`TyperPass`)
  * AST lowering into an intermediate form

* **SSA Generation**
  Builds a Static Single Assignment (SSA) representation for optimization and code generation.

* **Code Generation**

  * AMD64 instruction selection
  * Assembly emission
  * Assembly fixing (final tweaks for correctness)

* **Backend**
  Emits `.s` assembly, `.o` object files, or full executables via `gcc`.

* **Profiling**
  Built-in timing and profiling of each compiler stage.

* **Verbose Output**
  Optionally print tokens, SSA, or assembly to stdout.

---

## 🚀 Usage

### Build

```bash
cargo build --release
```

This produces the `jcc` binary in `target/release/`.

### Basic Compilation

```bash
./target/release/jcc path/to/file.c
```

By default, this will:

1. Preprocess the C file (`gcc -E -P`).
2. Compile down through lexing, parsing, semantic analysis, SSA, and code generation.
3. Emit AMD64 assembly.
4. Assemble and link with `gcc`, producing an executable.

### CLI Options

| Flag                  | Description                                                  |
| --------------------- | ------------------------------------------------------------ |
| `--verbose`           | Print additional information (tokens, SSA, assembly).        |
| `--profile`           | Time each compiler stage and print a report.                 |
| `--lex`               | Stop after lexing.                                           |
| `--parse`             | Stop after parsing.                                          |
| `--emit-ast-graphviz` | Emit the AST as a Graphviz DOT file.                         |
| `--validate`          | Stop after semantic analysis.                                |
| `--tacky`             | Stop after SSA generation.                                   |
| `--codegen`           | Stop after AMD64 code generation (before assembly emission). |
| `-S, --assembly`      | Emit an assembly file (`.s`) but no executable.              |
| `-c, --no-link`       | Emit an object file (`.o`) but do not link.                  |

---

## 🖼 Example Workflow

Compile and link a program:

```bash
./jcc examples/hello.c
./examples/hello   # Run the output program
```

Generate and view the AST as a graph:

```bash
./jcc examples/hello.c --emit-ast-graphviz --parse
dot -Tpng examples/hello.dot -o ast.png
```

Produce assembly only:

```bash
./jcc examples/hello.c -S
cat examples/hello.s
```

Compile to object file only:

```bash
./jcc examples/hello.c -c
ls examples/hello.o
```

Profile each stage:

```bash
./jcc examples/hello.c --profile
```

---

## 🔧 Dependencies

* **Rust toolchain** (Rust 1.70+ recommended)
* **GCC** (used for preprocessing, assembling, and linking)

---

## 📂 Project Structure

* `ast/` – AST definition, parser, Graphviz emission
* `lower/` – AST lowering passes
* `sema/` – Semantic analysis (resolver, typer, control flow checks)
* `ssa/` – SSA builder and optimizations
* `amd64/` – Code generation backend (instruction selection, emitter, fixer)
* `sourcemap/` – Diagnostics and source mapping
* `tok/` – Lexer and token definitions
* `profile/` – Profiler utilities

---

## ⚠️ Status

This compiler is **work-in-progress** and not production-ready.
The goal is to experiment with compiler construction and gradually expand language coverage, optimizations, and backends.

---

## 📜 License

[MIT](LICENSE)
