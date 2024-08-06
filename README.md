# João's C Compiler (JCC)

Welcome to JCC, a simple yet functional C11 compiler written in C11. Inspired by Rui Ueyema's [chibicc](https://github.com/rui314/chibicc), this project aims to provide a clear understanding of the compilation process while maintaining a clean and modular codebase.

## Current Status

Currently implementing zero-arity function calls.

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Compilation Pipeline](#compilation-pipeline)
- [Installation](#installation)
- [Usage](#usage)
- [Contributing](#contributing)
- [License](#license)

## Overview

JCC is an educational C compiler that demonstrates the key stages of compiling C code into x86 machine code. It follows a structured approach, making it ideal for learning about compiler design and implementation.

## Features

- Compiles C11 code to x86 machine code
- Multi-stage compilation process for clarity and modularity
- Detailed and user-friendly error messages
- Comprehensive test suite

### Detailed Error Messages

JCC provides Rust-inspired error messages that include source location and code snippets:

```
./bin/jcc '{ return ret3(); }'
error: undeclared identifier
 --> stdin:1:10
  |
1 | { return ret3(); }
  |          ┌───
  |          └─ must be declared before usage
```

## Compilation Pipeline

JCC uses a multi-stage compilation process:

1. **Scan:** Tokenize source code (`scan.c/h`)
2. **Parse:** Construct abstract syntax tree (AST) (`parse.c/h`)
3. **Resolve:** Link names to declarations/definitions (`resolve.c/h`)
4. **Sema:** Canonicalize types and perform semantic checks (`sema.c/h`)
5. **Desugar:** Simplify complex constructs (`desugar.c/h`)
6. **Optimize:** Performs AST-level code optimizations (TODO)
7. **Codegen:** Generates the final machine code from the optimized AST (`codegen_x86.c/h`)

## Installation

To install and build the C compiler, follow these steps:

1. Clone the repository:
    ```sh
    git clone https://github.com/jsilll/jcc.git
    ```
2. Navigate to the project directory:
    ```sh
    cd jcc
    ```
3. Build the compiler:
    ```sh
    make
    ```

## Usage

To compile a C source file, use the following command:

```sh
./bin/jcc --file <source_file>
```

This will generate the corresponding machine code or executable file.

To run the tests, use the following command:

```sh
./test.sh
```

## License

This project is licensed under the MIT License. See the LICENSE file for details.