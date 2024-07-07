# João's C Compiler (JCC)

Welcome to my C Compiler project! This project aims to create a simple, yet functional C11 compiler written in C11, inspired by Rui Ueyema's [chibicc](https://github.com/rui314/chibicc) compiler. The goal is to provide a clear understanding of the compilation process while maintaining a clean and modular codebase.

## Current Status

I'm currently working on implementing zero-arity function calls.

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Compilation Pipeline](#compilation-pipeline)
- [Installation](#installation)
- [Usage](#usage)
- [Contributing](#contributing)
- [License](#license)

## Overview

This C compiler project follows a structured approach to compile C code into x86 machine code. It is designed for educational purposes, demonstrating the key stages of the compilation process.

## Features

### Detailed and User-Friendly Error Messages

Special attention has been given to the quality of error messages. Inspired by Rust, the compiler provides error messages that include the source location and a snippet of the problematic code to help developers quickly identify and fix issues.

Example:

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

Although C can be compiled with a single-stage compiler, this project follows a multi-stage compilation process to provide a clear understanding of the different problems that need to be solved during compilation. These stages are:

1. **Scan:** Converts the input source code into a stream of tokens (`scan.c/h`).
2. **Parse:** Analyzes the token stream to construct an abstract syntax tree (AST) (`parse.c/h`).
3. **Resolve:** Links names to their declarations and definitions (`resolve.c/h`).
4. **Sema:** Canonicalizes types found during parsing and type-checks the whole program for semantic correctness (`sema.c/h`).
5. **Desugar:** Simplifies complex constructs, such as pointer arithmetic operations (`desugar.c/h`).
6. **Optimize:** Performs AST-level code optimizations (TODO)
7. **Codegen:** Generates the final machine code from the optimized AST (`codegen_x86.c/h`).

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
