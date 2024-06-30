# Compilation Pipeline

Scan -> Parse -> Resolve -> Sema -> Desugar -> Optimize -> Codegen

- Resolve: Links names to their declarations and definitinos
- Sema: Canonicalizes types found during parsing and type-checks the program
- Desugar: Desugars pointer arithmetic operations
- Optimize: TBD