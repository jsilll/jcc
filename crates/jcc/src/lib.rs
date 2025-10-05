pub mod ast;
pub mod cli;
pub mod lower;
pub mod profile;
pub mod sema;
pub mod ssa;
pub mod tok;

pub trait PassResult {
    fn diagnostics(&self) -> &[impl Into<jcc_ssa::sourcemap::diag::Diagnostic> + Clone];
}
