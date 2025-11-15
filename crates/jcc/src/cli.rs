use jcc_ssa::TargetOs;

use clap::{Parser, ValueEnum};

use std::path::PathBuf;

/// Represents the target operating systems supported directly by the CLI.
///
/// This enum is used with `clap`'s `ValueEnum` to allow users to specify
/// the target OS via the `--target` command-line argument (e.g., `--target linux`).
/// It is intentionally kept separate from the internal `jcc_ssa::TargetOs` to decouple
/// the CLI representation from the compiler's core logic.
#[derive(ValueEnum, Clone, Copy, Debug)]
pub enum CliTargetOs {
    /// The Linux operating system.
    Linux,
    /// The macOS operating system.
    Macos,
}

/// Converts the CLI-specific `CliTargetOs` into the compiler's internal `TargetOs`.
///
/// This implementation allows for a clean separation of concerns, where `CliTargetOs`
/// is used only for parsing arguments, and `TargetOs` is used throughout the
/// rest of the compiler backend.
impl From<CliTargetOs> for TargetOs {
    fn from(cli_os: CliTargetOs) -> Self {
        match cli_os {
            CliTargetOs::Linux => TargetOs::Linux,
            CliTargetOs::Macos => TargetOs::Macos,
        }
    }
}

/// Defines and parses the command-line arguments accepted by the compiler.
///
/// This struct uses `clap::Parser` to automatically generate a parser from its
/// definition. Each field corresponds to a specific command-line option, flag,
/// or positional argument that controls the compiler's execution.
#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
pub struct Args {
    /// Run the compiler in verbose mode, printing detailed information
    /// about each compilation stage.
    #[clap(long)]
    pub verbose: bool,

    /// Run the compiler in profile mode to measure and report the time
    /// taken for each compilation stage.
    #[clap(long)]
    pub profile: bool,

    /// Stop compilation after the lexical analysis (lexing) phase.
    /// When this flag is set, the compiler only tokenizes the source file and then exits.
    #[clap(long)]
    pub lex: bool,

    /// Stop compilation after the parsing phase.
    /// When this flag is set, the compiler builds the Abstract Syntax Tree (AST) and then exits.
    #[clap(long)]
    pub parse: bool,

    /// Stop compilation after the semantic analysis (validation) phase.
    /// This phase includes critical checks like type checking.
    #[clap(long)]
    pub validate: bool,

    /// Stop compilation after generating the Tacky Intermediate Representation (IR).
    /// Tacky is a three-address code representation used internally by the compiler.
    #[clap(long)]
    pub tacky: bool,

    /// Stop compilation after the final code generation phase.
    /// This generates assembly or object code but skips the final linking step.
    #[clap(long)]
    pub codegen: bool,

    /// Compile and generate an object file, but do not run the linker to create an executable.
    /// This is equivalent to the `-c` flag in compilers like GCC and Clang.
    #[clap(long, short = 'c')]
    pub no_link: bool,

    /// Emit a human-readable assembly file (`.s`) instead of an object file or executable.
    /// This is equivalent to the `-S` flag in compilers like GCC and Clang.
    #[clap(long, short = 'S')]
    pub assembly: bool,

    /// The path to the source file to be compiled. This is a required argument.
    pub path: PathBuf,

    /// The target operating system for which to compile the code.
    /// The compiled output will be formatted for the specified OS.
    #[clap(long, value_enum, default_value_t = CliTargetOs::Linux)]
    pub target: CliTargetOs,

    /// Emit the Abstract Syntax Tree (AST) as a Graphviz DOT file and then stop.
    /// This is a debugging feature to visualize the structure of the parsed code.
    /// The output can be rendered into an image using tools like `dot`.
    #[clap(long)]
    pub emit_ast_graphviz: bool,
}

impl Args {
    /// Parses command-line arguments from the execution environment.
    ///
    /// This is a convenience function that wraps `clap::Parser::parse` to provide a
    /// clear, conventional entry point for argument parsing at the start of the program.
    ///
    /// # Panics
    ///
    /// This function will cause the program to exit if `clap` fails to parse the
    /// arguments (e.g., due to invalid input). In such cases, `clap` will
    /// automatically print a helpful error message or the help screen to the user.
    pub fn from_cli() -> Self {
        Self::parse()
    }
}
