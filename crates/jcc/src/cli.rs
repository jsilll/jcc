use jcc_backend::TargetOs;

use std::{path::PathBuf, str::FromStr};

#[derive(Debug)]
pub struct Args {
    pub path: PathBuf,
    pub target: TargetOs,
    pub libs: Vec<String>,
    pub lex: bool,
    pub parse: bool,
    pub tacky: bool,
    pub codegen: bool,
    pub no_link: bool,
    pub verbose: bool,
    pub profile: bool,
    pub validate: bool,
    pub assembly: bool,
    pub emit_ast_graphviz: bool,
}

impl Args {
    #[rustfmt::skip]
    pub fn help(mut out: impl std::io::Write) -> std::io::Result<()> {
        writeln!(out, "Usage: jcc [OPTIONS] <PATH>")?;
        writeln!(out)?;
        writeln!(out, "Options:")?;
        writeln!(out, "  -h, --help              Show this help message")?;
        writeln!(out, "  -l <lib>                Link with the given library")?;
        writeln!(out, "  --target <target>       Target OS (linux, macos) [default: linux]")?;
        writeln!(out, "  --lex                   Stop after lexing")?;
        writeln!(out, "  --parse                 Stop after parsing")?;
        writeln!(out, "  --verbose               Run in verbose mode")?;
        writeln!(out, "  --profile               Run in profile mode")?;
        writeln!(out, "  --validate              Stop after validation")?;
        writeln!(out, "  --codegen               Stop after code generation")?;
        writeln!(out, "  --tacky                 Stop after Tacky IR generation")?;
        writeln!(out, "  -S, --assembly          Emit assembly file (.s)")?;
        writeln!(out, "  -c, --no-link           Compile and assemble, but do not link")?;
        writeln!(out, "  --emit-ast-graphviz     Emit AST Graphviz DOT file")?;
        Ok(())
    }

    pub fn from_env() -> Result<Option<Self>, jcc_args::error::Error> {
        let mut args = jcc_args::Args::from_env();
        if args.contains(["-h", "--help"]) {
            return Ok(None);
        }
        let res = Args {
            libs: args.values_from_str("-l")?,
            target: args
                .opt_value_from_str("--target")?
                .unwrap_or(TargetOs::Linux),
            lex: args.contains("--lex"),
            parse: args.contains("--parse"),
            tacky: args.contains("--tacky"),
            codegen: args.contains("--codegen"),
            verbose: args.contains("--verbose"),
            profile: args.contains("--profile"),
            validate: args.contains("--validate"),
            no_link: args.contains(["-c", "--no-link"]),
            assembly: args.contains(["-S", "--assembly"]),
            emit_ast_graphviz: args.contains("--emit-ast-graphviz"),
            path: args.free_from_fn(PathBuf::from_str)?,
        };
        args.finish()?;
        Ok(Some(res))
    }
}
