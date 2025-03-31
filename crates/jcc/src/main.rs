use jcc::{
    lex::{Lexer, LexerDiagnosticKind},
    parse::Parser,
    sema::{control::ControlPass, resolve::ResolverPass, ty::TyperPass, SemaCtx},
    tacky::TackyBuilder,
};

use ssa::verify::SSAVerifier;
use tacky::{
    amd64::{build::AMD64Builder, emit::AMD64Emitter, fix::AMD64Fixer},
    source_file::{self, SourceDb, SourceFile},
    string_interner::StringInterner,
};

use anyhow::{Context, Result};
use clap::Parser as ClapParser;

use std::{path::PathBuf, process::Command};

#[derive(ClapParser)]
struct Args {
    /// Run the compiler in verbose mode
    #[clap(long)]
    pub verbose: bool,
    /// Run until the lexer and stop
    #[clap(long)]
    pub lex: bool,
    /// Run until the parser and stop
    #[clap(long)]
    pub parse: bool,
    /// Run until the semantic analyzer and stop
    #[clap(long)]
    pub validate: bool,
    /// Run until the SSA generator and stop
    #[clap(long)]
    pub ssa: bool,
    /// Run until the tacky generator and stop
    #[clap(long)]
    pub tacky: bool,
    /// Run until the codegen and stop
    #[clap(long)]
    pub codegen: bool,
    /// Emit an assembly file but not an executable
    #[clap(long, short = 'S')]
    pub assembly: bool,
    /// The path to the source file to compile
    pub path: PathBuf,
}

fn main() {
    if let Err(e) = try_main() {
        println!("{:?}", e);
        std::process::exit(1)
    }
}

fn try_main() -> Result<()> {
    let args = Args::parse();

    // Run the preprocessor with `gcc -E -P`
    let pp_path = args.path.with_extension("i");
    let output = Command::new("gcc")
        .arg("-E")
        .arg("-P")
        .arg(&args.path)
        .arg("-o")
        .arg(&pp_path)
        .output()?;
    if !output.status.success() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        return Err(anyhow::anyhow!("exiting due to preprocessor errors"));
    }

    // Add file to db
    let mut db = SourceDb::new();
    db.add(SourceFile::new(&pp_path).context("Failed to read file")?);
    let file = db.files().last().context("Failed to store file in db")?;

    // Lex file
    let mut interner = StringInterner::default();
    let mut lexer_result = Lexer::new(&file, &mut interner).lex();
    if args.lex {
        lexer_result
            .diagnostics
            .retain(|d| !matches!(d.kind, LexerDiagnosticKind::UnbalancedToken(_)));
    }
    if !lexer_result.diagnostics.is_empty() {
        source_file::diag::report_batch(&file, &mut std::io::stderr(), &lexer_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to lexer errors"));
    }
    if args.verbose {
        source_file::diag::report_batch(&file, &mut std::io::stderr(), &lexer_result.diagnostics)?;
    }
    if args.lex {
        return Ok(());
    }

    // Parse tokens
    let parser_result = Parser::new(&file, lexer_result.tokens.iter(), &mut interner).parse();
    if !parser_result.diagnostics.is_empty() {
        source_file::diag::report_batch(&file, &mut std::io::stderr(), &parser_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to parser errors"));
    }
    if args.verbose {
        println!("{:#?}", parser_result.ast);
    }
    if args.parse {
        return Ok(());
    }
    if parser_result.ast.items().is_empty() {
        eprintln!("Error: no items found in the source file");
        return Err(anyhow::anyhow!("exiting due to no items found"));
    }

    let mut ast = parser_result.ast;

    // Analyze the AST
    let mut ctx = SemaCtx::default();
    let control_result = ControlPass::new(&mut ctx).analyze(&mut ast);
    if !control_result.diagnostics.is_empty() {
        source_file::diag::report_batch(
            &file,
            &mut std::io::stderr(),
            &control_result.diagnostics,
        )?;
        return Err(anyhow::anyhow!("exiting due to control errors"));
    }
    let resolver_result = ResolverPass::new().analyze(&mut ast);
    if !resolver_result.diagnostics.is_empty() {
        source_file::diag::report_batch(
            &file,
            &mut std::io::stderr(),
            &resolver_result.diagnostics,
        )?;
        return Err(anyhow::anyhow!("exiting due to resolver errors"));
    }
    let typer_result = TyperPass::new(&mut ctx).analyze(&ast);
    if !typer_result.diagnostics.is_empty() {
        source_file::diag::report_batch(&file, &mut std::io::stderr(), &typer_result.diagnostics)?;
        return Err(anyhow::anyhow!("exiting due to typer errors"));
    }
    if args.verbose {
        println!("{:#?}", ast);
    }
    if args.validate {
        return Ok(());
    }

    let (mut amd64, interner) = match args.ssa {
        true => {
            // Generate SSA
            let ssa = jcc::ssa::build(&ast, interner);
            if args.verbose {
                println!("{}", ssa);
            }

            let verifier_result = SSAVerifier::new(&ssa).verify();
            if !verifier_result.diagnostics.is_empty() {
                for diag in verifier_result.diagnostics {
                    match diag {
                        ssa::verify::SSAVerifierDiagnostic::InvalidType(i) => {
                            eprintln!("error: invalid type for instruction {}", i);
                        }
                    }
                }
                return Err(anyhow::anyhow!("exiting due to ssa verifier errors"));
            }

            let amd64 = ssa::amd64::AMD64Builder::new(&ssa).build();
            if args.verbose {
                println!("{:#?}", amd64);
            }

            (amd64, ssa.take_interner())
        }
        false => {
            // Generate Tacky
            let tacky = TackyBuilder::new(&ctx, &mut interner).build(&ast);
            if args.verbose {
                println!("{:#?}", tacky);
            }
            if args.tacky {
                return Ok(());
            }

            // Generate AMD64
            let amd64 = AMD64Builder::new(&tacky).build();
            if args.verbose {
                println!("{:#?}", amd64);
            }

            (amd64, interner)
        }
    };

    // Fix intructions
    AMD64Fixer::new().fix(&mut amd64);
    if args.verbose {
        println!("{:#?}", amd64);
    }
    if args.codegen {
        return Ok(());
    }

    // Emit assembly
    let asm_path = args.path.with_extension("s");
    let asm = AMD64Emitter::new(&amd64, &interner).emit();
    std::fs::write(&asm_path, &asm).context("Failed to write assembly file")?;
    if args.assembly {
        return Ok(());
    }

    // Run the assembler and linker with `gcc`
    let output = Command::new("gcc")
        .arg(&asm_path)
        .arg("-o")
        .arg(&args.path.with_extension(""))
        .output()?;
    if !output.status.success() {
        eprintln!("{}", String::from_utf8_lossy(&output.stderr));
        return Err(anyhow::anyhow!("exiting due to assembler errors"));
    }

    Ok(())
}
