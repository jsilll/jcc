use jcc::{
    ast::{graphviz::AstGraphviz, parse::Parser},
    cli::Args,
    lower::LoweringPass,
    profile::Profiler,
    sema::{control::ControlPass, resolve::ResolverPass, ty::TyperPass, SemaCtx, TypeDict},
    ssa::SSABuilder,
    tok::lex::Lexer,
};

use jcc_ssa::{
    self as ssa,
    amd64::{emit::AMD64Emitter, fix::AMD64Fixer},
    interner::Interner,
    sourcemap::{self, diag::Diagnostic, SourceDb, SourceMap},
};

use anyhow::{Context, Result};

fn main() {
    let args = Args::from_cli();
    let mut profiler = Profiler::new(args.profile);
    let r = try_main(&args, &mut profiler);
    profiler
        .report(&mut std::io::stdout())
        .expect("Failed to write profile report");
    if let Err(e) = r {
        println!("{:?}", e);
        std::process::exit(1)
    }
}

fn try_main(args: &Args, profiler: &mut Profiler) -> Result<()> {
    // === Run the preprocessor ===
    let pp_path = args.path.with_extension("i");
    profiler.time("Preprocessor (gcc -E)", || {
        let pp_output = std::process::Command::new("gcc")
            .arg("-E")
            .arg("-P")
            .arg(&args.path)
            .arg("-o")
            .arg(&pp_path)
            .output()
            .context("Failed to run preprocessor (gcc -E -P)")?;
        if !pp_output.status.success() {
            eprintln!("{}", String::from_utf8_lossy(&pp_output.stderr));
            return Err(anyhow::anyhow!("exiting due to preprocessor errors"));
        }
        Ok(())
    })?;

    // === Source Database ===
    let mut db = SourceDb::new();
    db.add(SourceMap::new(&pp_path).context(format!(
        "Failed to create source map for {}",
        pp_path.display()
    ))?);
    let file = db.files().last().context(format!(
        "No source files found in the database for {}",
        pp_path.display()
    ))?;

    // === Lexing & Parsing ===
    let lexer = Lexer::new(file);
    let mut dict = TypeDict::new();
    let mut interner = Interner::new();
    let r = profiler.time("Parser", || {
        Parser::new(lexer, file, &mut dict, &mut interner).parse()
    });
    check_diags("lexer", file, &r.lexer_diagnostics)?;
    if args.lex {
        return Ok(());
    }
    check_diags("parser", file, &r.parser_diagnostics)?;
    if args.emit_ast_graphviz {
        let dot_path = args.path.with_extension("dot");
        let ast_graphviz = AstGraphviz::new(&r.ast, &interner);
        let dot = ast_graphviz.emit().context("Failed to emit AST graphviz")?;
        std::fs::write(&dot_path, &dot).context("Failed to write AST graphviz file")?;
    }
    if args.parse {
        return Ok(());
    }

    // === Resolve ===
    let ast = r.ast;
    if ast.root().is_empty() {
        eprintln!("Error: no declarations in the source file");
        return Err(anyhow::anyhow!("exiting due to empty parse tree"));
    }
    let r = profiler.time("Resolver", || ResolverPass::new(&ast).check());
    check_diags("resolver", file, &r.diagnostics)?;

    // === Semantic Analysis ===
    let mut ctx = SemaCtx::with_dict(dict, r.symbol_count);
    let r = profiler.time("Control", || ControlPass::new(&ast, &mut ctx).check());
    check_diags("control", file, &r.diagnostics)?;
    let r = profiler.time("Typer", || TyperPass::new(&ast, &mut ctx).check());
    check_diags("typer", file, &r.diagnostics)?;
    let ast = profiler.time("Lower", || LoweringPass::new(ast, r.actions).build());
    if args.emit_ast_graphviz {
        let dot_path = args.path.with_extension("dot");
        let ast_graphviz = AstGraphviz::new(&ast, &interner);
        let dot = ast_graphviz.emit().context("Failed to emit AST graphviz")?;
        std::fs::write(&dot_path, &dot).context("Failed to write AST graphviz file")?;
    }
    if args.validate {
        return Ok(());
    }

    // === Build SSA ===
    let ssa = profiler.time("SSA Build", || {
        SSABuilder::new(&ast, &ctx, &mut interner).build()
    });
    if args.verbose {
        println!("{}", ssa);
    }
    if args.tacky {
        return Ok(());
    }

    // === Codegen ===
    let mut r = profiler.time("AMD64 Build", || {
        ssa::amd64::build::Builder::new(&ssa).build()
    });
    if args.verbose {
        println!(
            "{}",
            AMD64Emitter::new(&r.program, &interner, args.target.into()).emit()?
        );
    }
    profiler.time("AMD64 Fixer", || {
        AMD64Fixer::new(&r.table).fix(&mut r.program)
    });
    if args.verbose {
        println!(
            "{}",
            AMD64Emitter::new(&r.program, &interner, args.target.into()).emit()?
        );
    }
    if args.codegen {
        return Ok(());
    }

    // === Emit & Link ===
    let asm_path = args.path.with_extension("s");
    let asm = profiler.time("Assembly Emission", || {
        AMD64Emitter::new(&r.program, &interner, args.target.into()).emit()
    });
    std::fs::write(&asm_path, asm.unwrap()).context("Failed to write assembly file")?;
    if args.assembly {
        return Ok(());
    }
    profiler.time("Assembler & Linker (gcc)", || -> Result<()> {
        let mut extension = "";
        let mut cmd = std::process::Command::new("gcc");
        if args.no_link {
            cmd.arg("-c");
            extension = "o";
        }
        cmd.arg(&asm_path)
            .arg("-o")
            .arg(args.path.with_extension(extension));
        let output = cmd.output()?;
        if !output.status.success() {
            eprintln!("{}", String::from_utf8_lossy(&output.stderr));
            return Err(anyhow::anyhow!("exiting due to assembler errors"));
        }
        Ok(())
    })?;

    Ok(())
}

// ---------------------------------------------------------------------------
// Auxiliary functions
// ---------------------------------------------------------------------------

fn check_diags(
    pass: &str,
    file: &SourceMap,
    diags: &[impl Into<Diagnostic> + Clone],
) -> Result<()> {
    if !diags.is_empty() {
        sourcemap::diag::report_batch_to_stderr(file, diags)?;
        return Err(anyhow::anyhow!("exiting due to {} errors", pass));
    }
    Ok(())
}
