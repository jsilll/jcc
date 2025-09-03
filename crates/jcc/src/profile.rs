use std::time::{Duration, Instant};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Pass {
    Preprocessor,
    Lexer,
    Parser,
    Resolver,
    Control,
    Typer,
    Lowering,
    SSABuild,
    AMD64Build,
    AMD64Fixer,
    AssemblyEmission,
    AssemblerAndLinker,
}

impl std::fmt::Display for Pass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pass::Preprocessor => write!(f, "Preprocessor (gcc -E)"),
            Pass::Lexer => write!(f, "Lexer"),
            Pass::Parser => write!(f, "Parser"),
            Pass::Resolver => write!(f, "Resolver"),
            Pass::Control => write!(f, "Control Pass"),
            Pass::Typer => write!(f, "Typer Pass"),
            Pass::Lowering => write!(f, "Lowering Pass"),
            Pass::SSABuild => write!(f, "SSA Build"),
            Pass::AMD64Build => write!(f, "AMD64 Build"),
            Pass::AMD64Fixer => write!(f, "AMD64 Fixer"),
            Pass::AssemblyEmission => write!(f, "Assembly Emission"),
            Pass::AssemblerAndLinker => write!(f, "Assembler & Linker (gcc)"),
        }
    }
}

pub struct Profiler {
    passes: Option<Vec<(Pass, Duration)>>,
}

impl Profiler {
    pub fn new(enabled: bool) -> Self {
        Self {
            passes: if !enabled {
                None
            } else {
                Some(Vec::with_capacity(16))
            },
        }
    }

    pub fn time<F, R>(&mut self, pass: Pass, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        match &mut self.passes {
            None => f(),
            Some(passes) => {
                let start = Instant::now();
                let result = f();
                let duration = start.elapsed();
                passes.push((pass, duration));
                result
            }
        }
    }

    pub fn report<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        match &self.passes {
            None => Ok(()),
            Some(passes) => {
                let max_name_len = passes
                    .iter()
                    .map(|(p, _)| p.to_string().len())
                    .max()
                    .unwrap_or(0);
                writeln!(writer, "{}", "-".repeat(max_name_len + 28))?;
                writeln!(writer, "Pass Timing Report")?;
                writeln!(writer, "{}", "-".repeat(max_name_len + 28))?;
                let total_duration: Duration = passes.iter().map(|(_, d)| *d).sum();
                for (pass, duration) in passes {
                    let percentage = if total_duration.as_nanos() > 0 {
                        (duration.as_secs_f64() / total_duration.as_secs_f64()) * 100.0
                    } else {
                        0.0
                    };
                    writeln!(
                        writer,
                        "{:<width$} : {:>10.4}ms ({:>5.1}%)",
                        pass.to_string(),
                        duration.as_micros() as f64 / 1000.0,
                        percentage,
                        width = max_name_len
                    )?;
                }
                writeln!(writer, "{}", "-".repeat(max_name_len + 28))?;
                writeln!(
                    writer,
                    "{:<width$} : {:>10.4}ms (100.0%)",
                    "Total",
                    total_duration.as_micros() as f64 / 1000.0,
                    width = max_name_len
                )?;
                writeln!(writer, "{}", "-".repeat(max_name_len + 28))?;
                Ok(())
            }
        }
    }
}
