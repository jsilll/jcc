use jcc::{cli::CompileOptions, profile::Profiler};
use jcc_args::Args;
use jcc_backend::TargetOs;

use std::{collections::HashMap, path::PathBuf, time::Duration};

// ---------------------------------------------------------------------------
// CLI
// ---------------------------------------------------------------------------

struct BenchArgs {
    paths: Vec<PathBuf>,
    up_to: Option<usize>,
    suite: Option<PathBuf>,
}

impl BenchArgs {
    #[rustfmt::skip]
    fn print_help(mut out: impl std::io::Write) -> std::io::Result<()> {
        writeln!(out, "Usage:")?;
        writeln!(out, "  jcc-bench <file-or-dir>...")?;
        writeln!(out, "  jcc-bench --suite <tests-dir> [--up-to <N>]")?;
        writeln!(out)?;
        writeln!(out, "Options:")?;
        writeln!(out, "  --suite <dir>   Root directory containing chapter_N subdirectories.")?;
        writeln!(out, "                  Collects all .c files under each chapter's valid/ tree.")?;
        writeln!(out, "  --up-to <N>     Only include chapters 1 through N (default: all found).")?;
        writeln!(out, "  -h, --help      Show this help message.")
    }

    fn from_env() -> Result<Option<Self>, jcc_args::error::Error> {
        let mut args = Args::from_env();
        if args.contains(["-h", "--help"]) {
            return Ok(None);
        }

        let up_to: Option<usize> = args.opt_value_from_str("--up-to")?;
        let suite: Option<PathBuf> = args.opt_value_from_str("--suite")?;

        let paths: Vec<PathBuf> = args.free_values_from_str()?;
        args.finish()?;

        Ok(Some(BenchArgs {
            suite,
            up_to,
            paths,
        }))
    }
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

macro_rules! fatal {
    ($($arg:tt)*) => {{
        eprintln!("error: {}", format_args!($($arg)*));
        std::process::exit(1);
    }};
}

fn main() {
    match BenchArgs::from_env() {
        Err(e) => fatal!("{e}"),
        Ok(None) => BenchArgs::print_help(std::io::stderr()).unwrap(),
        Ok(Some(args)) => {
            if args.up_to.is_some() && args.suite.is_none() {
                fatal!("--up-to requires --suite");
            }

            let paths = args
                .suite
                .map(|root| collect_chapters_files(&root, args.up_to))
                .unwrap_or_else(|| {
                    let mut paths = Vec::new();
                    for p in args.paths {
                        if p.is_dir() {
                            collect_c_files(&p, &mut paths);
                        } else {
                            paths.push(p);
                        }
                    }
                    paths
                });
            if paths.is_empty() {
                fatal!("no .c files found");
            }

            let mut res = AggregateResults::default();
            eprintln!("compiling {} file(s)...", paths.len());
            for path in &paths {
                res.total += 1;
                let mut profiler = Profiler::new(true);
                if CompileOptions::new(TargetOs::Linux, path.clone())
                    .run(&mut profiler)
                    .is_ok()
                {
                    let log = profiler.log();
                    res.success += 1;
                    res.totals.push(log.iter().map(|(_, d)| *d).sum());
                    for &(pass, dur) in log {
                        let bucket = res.per_pass.entry(pass).or_insert_with(|| {
                            res.order.push(pass);
                            Vec::new()
                        });
                        bucket.push(dur);
                    }
                }
            }
            res.report(&mut std::io::stdout())
                .expect("failed to write report");
        }
    }
}

// ---------------------------------------------------------------------------
// Path collection
// ---------------------------------------------------------------------------

fn collect_c_files(dir: &std::path::Path, out: &mut Vec<PathBuf>) {
    let Ok(entries) = std::fs::read_dir(dir) else {
        return;
    };
    for entry in entries.filter_map(|e| e.ok()) {
        let path = entry.path();
        if path.is_dir() {
            collect_c_files(&path, out);
        } else if path.extension().is_some_and(|e| e == "c") {
            out.push(path);
        }
    }
}

fn collect_chapters_files(root: &std::path::Path, up_to: Option<usize>) -> Vec<PathBuf> {
    let mut paths = Vec::new();
    for chapter in 1..up_to.map_or(usize::MAX, |n| n + 1) {
        let dir = root.join(format!("chapter_{chapter}"));
        if !dir.is_dir() {
            break;
        }
        let valid = dir.join("valid");
        if valid.is_dir() {
            collect_c_files(&valid, &mut paths);
        }
    }
    paths
}

// ---------------------------------------------------------------------------
// Report
// ---------------------------------------------------------------------------

#[derive(Debug, Default)]
struct AggregateResults {
    total: usize,
    success: usize,
    totals: Vec<Duration>,
    order: Vec<&'static str>,
    per_pass: HashMap<&'static str, Vec<Duration>>,
}

impl AggregateResults {
    fn report(&mut self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        let max_name = self
            .order
            .iter()
            .map(|s| s.len())
            .max()
            .unwrap_or(0)
            .max("Total (per file)".len());
        let total = self.total;
        let success = self.success;
        let sep = "─".repeat(max_name + 76);
        writeln!(writer, "{sep}")?;
        writeln!(
            writer,
            "  jcc-bench  ·  {success}/{total} succeeded  ·  {} skipped",
            total - success
        )?;
        writeln!(writer, "{sep}")?;
        writeln!(
            writer,
            "  {:<w$}  {:>7}  {:>11}  {:>11}  {:>11}  {:>11}  {:>11}",
            "Pass",
            "n",
            "Total",
            "Mean",
            "Min",
            "Max",
            "p95",
            w = max_name
        )?;
        writeln!(writer, "{sep}")?;
        for pass in self.order.iter() {
            let Some(durations) = self.per_pass.get_mut(pass) else {
                continue;
            };
            Self::print_row(writer, pass, durations, max_name)?;
        }
        if !self.totals.is_empty() {
            writeln!(writer, "{sep}")?;
            Self::print_row(writer, "Total (per file)", &mut self.totals, max_name)?;
        }
        writeln!(writer, "{sep}")
    }

    fn ms(d: Duration) -> f64 {
        d.as_micros() as f64 / 1000.0
    }

    fn percentile(sorted: &[Duration], pct: f64) -> Duration {
        let norm = pct / 100.0;
        let range = (sorted.len() - 1) as f64;
        let idx = (norm * range).round() as usize;
        sorted[idx.min(sorted.len() - 1)]
    }

    fn print_row(
        writer: &mut impl std::io::Write,
        pass: &str,
        durations: &mut [Duration],
        width: usize,
    ) -> std::io::Result<()> {
        durations.sort();
        let min = durations[0];
        let max = durations[durations.len() - 1];
        let p95 = Self::percentile(durations, 95.0);
        let total = durations.iter().sum();
        let mean = total / durations.len() as u32;
        writeln!(
            writer,
            "  {:<w$}  {:>7}  {:>9.3}ms  {:>9.3}ms  {:>9.3}ms  {:>9.3}ms  {:>9.3}ms",
            pass,
            durations.len(),
            Self::ms(total),
            Self::ms(mean),
            Self::ms(min),
            Self::ms(max),
            Self::ms(p95),
            w = width
        )
    }
}
