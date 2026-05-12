use std::time::{Duration, Instant};

// ---------------------------------------------------------------------------
// Profiler
// ---------------------------------------------------------------------------

type PassLog = Vec<(&'static str, Duration)>;

pub struct Profiler {
    log: Option<PassLog>,
}

impl Profiler {
    pub fn new(enabled: bool) -> Self {
        Self {
            log: if !enabled {
                None
            } else {
                Some(Vec::with_capacity(16))
            },
        }
    }

    #[inline]
    pub fn time<F, R>(&mut self, label: &'static str, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        match &mut self.log {
            None => f(),
            Some(passes) => {
                let start = Instant::now();
                let result = f();
                let duration = start.elapsed();
                passes.push((label, duration));
                result
            }
        }
    }

    pub fn log(&self) -> &[(&'static str, Duration)] {
        self.log.as_deref().unwrap_or(&[])
    }

    pub fn report(&self, writer: &mut impl std::io::Write) -> std::io::Result<()> {
        let passes = self.log();
        if passes.is_empty() {
            return Ok(());
        }
        let max_name_len = passes.iter().map(|(p, _)| p.len()).max().unwrap_or(0);
        let total: Duration = passes.iter().map(|(_, d)| *d).sum();
        let line_separator = "-".repeat(max_name_len + 28);
        writeln!(writer, "{}", line_separator)?;
        writeln!(writer, "Profiler Report")?;
        writeln!(writer, "{}", line_separator)?;
        for (pass, duration) in passes {
            let percentage = if total.as_nanos() > 0 {
                (duration.as_secs_f64() / total.as_secs_f64()) * 100.0
            } else {
                0.0
            };
            writeln!(
                writer,
                "{:<width$} : {:>10.4}ms ({:>5.1}%)",
                pass,
                duration.as_micros() as f64 / 1000.0,
                percentage,
                width = max_name_len
            )?;
        }
        writeln!(writer, "{}", line_separator)?;
        writeln!(
            writer,
            "{:<width$} : {:>10.4}ms (100.0%)",
            "Total",
            total.as_micros() as f64 / 1000.0,
            width = max_name_len
        )?;
        writeln!(writer, "{}", line_separator)
    }
}
