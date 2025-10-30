use std::time::{Duration, Instant};

// ---------------------------------------------------------------------------
// Profiler
// ---------------------------------------------------------------------------

pub struct Profiler {
    log: Option<Vec<(&'static str, Duration)>>,
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

    pub fn report<W: std::io::Write>(&self, writer: &mut W) -> std::io::Result<()> {
        match &self.log {
            None => Ok(()),
            Some(passes) => {
                let max_name_len = passes.iter().map(|(p, _)| p.len()).max().unwrap_or(0);
                let line_separator = "-".repeat(max_name_len + 28);
                writeln!(writer, "{}", line_separator)?;
                writeln!(writer, "Profiler Report")?;
                writeln!(writer, "{}", line_separator)?;
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
                writeln!(writer, "{}", line_separator)?;
                writeln!(
                    writer,
                    "{:<width$} : {:>10.4}ms (100.0%)",
                    "Total",
                    total_duration.as_micros() as f64 / 1000.0,
                    width = max_name_len
                )?;
                writeln!(writer, "{}", line_separator)?;
                Ok(())
            }
        }
    }
}
