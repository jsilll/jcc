use std::time::{Duration, Instant};

pub struct Profiler<T> {
    passes: Option<Vec<(T, Duration)>>,
}

impl<T: std::fmt::Display> Profiler<T> {
    pub fn new(enabled: bool) -> Self {
        Self {
            passes: if !enabled {
                None
            } else {
                Some(Vec::with_capacity(16))
            },
        }
    }

    #[inline]
    pub fn time<F, R>(&mut self, pass: T, f: F) -> R
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
