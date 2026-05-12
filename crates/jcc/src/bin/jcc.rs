use jcc::{cli::CompileOptions, profile::Profiler};

macro_rules! fatal {
    ($($arg:tt)*) => {{
        eprintln!("error: {}", format_args!($($arg)*));
        std::process::exit(1);
    }};
}

fn main() {
    match CompileOptions::from_env() {
        Err(e) => fatal!("{e}"),
        Ok(None) => CompileOptions::help(std::io::stdout()).expect("Failed to write help"),
        Ok(Some(args)) => {
            let mut p = Profiler::new(args.profile);
            let res = args.run(&mut p);
            p.report(&mut std::io::stdout())
                .expect("Failed to write profile report");
            if let Err(e) = res {
                fatal!("{e}");
            }
        }
    }
}
