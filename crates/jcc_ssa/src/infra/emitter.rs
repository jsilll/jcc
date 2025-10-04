pub struct IndentedEmitter<T: std::fmt::Write> {
    output: T,
    indent_level: u8,
}

impl<T: std::fmt::Write> std::fmt::Write for IndentedEmitter<T> {
    fn write_str(&mut self, s: &str) -> std::fmt::Result {
        self.output.write_str(s)
    }

    #[inline]
    fn write_fmt(&mut self, args: std::fmt::Arguments) -> std::fmt::Result {
        for _ in 0..self.indent_level {
            self.output.write_str("    ")?;
        }
        self.output.write_fmt(args)?;
        Ok(())
    }
}

impl<T: std::fmt::Write> IndentedEmitter<T> {
    pub fn new(output: T) -> Self {
        Self {
            output,
            indent_level: 0,
        }
    }

    #[inline]
    pub fn indent(&mut self) {
        self.indent_level += 1
    }

    #[inline]
    pub fn unindent(&mut self) {
        self.indent_level -= 1
    }

    pub fn into_inner(self) -> T {
        self.output
    }
}
