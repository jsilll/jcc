use crate::lexer::Token;
use string_interner::backend::Backend;

pub struct Parser<'a, StringBackend: Backend> {
    tokens: &'a [Token<StringBackend>],
}

impl<'a, StringBackend: Backend> Parser<'a, StringBackend> {
    pub fn new(tokens: &'a [Token<StringBackend>]) -> Self {
        Self { tokens }
    }

    pub fn tokens(&self) -> &'a [Token<StringBackend>] {
        self.tokens
    }
}
