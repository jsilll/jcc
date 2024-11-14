use crate::lexer::Token;

pub struct Parser<'a> {
    tokens: &'a [Token],
}

impl<'a> Parser<'a> {
    pub fn new(tokens: &'a [Token]) -> Self {
        Self { tokens }
    }

    pub fn tokens(&self) -> &'a [Token] {
        self.tokens
    }
}
