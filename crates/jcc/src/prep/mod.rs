mod stack;
mod table;

pub mod stream;

use crate::token::{lex::LexerIssue, stream::TokenStream, Token, TokenKind};
use stack::{CondStack, CondState};
use table::MacroTable;

use jcc_backend::codemap::{
    file::SourceFile, span::Span, Diagnostic, IntoDiagnostic, Issue, Label,
};

use std::iter::FusedIterator;

// ---------------------------------------------------------------------------
// Preprocessor
// ---------------------------------------------------------------------------

pub struct Preprocessor<'a> {
    file: &'a SourceFile,
    stack: CondStack,
    current: Vec<Token>,
    pending: Vec<Token>,
    macros: MacroTable<'a>,
    stream: TokenStream<'a>,
}

impl<'a> Preprocessor<'a> {
    pub fn new(file: &'a SourceFile) -> Self {
        Self {
            file,
            current: Vec::new(),
            pending: Vec::new(),
            stack: CondStack::new(),
            macros: MacroTable::new(),
            stream: TokenStream::new(file),
        }
    }

    fn try_expand(&mut self, span: Span) -> bool {
        let Some(tokens) = self
            .file
            .slice(span)
            .and_then(|name| self.macros.expansion(name))
        else {
            return false;
        };
        self.pending.extend(tokens.iter().rev().copied());
        true
    }

    fn eval_cond(&mut self) -> bool {
        let res = self
            .stream
            .next_on_line()
            .and_then(Result::ok)
            .filter(|t| t.kind == TokenKind::NumInt)
            .and_then(|t| self.file.slice(t.span))
            .and_then(|s| s.parse::<i64>().ok())
            .is_some_and(|n| n != 0);
        self.stream.skip_line();
        res
    }

    fn read_ident_inline(&mut self, span: Span) -> Result<Option<&'a str>, Issue<PrepIssue>> {
        match self.stream.next_on_line() {
            None => Err(Issue::new(PrepIssue::ExpectedMacroName, span)),
            Some(Err(e)) => Err(Issue::new(PrepIssue::Lexer(e.kind), e.span)),
            Some(Ok(token)) => match token.kind {
                TokenKind::Identifier => Ok(self.file.slice(token.span)),
                _ => Err(Issue::new(PrepIssue::ExpectedMacroName, token.span)),
            },
        }
    }

    fn process_directive(&mut self) -> Result<(), Issue<PrepIssue>> {
        let span = match self.stream.next_on_line() {
            None => return Ok(()),
            Some(Ok(token)) => token.span,
            Some(Err(e)) => return Err(Issue::new(PrepIssue::Lexer(e.kind), e.span)),
        };
        let Some(keyword) = self.file.slice(span) else {
            return Ok(());
        };
        match keyword {
            "pragma" => self.stream.skip_line(),
            "if" => {
                if self.stack.is_suppressed() {
                    self.stream.skip_line();
                    self.stack.push(CondState::Done);
                } else {
                    let active = self.eval_cond();
                    self.stack.push(active);
                }
            }
            "ifdef" | "ifndef" => {
                if self.stack.is_suppressed() {
                    self.stream.skip_line();
                    self.stack.push(CondState::Done);
                } else {
                    let Some(name) = self.read_ident_inline(span)? else {
                        return Ok(());
                    };
                    self.stream.skip_line();
                    let defined = self.macros.is_defined(name);
                    self.stack.push(if keyword == "ifdef" {
                        defined
                    } else {
                        !defined
                    });
                }
            }
            "endif" => {
                self.stream.skip_line();
                if !self.stack.pop() {
                    return Err(Issue::new(PrepIssue::UnmatchedEndif, span));
                }
            }
            "else" => {
                self.stream.skip_line();
                match self.stack.top() {
                    Some(CondState::False) => self.stack.set(CondState::True),
                    Some(_) => self.stack.set(CondState::Done),
                    None => return Err(Issue::new(PrepIssue::UnmatchedEndif, span)),
                }
            }
            "elif" => match self.stack.top() {
                Some(CondState::Done) => self.stream.skip_line(),
                Some(CondState::False) => {
                    let active = self.eval_cond();
                    self.stack.set(active);
                }
                Some(CondState::True) => {
                    self.stream.skip_line();
                    self.stack.set(CondState::Done);
                }
                None => {
                    self.stream.skip_line();
                    return Err(Issue::new(PrepIssue::UnmatchedEndif, span));
                }
            },
            _ if self.stack.is_suppressed() => self.stream.skip_line(),
            "include" => {
                self.stream.skip_line();
                return Err(Issue::new(PrepIssue::IncludeNotSupported, span));
            }
            "undef" => {
                let Some(name) = self.read_ident_inline(span)? else {
                    return Ok(());
                };
                self.stream.skip_line();
                self.macros.undefine(name);
            }
            "define" => {
                let Some(name) = self.read_ident_inline(span)? else {
                    return Ok(());
                };
                while let Some(item) = self.stream.next_on_line() {
                    match item {
                        Ok(token) => self.current.push(token),
                        Err(e) => return Err(Issue::new(PrepIssue::Lexer(e.kind), e.span)),
                    }
                }
                self.macros.define(name, self.current.drain(..));
            }
            _ => {
                self.stream.skip_line();
                return Err(Issue::new(PrepIssue::UnknownDirective, span));
            }
        }

        Ok(())
    }
}

impl<'a> FusedIterator for Preprocessor<'a> {}
impl<'a> Iterator for Preprocessor<'a> {
    type Item = Result<Token, Issue<PrepIssue>>;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.pending.pop() {
                Some(token) => {
                    if !self.try_expand(token.span) {
                        return Some(Ok(token));
                    }
                }
                None => match self.stream.next()? {
                    Err(issue) => {
                        return Some(Err(Issue::new(PrepIssue::Lexer(issue.kind), issue.span)));
                    }
                    Ok(token) => match token.kind {
                        TokenKind::CommentBlock | TokenKind::CommentInline => continue,
                        TokenKind::DirectiveHash => {
                            if let Err(e) = self.process_directive() {
                                return Some(Err(e));
                            }
                        }
                        _ => {
                            if !self.stack.is_suppressed() && !self.try_expand(token.span) {
                                return Some(Ok(token));
                            }
                        }
                    },
                },
            }
        }
    }
}

// ---------------------------------------------------------------------------
// PrepIssue
// ---------------------------------------------------------------------------

#[derive(Clone)]
pub enum PrepIssue {
    MissingEndif,
    UnmatchedEndif,
    UnknownDirective,
    ExpectedMacroName,
    IncludeNotSupported,
    Lexer(LexerIssue),
}

impl IntoDiagnostic for PrepIssue {
    fn into_diagnostic(self, span: Span) -> Diagnostic {
        let (msg1, msg2, note) = match self {
            PrepIssue::Lexer(issue) => return issue.into_diagnostic(span),
            PrepIssue::UnmatchedEndif => ("unmatched `#endif` or `#else`", "no matching `#if`", None),
            PrepIssue::IncludeNotSupported => ("`#include` is not yet implemented", "`#include` is not supported", None),
            PrepIssue::ExpectedMacroName => ("expected an identifier after preprocessor directive", "expected a macro name here", None),
            PrepIssue::MissingEndif => ("unterminated `#if` block", "opened here", Some("every `#if`/`#ifdef`/`#ifndef` must be closed by `#endif`")),
            PrepIssue::UnknownDirective => ("unknown preprocessor directive", "unknown directive", Some("supported directives: #if, #ifdef, #ifndef, #elif, #else, #endif, #define, #undef, #pragma")),
        };
        Diagnostic::error()
            .with_notes(note)
            .with_message(msg1)
            .with_label(Label::primary(span).with_message(msg2))
    }
}
