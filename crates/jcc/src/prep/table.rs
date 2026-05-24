use crate::token::Token;

use std::{collections::HashMap, ops::Range};

// ---------------------------------------------------------------------------
// MacroTable
// ---------------------------------------------------------------------------

pub struct MacroTable<'a> {
    arena: Vec<Token>,
    map: HashMap<&'a str, Range<u32>>,
}

impl<'a> MacroTable<'a> {
    /// Creates an empty macro table.
    pub fn new() -> Self {
        Self {
            arena: Vec::new(),
            map: HashMap::new(),
        }
    }

    /// Removes a macro definition.
    ///
    /// Note: The body tokens remain in the pool but are no longer reachable.
    pub fn undefine(&mut self, name: &str) {
        self.map.remove(name);
    }

    /// Returns `true` if `name` is currently defined.
    pub fn is_defined(&self, name: &str) -> bool {
        self.map.contains_key(name)
    }

    /// Returns the body tokens for `name`, or `None` if not defined.
    pub fn expansion(&self, name: &str) -> Option<&[Token]> {
        self.map
            .get(name)
            .map(|r| &self.arena[r.start as usize..r.end as usize])
    }

    /// Defines (or redefines) a macro.
    ///
    /// Note: Any previous body for the same name is orphaned but not freed.
    pub fn define(&mut self, name: &'a str, body: impl IntoIterator<Item = Token>) {
        let start = self.arena.len() as u32;
        self.arena.extend(body);
        let end = self.arena.len() as u32;
        self.map.insert(name, start..end);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::token::TokenKind;
    use jcc_backend::codemap::span::Span;

    const BODY: [Token; 1] = [Token {
        span: Span::DEFAULT,
        kind: TokenKind::NumInt,
    }];

    const NEW_BODY: [Token; 2] = [
        Token {
            span: Span::DEFAULT,
            kind: TokenKind::NumFloat,
        },
        Token {
            span: Span::DEFAULT,
            kind: TokenKind::NumFloat,
        },
    ];

    #[test]
    fn undefine_noop() {
        let mut t = MacroTable::new();
        t.undefine("MISSING");
    }

    #[test]
    fn undefine_removes_macro() {
        let mut t = MacroTable::new();
        t.define("X", BODY);

        t.undefine("X");
        assert!(!t.is_defined("X"));
        assert!(t.expansion("X").is_none());
    }

    #[test]
    fn undefined_macro_returns_none() {
        let t = MacroTable::new();
        assert!(!t.is_defined("FOO"));
        assert!(t.expansion("FOO").is_none());
    }

    #[test]
    fn define_empty() {
        let mut t = MacroTable::new();
        t.define("EMPTY", []);
        assert!(t.is_defined("EMPTY"));
        assert_eq!(t.expansion("EMPTY"), Some([].as_slice()));
    }

    #[test]
    fn define_and_expand() {
        let mut t = MacroTable::new();
        t.define("X", BODY);
        assert_eq!(t.expansion("X"), Some(BODY.as_slice()));
    }

    #[test]
    fn redefine_replaces_expansion() {
        let mut t = MacroTable::new();
        t.define("X", BODY);
        t.define("X", NEW_BODY);
        assert_eq!(t.expansion("X"), Some(NEW_BODY.as_slice()));
    }

    #[test]
    fn multiple_macros_are_independent() {
        let mut t = MacroTable::new();
        t.define("A", BODY);
        t.define("B", NEW_BODY);
        assert_eq!(t.expansion("A"), Some(BODY.as_slice()));
        assert_eq!(t.expansion("B"), Some(NEW_BODY.as_slice()));
    }
}
