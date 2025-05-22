use std::{collections::HashMap, num::NonZeroU32};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Symbol(NonZeroU32);

impl Default for Symbol {
    fn default() -> Self {
        Symbol(unsafe { NonZeroU32::new_unchecked(1) })
    }
}

pub struct Interner {
    buf: String,
    full: Vec<String>,
    vec: Vec<&'static str>,
    map: HashMap<&'static str, Symbol>,
}

impl Interner {
    pub fn new() -> Self {
        Self::with_capacity(1024)
    }

    pub fn with_capacity(capacity: usize) -> Self {
        Interner {
            full: Vec::new(),
            map: HashMap::new(),
            vec: vec![Default::default()],
            buf: String::with_capacity(capacity),
        }
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.vec[1..].len()
    }

    #[inline]
    pub fn is_empty(&self) -> bool {
        self.vec[1..].is_empty()
    }

    #[inline]
    pub fn lookup(&self, id: Symbol) -> &str {
        self.vec[id.0.get() as usize]
    }

    #[inline]
    pub fn get(&self, id: Symbol) -> Option<&str> {
        self.vec.get(id.0.get() as usize).copied()
    }

    #[inline]
    pub fn intern(&mut self, name: &str) -> Symbol {
        if let Some(&id) = self.map.get(name) {
            return id;
        }
        let symbol = unsafe { Symbol(NonZeroU32::new_unchecked(self.vec.len() as u32)) };
        let name = unsafe { self.alloc(name) };
        self.map.insert(name, symbol);
        self.vec.push(name);
        symbol
    }

    #[inline]
    unsafe fn alloc(&mut self, name: &str) -> &'static str {
        let cap = self.buf.capacity();
        if cap < self.buf.len() + name.len() {
            let cap = (cap.max(name.len()) + 1).next_power_of_two();
            let full = std::mem::replace(&mut self.buf, String::with_capacity(cap));
            self.full.push(full);
        }
        let start = self.buf.len();
        self.buf.push_str(name);
        &*(&self.buf[start..] as *const str)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_interner_is_empty() {
        let interner = Interner::new();
        assert_eq!(interner.len(), 0);
        assert!(interner.is_empty());
    }

    #[test]
    fn with_capacity_is_empty() {
        let interner = Interner::with_capacity(100);
        assert_eq!(interner.len(), 0);
        assert!(interner.is_empty());
    }

    #[test]
    fn intern_single_string() {
        let mut interner = Interner::new();
        let s1 = "hello";
        let sym1 = interner.intern(s1);

        assert_eq!(interner.len(), 1);
        assert!(!interner.is_empty());
        assert_eq!(interner.lookup(sym1), s1);
        assert_eq!(interner.get(sym1), Some(s1));
    }

    #[test]
    fn intern_same_string_multiple_times() {
        let mut interner = Interner::new();
        let s1 = "world";
        let sym1 = interner.intern(s1);
        let sym2 = interner.intern(s1);

        assert_eq!(sym1, sym2);
        assert_eq!(interner.len(), 1);
        assert_eq!(interner.lookup(sym1), s1);
    }

    #[test]
    fn intern_different_strings() {
        let mut interner = Interner::new();
        let s1 = "foo";
        let s2 = "bar";

        let sym1 = interner.intern(s1);
        let sym2 = interner.intern(s2);

        assert_ne!(sym1, sym2);
        assert_eq!(interner.len(), 2);
        assert_eq!(interner.lookup(sym1), s1);
        assert_eq!(interner.lookup(sym2), s2);
    }

    #[test]
    fn intern_empty_string() {
        let mut interner = Interner::new();
        let empty_str = "";
        let sym_empty = interner.intern(empty_str);

        assert_eq!(interner.len(), 1);
        assert_eq!(interner.lookup(sym_empty), empty_str);
        assert_eq!(interner.get(sym_empty), Some(empty_str));

        let sym_empty2 = interner.intern(empty_str);
        assert_eq!(sym_empty, sym_empty2);
        assert_eq!(interner.len(), 1);
    }
}
