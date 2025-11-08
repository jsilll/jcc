pub mod arena;
pub mod intern;

mod frozen;
mod traits;

use std::{
    hash::{Hash, Hasher},
    ops::Deref,
};

/// A reference to a value stored in an arena.
///
/// This is simply a type alias for a regular reference, but provides
/// better semantic clarity when working with arena-allocated values.
pub type ArenaRef<'arena, T> = &'arena T;

/// A reference to a value stored in an interning arena.
///
/// `Interned` behaves like a regular reference, but also encodes the fact that
/// the value has been interned (deduplicated) within an `InternArena`. This
/// allows for efficient equality comparisons based on pointer identity.
#[derive(Debug)]
pub struct Interned<'arena, T>(ArenaRef<'arena, T>);

impl<'a, T> Hash for Interned<'a, T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.0 as *const T).hash(state)
    }
}

impl<'a, T> Deref for Interned<'a, T> {
    type Target = T;

    fn deref(&self) -> ArenaRef<'a, T> {
        self.0
    }
}

impl<'a, T> Copy for Interned<'a, T> {}
impl<'a, T> Clone for Interned<'a, T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<'a, T> Eq for Interned<'a, T> {}
impl<'a, T> PartialEq for Interned<'a, T> {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.0, other.0)
    }
}
