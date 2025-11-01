pub mod arena;
pub mod intern;

mod frozen;
mod traits;

/// A reference to a value stored in an arena.
///
/// This is simply a type alias for a regular reference, but provides
/// better semantic clarity when working with arena-allocated values.
pub type ArenaRef<'arena, T> = &'arena T;
