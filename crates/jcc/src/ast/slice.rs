use std::marker::PhantomData;

/// A type-safe slice into one of the AST's arena vectors.
///
/// The phantom type parameter `T` ensures that slices can only be used with
/// their corresponding vector. For example, a `Slice<ExprRef>` can only be
/// used to index into `sliced_exprs`, preventing accidental misuse with
/// `sliced_decls` or other vectors.
#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct Slice<T>(pub u32, pub u32, PhantomData<T>);

impl<T> Default for Slice<T> {
    fn default() -> Self {
        Self::new(0, 0)
    }
}

impl<T> Slice<T> {
    /// Creates a new slice with the given begin and end indices.
    ///
    /// The range is `[begin, end)` (inclusive start, exclusive end).
    #[inline]
    pub fn new(begin: u32, end: u32) -> Self {
        Slice(begin, end, PhantomData)
    }

    /// Returns the number of elements in the slice.
    #[inline]
    pub fn len(&self) -> usize {
        (self.1 - self.0) as usize
    }

    /// Returns `true` if the slice contains no elements.
    #[inline]
    pub fn is_empty(&self) -> bool {
        self.0 == self.1
    }
}
