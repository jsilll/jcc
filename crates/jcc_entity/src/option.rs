/// Types that have a sentinel value which can't be created any other way.
pub trait SentinelValue: Eq {
    /// The sentinel value.
    const SENTINEL: Self;
}

/// A packed representation of `Option<T>`.
///
/// This is a wrapper around a `T`, using `T::SENTINEL` to represent `None`.
#[repr(transparent)]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct PackedOption<T: SentinelValue>(T);

impl<T: SentinelValue> Default for PackedOption<T> {
    fn default() -> Self {
        Self(T::SENTINEL)
    }
}

impl<T: SentinelValue> From<PackedOption<T>> for Option<T> {
    fn from(packed: PackedOption<T>) -> Option<T> {
        packed.expand()
    }
}

impl<T: SentinelValue> From<Option<T>> for PackedOption<T> {
    fn from(opt: Option<T>) -> Self {
        match opt {
            Some(t) => t.into(),
            None => Self::default(),
        }
    }
}

impl<T: SentinelValue> From<T> for PackedOption<T> {
    fn from(t: T) -> Self {
        debug_assert!(
            t != T::SENTINEL,
            "PackedOption::from called with the sentinel value."
        );
        Self(t)
    }
}

impl<T> std::fmt::Debug for PackedOption<T>
where
    T: SentinelValue + std::fmt::Debug,
{
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if self.is_none() {
            write!(f, "None")
        } else {
            write!(f, "Some({:?})", self.0)
        }
    }
}

impl<T: SentinelValue> PackedOption<T> {
    /// Const constructor wrapping a `T`.
    ///
    /// ## Notes
    ///
    /// - To create `None`, pass `T::SENTINEL`.
    /// - To create `Some(val)`, pass a non-reserved `val`.
    pub const fn new(val: T) -> Self {
        Self(val)
    }

    /// Returns `true` if the packed option is a `None` value.
    pub fn is_none(&self) -> bool {
        self.0 == T::SENTINEL
    }

    /// Returns `true` if the packed option is a `Some` value.
    pub fn is_some(&self) -> bool {
        self.0 != T::SENTINEL
    }

    /// Expand the packed option into a regular `Option`.
    pub fn expand(self) -> Option<T> {
        if self.is_none() {
            None
        } else {
            Some(self.0)
        }
    }

    /// Unwrap a packed `Some` value or panic.
    #[track_caller]
    pub fn unwrap(self) -> T {
        self.expand().unwrap()
    }

    /// Unwrap a packed `Some` value or panic with a message.
    #[track_caller]
    pub fn expect(self, msg: &str) -> T {
        self.expand().expect(msg)
    }

    /// Takes the value out of the option, leaving a `None` in its place.
    pub fn take(&mut self) -> Option<T> {
        std::mem::replace(self, Self(T::SENTINEL)).expand()
    }

    /// Maps a `PackedOption<T>` to `Option<U>` by applying a function to a contained value.
    pub fn map<U, F>(self, f: F) -> Option<U>
    where
        F: FnOnce(T) -> U,
    {
        self.expand().map(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Debug, PartialEq, Eq)]
    struct NoC(u32);

    impl SentinelValue for NoC {
        const SENTINEL: Self = Self(13);
    }

    #[derive(Clone, Copy, Debug, PartialEq, Eq)]
    struct TestEntity(u32);

    impl SentinelValue for TestEntity {
        const SENTINEL: Self = Self(13);
    }

    #[test]
    fn copies() {
        let x = TestEntity(2);
        let some: PackedOption<TestEntity> = x.into();
        assert_eq!(some.expand(), x.into());
        assert_eq!(some, x.into());
    }

    #[test]
    fn moves() {
        let x = NoC(3);
        let somex: PackedOption<NoC> = x.into();
        assert!(!somex.is_none());
        assert_eq!(somex.expand(), Some(NoC(3)));

        let none: PackedOption<NoC> = None.into();
        assert!(none.is_none());
        assert_eq!(none.expand(), None);
    }
}
