// ---------------------------------------------------------------------------
// CondStack
// ---------------------------------------------------------------------------

pub struct CondStack {
    stack: Vec<CondState>,
    suppressed_depth: usize,
}

impl CondStack {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            suppressed_depth: 0,
        }
    }

    /// Returns `true` if any level of the stack is currently suppressing output.
    pub fn is_suppressed(&self) -> bool {
        self.suppressed_depth > 0
    }

    /// Returns the state of the innermost open block without consuming it.
    pub fn top(&self) -> Option<CondState> {
        self.stack.last().copied()
    }

    /// Opens a new conditional level with the given state.
    pub fn push(&mut self, state: impl Into<CondState>) {
        let state = state.into();
        if state != CondState::True {
            self.suppressed_depth += 1;
        }
        self.stack.push(state);
    }

    /// Closes the innermost conditional level. Returns `false` if the stack was already empty.
    pub fn pop(&mut self) -> bool {
        if let Some(state) = self.stack.pop() {
            if state != CondState::True {
                self.suppressed_depth -= 1;
            }
            return true;
        }
        false
    }

    /// Transitions the innermost level to `state`, updating suppression depth as needed.
    ///
    /// Note: This is a no-op if the stack is empty.
    pub fn set(&mut self, state: impl Into<CondState>) {
        let state = state.into();
        if let Some(last) = self.stack.last_mut() {
            let was = *last != CondState::True;
            let will = state != CondState::True;
            match (was, will) {
                (true, false) => self.suppressed_depth -= 1,
                (false, true) => self.suppressed_depth += 1,
                _ => {}
            }
            *last = state;
        }
    }
}

// ---------------------------------------------------------------------------
// CondState
// ---------------------------------------------------------------------------

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum CondState {
    /// Branch has not yet been taken.
    False,
    /// Branch is active, tokens are emitted.
    True,
    /// A prior branch was already taken, all remaining branches are suppressed.
    Done,
}

impl From<bool> for CondState {
    fn from(active: bool) -> Self {
        if active {
            CondState::True
        } else {
            CondState::False
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn new_is_not_suppressed() {
        let s = CondStack::new();
        assert!(!s.is_suppressed());
    }

    #[test]
    fn pop_on_empty_returns_false() {
        let mut s = CondStack::new();
        assert!(!s.pop());
    }

    #[test]
    fn push_false_suppresses() {
        let mut s = CondStack::new();
        s.push(CondState::False);
        assert!(s.is_suppressed());
    }

    #[test]
    fn push_done_suppresses() {
        let mut s = CondStack::new();
        s.push(CondState::Done);
        assert!(s.is_suppressed());
    }

    #[test]
    fn push_true_does_not_suppress() {
        let mut s = CondStack::new();
        s.push(CondState::True);
        assert!(!s.is_suppressed());
    }

    #[test]
    fn push_pop_round_trip() {
        let mut s = CondStack::new();
        s.push(CondState::False);
        assert!(s.is_suppressed());
        assert!(s.pop());
        assert!(!s.is_suppressed());
    }

    #[test]
    fn set_on_empty_is_noop() {
        let mut s = CondStack::new();
        s.set(CondState::True);
        assert!(!s.is_suppressed());
    }

    #[test]
    fn set_lifts_suppression() {
        let mut s = CondStack::new();
        s.push(CondState::False);
        s.set(CondState::True);
        assert!(!s.is_suppressed());
        assert_eq!(s.top(), Some(CondState::True));
    }

    #[test]
    fn set_adds_suppression() {
        let mut s = CondStack::new();
        s.push(CondState::True);
        s.set(CondState::Done);
        assert!(s.is_suppressed());
        assert_eq!(s.top(), Some(CondState::Done));
    }

    #[test]
    fn set_done_to_done_noop() {
        let mut s = CondStack::new();
        s.push(CondState::Done);
        s.set(CondState::Done);
        assert!(s.is_suppressed());
        assert_eq!(s.top(), Some(CondState::Done));
    }

    #[test]
    fn set_false_to_false_noop() {
        let mut s = CondStack::new();
        s.push(CondState::False);
        s.set(CondState::False);
        assert!(s.is_suppressed());
        assert_eq!(s.top(), Some(CondState::False));
    }

    #[test]
    fn nested_outer_true_inner_false() {
        let mut s = CondStack::new();
        s.push(CondState::True);
        s.push(CondState::False);
        assert!(s.is_suppressed());
    }

    #[test]
    fn nested_suppression_survives_pop() {
        let mut s = CondStack::new();
        s.push(CondState::False);
        s.push(CondState::True);
        assert!(s.is_suppressed());
        s.pop();
        assert!(s.is_suppressed());
    }

    #[test]
    fn suppressed_depth_tracks_levels() {
        let mut s = CondStack::new();

        s.push(CondState::False);
        s.push(CondState::False);
        s.push(CondState::False);
        assert!(s.is_suppressed());

        s.pop();
        assert!(s.is_suppressed());

        s.pop();
        assert!(s.is_suppressed());

        s.pop();
        assert!(!s.is_suppressed());
    }
}
