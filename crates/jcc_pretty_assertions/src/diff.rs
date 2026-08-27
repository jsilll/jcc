#[derive(Debug, PartialEq, Eq)]
pub(crate) enum DiffOp<T> {
    Equal(T),
    Delete(T),
    Insert(T),
}

pub(crate) fn diff<T: PartialEq + Copy>(lhs: &[T], rhs: &[T]) -> Vec<DiffOp<T>> {
    let n = lhs.len();
    let m = rhs.len();
    let idx = |i, j| i * (m + 1) + j;
    let mut dp = vec![0; (n + 1) * (m + 1)];
    for i in 0..n {
        for j in 0..m {
            if lhs[i] == rhs[j] {
                dp[idx(i + 1, j + 1)] = dp[idx(i, j)] + 1;
            } else {
                let lval = dp[idx(i + 1, j)];
                let rval = dp[idx(i, j + 1)];
                dp[idx(i + 1, j + 1)] = std::cmp::max(lval, rval);
            }
        }
    }
    let mut i = n;
    let mut j = m;
    let mut diff = Vec::with_capacity(std::cmp::max(n, m));
    while i > 0 && j > 0 {
        if lhs[i - 1] == rhs[j - 1] {
            diff.push(DiffOp::Equal(lhs[i - 1]));
            i -= 1;
            j -= 1;
        } else if dp[idx(i - 1, j)] <= dp[idx(i, j - 1)] {
            diff.push(DiffOp::Insert(rhs[j - 1]));
            j -= 1;
        } else {
            diff.push(DiffOp::Delete(lhs[i - 1]));
            i -= 1;
        }
    }
    while i > 0 {
        diff.push(DiffOp::Delete(lhs[i - 1]));
        i -= 1;
    }
    while j > 0 {
        diff.push(DiffOp::Insert(rhs[j - 1]));
        j -= 1;
    }
    diff.reverse();
    diff
}

#[cfg(test)]
mod tests {
    use super::*;
    use DiffOp::*;

    #[test]
    fn empty() {
        let l: &[i32] = &[];
        let r: &[i32] = &[];
        assert!(diff(l, r).is_empty());
    }

    #[test]
    fn identical() {
        let l = &[1, 2, 3];
        let r = &[1, 2, 3];
        assert_eq!(diff(l, r), [Equal(1), Equal(2), Equal(3)]);
    }

    #[test]
    fn insertions() {
        let l = &[];
        let r = &[1, 2, 3];
        assert_eq!(diff(l, r), [Insert(1), Insert(2), Insert(3)]);
    }

    #[test]
    fn deletions() {
        let l = &[1, 2, 3];
        let r = &[];
        assert_eq!(diff(l, r), [Delete(1), Delete(2), Delete(3)]);
    }

    #[test]
    fn substitution() {
        let l = &[1, 2, 3];
        let r = &[1, 4, 3];
        assert_eq!(diff(l, r), [Equal(1), Delete(2), Insert(4), Equal(3)]);
    }

    #[test]
    fn partial() {
        let l = &[1, 2, 3];
        let r = &[2, 3, 4];
        assert_eq!(diff(l, r), [Delete(1), Equal(2), Equal(3), Insert(4)]);
    }

    #[test]
    fn different() {
        let l = &[1, 2];
        let r = &[3, 4];
        assert_eq!(diff(l, r), [Delete(1), Delete(2), Insert(3), Insert(4)]);
    }

    #[test]
    fn mixed() {
        let l = &['k', 'i', 't', 't', 'e', 'n'];
        let r = &['s', 'i', 't', 't', 'i', 'n', 'g'];
        let expected = [
            Delete('k'),
            Insert('s'), // substitution of k -> s
            Equal('i'),
            Equal('t'),
            Equal('t'),
            Delete('e'),
            Insert('i'), // substitution of e -> i
            Equal('n'),
            Insert('g'), // insertion at the end
        ];

        assert_eq!(diff(l, r), expected);
    }
}
