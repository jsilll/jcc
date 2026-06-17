pub mod lex;
pub mod parse;
pub mod token;

#[cfg(test)]
mod tests {
    use crate::{
        ir::testutil::{check_parse, parse_ir},
        IdentInterner,
    };

    fn check(input: &str) {
        let mut interner = IdentInterner::new();
        let mut db = jcc_codemap::simple::SimpleFiles::new();

        let ir1 = parse_ir(&mut db, &mut interner, input);
        check_parse(&mut db, &ir1).unwrap_or_else(|report| panic!("{report}"));

        let str1 = ir1.program.pretty(&interner).to_string();
        let ir2 = parse_ir(&mut db, &mut interner, &str1);
        check_parse(&mut db, &ir2).unwrap_or_else(|report| panic!("{report}"));

        let str2 = ir2.program.pretty(&interner).to_string();
        assert_eq!(str1, str2);
    }

    #[test]
    #[ignore]
    fn parse_null_pointer() {
        check(
            r#"
                define @maybe_ptr {
                bb0:
                  %0 = null ptr
                  ret %0
                }
            "#,
        );
    }

    #[test]
    #[ignore]
    fn parse_switch() {
        check(
            r#"
                define @classify {
                bb0:
                  %0 = param i32 #0

                  switch %0 [
                    default: bb_other,
                    0: bb_zero,
                    1: bb_one
                  ]

                bb_zero:
                  ret void

                bb_one:
                  ret void

                bb_other:
                  unreachable
                }
            "#,
        );
    }

    #[test]
    #[ignore]
    fn parse_compute() {
        check(
            r#"
                define @compute {
                bb0:
                  %0 = alloca i32, align 4

                  %1 = const i32 42
                  store %0 %1, align 4

                  %3 = load i32, ptr %0, align 4

                  %4 = neg i32 %3
                  %5 = not i32 %3

                  %6 = add i32 %4, %5

                  %7 = zext %6 to i64
                  %8 = trunc %7 to i8
                  %9 = sext %8 to i32

                  ret %9
                }
            "#,
        );
    }

    #[test]
    #[ignore]
    fn parse_abs() {
        check(
            r#"
                define @abs {
                bb0:
                  %0 = param i32 #0
                  %1 = const i32 0

                  %2 = icmp slt %0, %1
                  br i1 %2, bb_neg, bb_pos

                bb_neg:
                  %3 = neg i32 %0
                  br bb_merge

                bb_pos:
                  br bb_merge

                bb_merge:
                  %4 = phi i32

                  upsilon %3 -> %4
                  upsilon %0 -> %4

                  %7 = select i1 %2, i32 %3, i32 %0

                  ret %4
                }
            "#,
        );
    }

    #[test]
    #[ignore]
    fn parse_globals_and_calls() {
        check(
            r#"
                @limit = global i64 100
                @counter = global i32 zeroinitializer

                define @increment {
                bb0:
                  %0 = global.addr @counter

                  %1 = load i32, ptr %0, align 4
                  %2 = const i32 1
                  %3 = add i32 %1, %2

                  store %0 %3, align 4

                  ret %3
                }

                define @main {
                bb1:
                  %5 = call i32 @increment()

                  %6 = global.addr @limit

                  ret void
                }
            "#,
        );
    }
}
