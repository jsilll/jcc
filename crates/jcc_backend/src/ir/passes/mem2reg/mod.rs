use crate::ir::{
    analysis::{cfg::Control, dom::Dominance, order::Order},
    passes::mem2reg::{collect::Collect, insert::Insert, rename::Rename},
    Program,
};

mod collect;
mod insert;
mod rename;

#[derive(Default)]
pub struct Mem2Reg {
    rename: Rename,
    insert: Insert,
    collect: Collect,
}

impl Mem2Reg {
    pub fn run(&mut self, prog: &mut Program, ord: &Order, cfg: &Control, dom: &Dominance) {
        for &entry in cfg.entries() {
            if !self.collect.run(entry, prog, ord) {
                continue;
            }
            self.insert.run(prog, dom, &self.collect);

            let mut ctx = rename::Context::new(dom, &self.insert, &self.collect, prog);
            self.rename.run(entry, &mut ctx);
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use crate::ir::{
        analysis::{cfg::Control, dom::Dominance, order::Order},
        testutil::{check_parse, parse_ir},
    };

    use jcc_codemap::simple::SimpleFiles;

    fn assert_ir(input: &str, expected: &str) {
        let mut db = SimpleFiles::new();
        let mut interner = crate::IdentInterner::new();
        let mut parsed = parse_ir(&mut db, &mut interner, input);
        check_parse(&mut db, &parsed).unwrap_or_else(|report| panic!("{report}"));

        let mut ord = Order::default();
        let mut cfg = Control::default();
        let mut dom = Dominance::default();
        ord.compute(&parsed.program);
        cfg.compute(&parsed.program, &ord);
        dom.compute(&parsed.program, &ord, &cfg);
        Mem2Reg::default().run(&mut parsed.program, &ord, &cfg, &dom);

        let output = parsed.program.pretty(&interner).to_string();
        jcc_pretty_assertions::assert_eq!(output.trim(), expected.trim());
    }

    #[test]
    fn straight() {
        assert_ir(
r#"
define @straight {
bb0:
  %0 = alloca i32, align 4
  %1 = const i32 42
  store %0 %1, align 4
  %3 = load i32, ptr %0, align 4
  ret %3
}
"#,
            r#"
define @straight {
bb0:
  %1 = const i32 42
  ret %1
}
"#,
        );
    }

    #[test]
    fn diamond() {
        assert_ir(
r#"
define @diamond {
bb0:
  %0 = alloca i32, align 4
  %1 = param i1 #0
  %2 = const i32 1
  store %0 %2, align 4
  br i1 %1, bb1, bb2

bb1:
  %4 = const i32 2
  store %0 %4, align 4
  br bb3

bb2:
  %6 = const i32 3
  store %0 %6, align 4
  br bb3

bb3:
  %8 = load i32, ptr %0, align 4
  ret %8
}
"#,
r#"
define @diamond {
bb0:
  %1 = param i1 #0
  %2 = const i32 1
  br i1 %1, bb1, bb2

bb1:
  %4 = const i32 2
  upsilon %4 -> %9
  br bb3

bb3:
  %9 = phi i32
  ret %9

bb2:
  %6 = const i32 3
  upsilon %6 -> %9
  br bb3
}
"#,
        );
    }

    #[test]
    fn never_loaded() {
        assert_ir(
r#"
define @dead {
bb0:
  %0 = alloca i32, align 4
  %1 = const i32 42
  store %0 %1, align 4
  ret void
}
"#,
r#"
define @dead {
bb0:
  %1 = const i32 42
  ret void
}
"#,
        );
    }

    #[test]
    fn escaped() {
        assert_ir(
r#"
define @escaped {
bb0:
  %0 = alloca i32, align 4
  %1 = const i32 0
  %2 = gep ptr, ptr %0, %1
  ret void
}
"#,
r#"
define @escaped {
bb0:
  %0 = alloca i32, align 4
  %1 = const i32 0
  %2 = gep ptr, ptr %0, %1
  ret void
}
"#,
        );
    }

    #[test]
    fn undef() {
        assert_ir(
r#"
define @undef {
bb0:
  %0 = alloca ptr, align 8
  %1 = load ptr, ptr %0, align 8
  ret %1
}
"#,
            r#"
define @undef {
bb0:
  %1 = undef ptr
  ret %1
}
"#,
        );
    }
}
