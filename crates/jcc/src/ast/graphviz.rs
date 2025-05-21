use crate::ast::{Ast, BlockItem, Decl, DeclRef, Expr, ExprRef, ForInit, Stmt, StmtRef};

use std::fmt::Write;

use ssa::{Interner, Symbol};

pub struct AstGraphviz<'a> {
    ast: &'a Ast,
    interner: &'a Interner,
    output: String,
    node_counter: u32,
    indent_level: usize,
}

impl<'a> AstGraphviz<'a> {
    pub fn new(ast: &'a Ast, interner: &'a Interner) -> Self {
        AstGraphviz {
            ast,
            interner,
            node_counter: 0,
            indent_level: 0,
            output: String::with_capacity(1024),
        }
    }

    #[inline]
    fn with_indent<F>(&mut self, f: F)
    where
        F: FnOnce(&mut Self),
    {
        self.indent_level += 1;
        f(self);
        self.indent_level -= 1;
    }

    fn writeln(&mut self, s: &str) {
        for _ in 0..self.indent_level {
            self.output.push_str("  ");
        }
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn writeln_fmt(&mut self, args: std::fmt::Arguments) {
        for _ in 0..self.indent_level {
            self.output.push_str("  ");
        }
        self.output
            .write_fmt(args)
            .expect("Formatting AST to string failed");
        self.output.push('\n');
    }

    #[inline]
    fn get_symbol_name(&self, symbol: Symbol) -> &str {
        self.interner.resolve(symbol).unwrap_or("UnknownSymbol")
    }

    #[inline]
    fn fresh_aux_node_id(&mut self, hint: &str) -> String {
        let id = self.node_counter;
        self.node_counter += 1;
        format!("aux_{hint}_{id}")
    }

    #[inline]
    fn define_node(&mut self, id: &str, label: &str, fillcolor: &str) {
        self.writeln_fmt(format_args!(
            "{id} [label=\"{label}\", fillcolor={fillcolor}];",
        ));
    }

    #[inline]
    fn define_edge(&mut self, from_id: &str, to_id: &str, label: Option<&str>) {
        match label {
            None => self.writeln_fmt(format_args!("{from_id} -> {to_id};")),
            Some(label) => {
                self.writeln_fmt(format_args!("{from_id} -> {to_id} [label=\"{label}\"];",))
            }
        }
    }

    #[inline]
    fn define_edge_dotted(&mut self, from_id: &str, to_id: &str, label: Option<&str>) {
        match label {
            None => self.writeln_fmt(format_args!("{from_id} -> {to_id} [style=dotted];")),
            Some(label) => {
                self.writeln_fmt(format_args!(
                    "{from_id} -> {to_id} [label=\"{label}\", style=dotted];"
                ));
            }
        }
    }

    pub fn emit(mut self) -> String {
        self.writeln("digraph AST {{");
        self.with_indent(|s| {
            s.writeln("rankdir=TB;");
            s.writeln(
                "node [shape=box, style=\"rounded,filled\", fontname=\"Helvetica\", fontsize=10];",
            );
            s.writeln("edge [fontname=\"Helvetica\", fontsize=9];");
            s.define_node("ast_root", "ProgramRoot", "lightblue");
            s.ast.root().iter().for_each(|decl_ref| {
                let id = s.visit_decl(*decl_ref);
                s.define_edge("ast_root", &id, None);
            });
        });
        self.writeln("}}");
        self.output
    }

    fn visit_decl(&mut self, decl: DeclRef) -> String {
        let decl_id = format!("decl_{}", decl.0.get());
        match self.ast.decl(decl) {
            Decl::Var { name, init } => {
                let name = self.get_symbol_name(*name).escape_default();
                let label = format!("VarDecl\\nname: {}\\n(int assumed)", name);
                self.define_node(&decl_id, &label, "lightgoldenrodyellow");
                if let Some(init) = init {
                    let init_id = self.visit_expr(*init);
                    self.define_edge(&decl_id, &init_id, Some("initializer"));
                }
            }
            Decl::Func { name, params, body } => {
                let name = self.get_symbol_name(*name).escape_default();
                let label = format!("FuncDecl\\nname: {}\\n(int assumed)", name);
                self.define_node(&decl_id, &label, "palegreen");

                let params = self.ast.params(*params);
                if !params.is_empty() {
                    let params_id = self.fresh_aux_node_id("params");
                    self.define_node(&params_id, "Parameters", "aliceblue");
                    self.define_edge(&decl_id, &params_id, Some("params"));
                    for (idx, param) in params.iter().enumerate() {
                        let param_id = self.visit_decl(*param);
                        let edge_label = format!("param {idx}");
                        self.define_edge(&params_id, &param_id, Some(&edge_label));
                    }
                }

                match body {
                    None => {
                        let fwd_decl_id = format!("{decl_id}_fwd");
                        self.writeln_fmt(format_args!(
                            "{fwd_decl_id} [label=\"Forward Declaration\", shape=plaintext, fillcolor=none];",
                        ));
                        self.define_edge_dotted(&decl_id, &fwd_decl_id, Some("body (forward)"));
                    }
                    Some(body) => {
                        let body_id = self.fresh_aux_node_id("func_body");
                        self.define_node(&body_id, "Function Body", "whitesmoke");
                        self.define_edge(&decl_id, &body_id, Some("body"));
                        for (idx, item) in self.ast.block_items(*body).iter().enumerate() {
                            let item_id = match item {
                                BlockItem::Decl(decl) => self.visit_decl(*decl),
                                BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                            };
                            let edge_label = format!("item {idx}");
                            self.define_edge(&body_id, &item_id, Some(&edge_label));
                        }
                    }
                }
            }
        }
        decl_id
    }

    fn visit_stmt(&mut self, stmt: StmtRef) -> String {
        let stmt_id = format!("stmt_{}", stmt.0.get());
        match self.ast.stmt(stmt) {
            Stmt::Empty => {
                self.define_node(&stmt_id, "EmptyStmt", "gray90");
            }
            Stmt::Break => {
                self.define_node(&stmt_id, "BreakStmt", "lightcoral");
            }
            Stmt::Continue => {
                self.define_node(&stmt_id, "ContinueStmt", "lightsalmon");
            }
            Stmt::Expr(expr) => {
                self.define_node(&stmt_id, "ExprStmt", "azure");
                let expr_id = self.visit_expr(*expr);
                self.define_edge(&stmt_id, &expr_id, None);
            }
            Stmt::Return(expr) => {
                self.define_node(&stmt_id, "ReturnStmt", "mediumpurple1");
                let expr_id = self.visit_expr(*expr);
                self.define_edge(&stmt_id, &expr_id, Some("value"));
            }
            Stmt::Default(inner) => {
                self.define_node(&stmt_id, "DefaultStmt (Switch)", "khaki");
                let inner_id = self.visit_stmt(*inner);
                self.define_edge(&stmt_id, &inner_id, Some("stmt"));
            }
            Stmt::Goto(name) => {
                let name = self.get_symbol_name(*name).escape_default();
                let label = format!("GotoStmt\\nlabel: {}", name);
                self.define_node(&stmt_id, &label, "sandybrown");
            }
            Stmt::Label { label, stmt: inner } => {
                let label = self.get_symbol_name(*label).escape_default();
                let label = format!("LabelStmt\\nlabel: {}", label);
                self.define_node(&stmt_id, &label, "beige");
                let inner_id = self.visit_stmt(*inner);
                self.define_edge(&stmt_id, &inner_id, None);
            }
            Stmt::Case { expr, stmt: inner } => {
                self.define_node(&stmt_id, "CaseStmt (Switch)", "khaki");
                let expr_id = self.visit_expr(*expr);
                self.define_edge(&stmt_id, &expr_id, Some("condition"));
                let inner_id = self.visit_stmt(*inner);
                self.define_edge(&stmt_id, &inner_id, Some("stmt"));
            }
            Stmt::Switch { cond, body } => {
                self.define_node(&stmt_id, "SwitchStmt", "lightpink");
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&stmt_id, &cond_id, Some("condition"));
                let body_id = self.visit_stmt(*body);
                self.define_edge(&stmt_id, &body_id, Some("body"));
            }
            Stmt::While { cond, body } => {
                self.define_node(&stmt_id, "WhileStmt", "paleturquoise");
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&stmt_id, &cond_id, Some("condition"));
                let body_id = self.visit_stmt(*body);
                self.define_edge(&stmt_id, &body_id, Some("body"));
            }
            Stmt::DoWhile { body, cond } => {
                self.define_node(&stmt_id, "DoWhileStmt", "paleturquoise1");
                let body_id = self.visit_stmt(*body);
                self.define_edge(&stmt_id, &body_id, Some("body"));
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&stmt_id, &cond_id, Some("condition"));
            }
            Stmt::If {
                cond,
                then,
                otherwise,
            } => {
                self.define_node(&stmt_id, "IfStmt", "skyblue");
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&stmt_id, &cond_id, Some("condition"));
                let then_id = self.visit_stmt(*then);
                self.define_edge(&stmt_id, &then_id, Some("then_branch"));
                if let Some(otherwise) = otherwise {
                    let otherwise_id = self.visit_stmt(*otherwise);
                    self.define_edge(&stmt_id, &otherwise_id, Some("else_branch"));
                }
            }
            Stmt::Compound(items) => {
                self.define_node(&stmt_id, "CompoundStmt (Block)", "lightcyan");
                let items = self.ast.block_items(*items);
                if items.is_empty() {
                    let empty_marker_id = format!("{stmt_id}_empty_marker");
                    self.writeln_fmt(format_args!(
                        "{empty_marker_id} [label=\"(empty block)\", shape=plaintext, fillcolor=none];",
                    ));
                    self.define_edge(&stmt_id, &empty_marker_id, None);
                } else {
                    for (idx, item) in items.iter().enumerate() {
                        let item_id = match item {
                            BlockItem::Decl(decl) => self.visit_decl(*decl),
                            BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                        };
                        let label = format!("item {idx}");
                        self.define_edge(&stmt_id, &item_id, Some(&label));
                    }
                }
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
            } => {
                self.define_node(&stmt_id, "ForStmt", "thistle");
                if let Some(init) = init {
                    match init {
                        ForInit::Expr(expr) => {
                            let init_id = self.visit_expr(*expr);
                            self.define_edge(&stmt_id, &init_id, Some("initializer (expr)"));
                        }
                        ForInit::VarDecl(decl) => {
                            let decl_id = self.visit_decl(*decl);
                            self.define_edge(&stmt_id, &decl_id, Some("initializer (decl)"));
                        }
                    }
                }
                if let Some(cond) = cond {
                    let cond_id = self.visit_expr(*cond);
                    self.define_edge(&stmt_id, &cond_id, Some("condition"));
                }
                if let Some(step) = step {
                    let step_id = self.visit_expr(*step);
                    self.define_edge(&stmt_id, &step_id, Some("step"));
                }
                let body_id = self.visit_stmt(*body);
                self.define_edge(&stmt_id, &body_id, Some("body"));
            }
        }
        stmt_id
    }

    fn visit_expr(&mut self, expr: ExprRef) -> String {
        let expr_id = format!("expr_{}", expr.0.get());
        match self.ast.expr(expr) {
            Expr::Const(val) => {
                let label = format!("Const\\nvalue: {val}");
                self.define_node(&expr_id, &label, "gold");
            }
            Expr::Grouped(inner) => {
                self.define_node(&expr_id, "GroupedExpr", "lightgrey");
                let inner_id = self.visit_expr(*inner);
                self.define_edge(&expr_id, &inner_id, None);
            }
            Expr::Var(symbol) => {
                let symbol = self.get_symbol_name(*symbol).escape_default();
                let label = format!("VarRef\\nname: {}", symbol);
                self.define_node(&expr_id, &label, "olivedrab1");
            }
            Expr::Unary { op, expr: inner } => {
                let label = format!("UnaryOp\\nop: {:?}", op);
                self.define_node(&expr_id, &label, "coral");
                let inner_id = self.visit_expr(*inner);
                self.define_edge(&expr_id, &inner_id, Some("operand"));
            }
            Expr::Binary { op, lhs, rhs } => {
                let label = format!("BinaryOp\\nop: {:?}", op);
                self.define_node(&expr_id, &label, "orchid");
                let lhs_id = self.visit_expr(*lhs);
                self.define_edge(&expr_id, &lhs_id, Some("lhs"));
                let rhs_id = self.visit_expr(*rhs);
                self.define_edge(&expr_id, &rhs_id, Some("rhs"));
            }
            Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                self.define_node(&expr_id, "TernaryOp", "mediumspringgreen");
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&expr_id, &cond_id, Some("condition"));
                let then_id = self.visit_expr(*then);
                self.define_edge(&expr_id, &then_id, Some("then_expr"));
                let otherwise_id = self.visit_expr(*otherwise);
                self.define_edge(&expr_id, &otherwise_id, Some("else_expr"));
            }
            Expr::Call { name, args } => {
                let name = self.get_symbol_name(*name).escape_default();
                let label = format!("FunctionCall\\nname: {}", name);
                self.define_node(&expr_id, &label, "deepskyblue");

                let args = self.ast.args(*args);
                if !args.is_empty() {
                    let args_id = self.fresh_aux_node_id("args");
                    self.define_node(&args_id, "Arguments", "aliceblue");
                    self.define_edge(&expr_id, &args_id, Some("args"));
                    for (idx, arg) in args.iter().enumerate() {
                        let arg_id = self.visit_expr(*arg);
                        let label = format!("arg {idx}");
                        self.define_edge(&args_id, &arg_id, Some(&label));
                    }
                } else {
                    let no_args_id = format!("{expr_id}_no_args_marker");
                    self.writeln_fmt(format_args!(
                        "{no_args_id} [label=\"(no arguments)\", shape=plaintext, fillcolor=none];",
                    ));
                    self.define_edge_dotted(&expr_id, &no_args_id, None);
                }
            }
        }
        expr_id
    }
}
