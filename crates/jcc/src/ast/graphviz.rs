use crate::ast::parse::{Ast, BlockItem, Decl, DeclRef, Expr, ExprRef, ForInit, Stmt, StmtRef};

use tacky::{Interner, Symbol};

use std::fmt::Write;

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
            output: String::new(),
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

    #[inline]
    fn writeln(&mut self, s: &str) {
        for _ in 0..self.indent_level {
            self.output.push(' ');
            self.output.push(' ');
        }
        self.output.push_str(s);
        self.output.push('\n');
    }

    #[inline]
    fn writeln_fmt(&mut self, args: std::fmt::Arguments) {
        for _ in 0..self.indent_level {
            self.output.push(' ');
            self.output.push(' ');
        }
        self.output.write_fmt(args).expect("invalid arguments");
        self.output.push('\n');
    }

    #[inline]
    fn decl_node_id(&self, decl: DeclRef) -> String {
        format!("decl_{}", decl.0.get())
    }

    #[inline]
    fn stmt_node_id(&self, stmt: StmtRef) -> String {
        format!("stmt_{}", stmt.0.get())
    }

    #[inline]
    fn expr_node_id(&self, expr: ExprRef) -> String {
        format!("expr_{}", expr.0.get())
    }

    #[inline]
    fn get_symbol_name(&self, symbol: Symbol) -> &str {
        self.interner.resolve(symbol).unwrap_or("Unknown")
    }

    #[inline]
    fn fresh_aux_node_id(&mut self, hint: &str) -> String {
        let id = self.node_counter;
        self.node_counter += 1;
        format!("aux_{}_{}", hint, id)
    }

    pub fn emit(mut self) -> String {
        self.writeln("digraph AST {{");
        self.with_indent(|s| {
            s.writeln("rankdir=TB;");
            s.writeln(
                "node [shape=box, style=\"rounded,filled\", fontname=\"Helvetica\", fontsize=10];",
            );
            s.writeln("edge [fontname=\"Helvetica\", fontsize=9];");
            s.writeln("ast_root [label=\"ProgramRoot\", fillcolor=lightblue];");
            s.ast.root().iter().for_each(|decl| {
                let id = s.visit_decl(*decl);
                s.writeln_fmt(format_args!("ast_root -> {};", id));
            });
        });
        self.writeln("}}");
        self.output
    }

    fn visit_decl(&mut self, decl: DeclRef) -> String {
        let id = self.decl_node_id(decl);
        match self.ast.decl(decl) {
            Decl::Var { name, init } => {
                let name = self.get_symbol_name(*name).to_string();
                self.writeln_fmt(
                    format_args!(
                    "{} [label=\"VarDecl\\nname: {}\\n(int assumed)\", fillcolor=lightgoldenrodyellow];",
                    id, name.escape_default()
                ));
                if let Some(init) = init {
                    let init_id = self.visit_expr(*init);
                    self.writeln_fmt(format_args!(
                        "{} -> {} [label=\"initializer\"];",
                        id, init_id
                    ));
                }
            }
            Decl::Func { name, params, body } => {
                let name = self.get_symbol_name(*name).to_string();
                self.writeln_fmt(format_args!(
                    "{} [label=\"FuncDecl\\nname: {}\\n(int assumed)\", fillcolor=palegreen];",
                    id,
                    name.escape_default()
                ));
                let params = self.ast.params(*params);
                if !params.is_empty() {
                    let params_id = self.fresh_aux_node_id("params");
                    self.writeln_fmt(format_args!(
                        "{} [label=\"Parameters\", fillcolor=aliceblue];",
                        params_id
                    ));
                    self.writeln_fmt(format_args!("{} -> {} [label=\"params\"];", id, params_id));
                    for (idx, param) in params.iter().enumerate() {
                        let param_id = self.visit_decl(*param);
                        self.writeln_fmt(format_args!(
                            "{} -> {} [label=\"param {}\"];",
                            params_id, param_id, idx
                        ));
                    }
                }
                match body {
                    None => {
                        self.writeln_fmt(format_args!(
                            "{} -> {}_fwd [label=\"body (forward)\"];",
                            id, id
                        ));
                        self.writeln_fmt(format_args!("{}_fwd [label=\"Forward Declaration\", shape=plaintext, fillcolor=none];", id));
                    }
                    Some(body) => {
                        let body_id = self.fresh_aux_node_id("func_body");
                        self.writeln_fmt(format_args!(
                            "{} [label=\"Function Body\", fillcolor=whitesmoke];",
                            body_id
                        ));
                        self.writeln_fmt(format_args!("{} -> {} [label=\"body\"];", id, body_id));
                        for (idx, item) in self.ast.block_items(*body).iter().enumerate() {
                            let item_id = self.visit_block_item(*item);
                            self.writeln_fmt(format_args!(
                                "{} -> {} [label=\"item {}\"];",
                                body_id, item_id, idx,
                            ));
                        }
                    }
                }
            }
        }
        id
    }

    fn visit_block_item(&mut self, item: BlockItem) -> String {
        match item {
            BlockItem::Decl(decl_ref) => self.visit_decl(decl_ref),
            BlockItem::Stmt(stmt_ref) => self.visit_stmt(stmt_ref),
        }
    }

    fn visit_stmt(&mut self, stmt: StmtRef) -> String {
        let stmt_id = self.stmt_node_id(stmt);
        let (label, color) = match self.ast.stmt(stmt) {
            Stmt::Empty => ("EmptyStmt".to_string(), "gray90".to_string()),
            Stmt::Break => ("BreakStmt".to_string(), "lightcoral".to_string()),
            Stmt::Continue => ("ContinueStmt".to_string(), "lightsalmon".to_string()),
            Stmt::Goto(symbol) => {
                let name = self.get_symbol_name(*symbol);
                (
                    format!("GotoStmt\\nlabel: {}", name.escape_default()),
                    "sandybrown".to_string(),
                )
            }
            Stmt::Expr(expr) => {
                let expr_id = self.visit_expr(*expr);
                self.writeln_fmt(format_args!(
                    "{} [label=\"ExprStmt\", fillcolor=azure];",
                    stmt_id
                ));
                self.writeln_fmt(format_args!("{} -> {};", stmt_id, expr_id));
                return stmt_id;
            }
            Stmt::Return(expr) => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"ReturnStmt\", fillcolor=mediumpurple1];",
                    stmt_id
                ));
                let expr_id = self.visit_expr(*expr);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"value\"];",
                    stmt_id, expr_id
                ));
                return stmt_id;
            }
            Stmt::Default(inner) => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"DefaultStmt (Switch)\", fillcolor=khaki];",
                    stmt_id
                ));
                let inner_id = self.visit_stmt(*inner);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"stmt\"];",
                    stmt_id, inner_id
                ));
                return stmt_id;
            }
            Stmt::Compound(items) => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"CompoundStmt (Block)\", fillcolor=lightcyan];",
                    stmt_id
                ));
                let items = self.ast.block_items(*items);
                if items.is_empty() {
                    self.writeln_fmt(
                        format_args!("{}_empty_marker [label=\"(empty block)\", shape=plaintext, fillcolor=none];", stmt_id)
                    );
                    self.writeln_fmt(format_args!("{} -> {}_empty_marker;", stmt_id, stmt_id));
                } else {
                    for (idx, item) in items.iter().enumerate() {
                        let item_id = self.visit_block_item(*item);
                        self.writeln_fmt(format_args!(
                            "{} -> {} [label=\"item {}\"];",
                            stmt_id, item_id, idx
                        ));
                    }
                }
                return stmt_id;
            }
            Stmt::Label { label, stmt: inner } => {
                let label = self.get_symbol_name(*label).to_string();
                self.writeln_fmt(format_args!(
                    "{} [label=\"LabelStmt\\nlabel: {}\", fillcolor=beige];",
                    stmt_id,
                    label.escape_default()
                ));
                let inner_id = self.visit_stmt(*inner);
                self.writeln_fmt(format_args!("{} -> {};", stmt_id, inner_id));
                return stmt_id;
            }
            Stmt::Case { expr, stmt: inner } => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"CaseStmt (Switch)\", fillcolor=khaki];",
                    stmt_id
                ));
                let expr_id = self.visit_expr(*expr);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"condition\"];",
                    stmt_id, expr_id
                ));
                let inner_id = self.visit_stmt(*inner);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"stmt\"];",
                    stmt_id, inner_id
                ));
                return stmt_id;
            }
            Stmt::Switch { cond, body } => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"SwitchStmt\", fillcolor=lightpink];",
                    stmt_id
                ));
                let cond_id = self.visit_expr(*cond);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"condition\"];",
                    stmt_id, cond_id
                ));
                let body_id = self.visit_stmt(*body);
                self.writeln_fmt(format_args!("{} -> {} [label=\"body\"];", stmt_id, body_id));
                return stmt_id;
            }
            Stmt::If {
                cond,
                then,
                otherwise,
            } => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"IfStmt\", fillcolor=skyblue];",
                    stmt_id
                ));
                let cond_id = self.visit_expr(*cond);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"condition\"];",
                    stmt_id, cond_id
                ));
                let then_id = self.visit_stmt(*then);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"then_branch\"];",
                    stmt_id, then_id
                ));
                if let Some(otherwise) = otherwise {
                    let otherwise_id = self.visit_stmt(*otherwise);
                    self.writeln_fmt(format_args!(
                        "{} -> {} [label=\"else_branch\"];",
                        stmt_id, otherwise_id
                    ));
                }
                return stmt_id;
            }
            Stmt::While { cond, body } => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"WhileStmt\", fillcolor=paleturquoise];",
                    stmt_id
                ));
                let cond_id = self.visit_expr(*cond);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"condition\"];",
                    stmt_id, cond_id
                ));
                let body_id = self.visit_stmt(*body);
                self.writeln_fmt(format_args!("{} -> {} [label=\"body\"];", stmt_id, body_id));
                return stmt_id;
            }
            Stmt::DoWhile { body, cond } => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"DoWhileStmt\", fillcolor=paleturquoise1];",
                    stmt_id
                ));
                let body_id = self.visit_stmt(*body);
                self.writeln_fmt(format_args!("{} -> {} [label=\"body\"];", stmt_id, body_id));
                let cond_id = self.visit_expr(*cond);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"condition\"];",
                    stmt_id, cond_id
                ));
                return stmt_id;
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
            } => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"ForStmt\", fillcolor=thistle];",
                    stmt_id
                ));
                if let Some(init) = init {
                    match init {
                        ForInit::Expr(expr) => {
                            let init_id = self.visit_expr(*expr);
                            self.writeln_fmt(format_args!(
                                "{} -> {} [label=\"initializer (expr)\"];",
                                stmt_id, init_id
                            ));
                        }
                        ForInit::VarDecl(decl) => {
                            let decl_id = self.visit_decl(*decl);
                            self.writeln_fmt(format_args!(
                                "{} -> {} [label=\"initializer (decl)\"];",
                                stmt_id, decl_id
                            ));
                        }
                    }
                }
                if let Some(cond) = cond {
                    let cond_id = self.visit_expr(*cond);
                    self.writeln_fmt(format_args!(
                        "{} -> {} [label=\"condition\"];",
                        stmt_id, cond_id
                    ));
                }
                if let Some(step) = step {
                    let step_id = self.visit_expr(*step);
                    self.writeln_fmt(format_args!("{} -> {} [label=\"step\"];", stmt_id, step_id));
                }
                let body_id = self.visit_stmt(*body);
                self.writeln_fmt(format_args!("{} -> {} [label=\"body\"];", stmt_id, body_id));
                return stmt_id;
            }
        };
        self.writeln_fmt(format_args!(
            "{} [label=\"{}\", fillcolor={}];",
            stmt_id, label, color
        ));
        stmt_id
    }

    fn visit_expr(&mut self, expr: ExprRef) -> String {
        let expr_id = self.expr_node_id(expr);
        let (label, fillcolor) = match self.ast.expr(expr) {
            Expr::Const(val) => (format!("Const\\nvalue: {}", val), "gold".to_string()),
            Expr::Var(symbol) => {
                let symbol = self.get_symbol_name(*symbol);
                (
                    format!("VarRef\\nname: {}", symbol.escape_default()),
                    "olivedrab1".to_string(),
                )
            }
            Expr::Grouped(inner) => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"GroupedExpr\", fillcolor=lightgrey];",
                    expr_id
                ));
                let inner_id = self.visit_expr(*inner);
                self.writeln_fmt(format_args!("{} -> {};", expr_id, inner_id));
                return expr_id;
            }
            Expr::Unary { op, expr: inner } => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"UnaryOp\\nop: {:?}\", fillcolor=coral];",
                    expr_id, op
                ));
                let inner_id = self.visit_expr(*inner);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"operand\"];",
                    expr_id, inner_id
                ));
                return expr_id;
            }
            Expr::Binary { op, lhs, rhs } => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"BinaryOp\\nop: {:?}\", fillcolor=orchid];",
                    expr_id, op
                ));
                let lhs_id = self.visit_expr(*lhs);
                self.writeln_fmt(format_args!("{} -> {} [label=\"lhs\"];", expr_id, lhs_id));
                let rhs_id = self.visit_expr(*rhs);
                self.writeln_fmt(format_args!("{} -> {} [label=\"rhs\"];", expr_id, rhs_id));
                return expr_id;
            }
            Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                self.writeln_fmt(format_args!(
                    "{} [label=\"TernaryOp\", fillcolor=mediumspringgreen];",
                    expr_id
                ));
                let cond_id = self.visit_expr(*cond);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"condition\"];",
                    expr_id, cond_id
                ));
                let then_id = self.visit_expr(*then);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"then_expr\"];",
                    expr_id, then_id
                ));
                let otherwise_id = self.visit_expr(*otherwise);
                self.writeln_fmt(format_args!(
                    "{} -> {} [label=\"else_expr\"];",
                    expr_id, otherwise_id
                ));
                return expr_id;
            }
            Expr::Call { name, args } => {
                let name = self.get_symbol_name(*name).to_string();
                self.writeln_fmt(format_args!(
                    "{} [label=\"FunctionCall\\nname: {}\", fillcolor=deepskyblue];",
                    expr_id,
                    name.escape_default()
                ));
                let args = self.ast.args(*args);
                if !args.is_empty() {
                    let args_id = self.fresh_aux_node_id("args");
                    self.writeln_fmt(format_args!(
                        "{} [label=\"Arguments\", fillcolor=aliceblue];",
                        args_id
                    ));
                    self.writeln_fmt(format_args!("{} -> {} [label=\"args\"];", expr_id, args_id));
                    for (idx, arg) in args.iter().enumerate() {
                        let arg_id = self.visit_expr(*arg);
                        self.writeln_fmt(format_args!(
                            "{} -> {} [label=\"arg {}\"];",
                            args_id, arg_id, idx
                        ));
                    }
                } else {
                    let no_args_node = format!("{}_no_args_marker", expr_id);
                    self.writeln_fmt(format_args!(
                        "{} [label=\"(no arguments)\", shape=plaintext, fillcolor=none];",
                        no_args_node
                    ));
                    self.writeln_fmt(format_args!(
                        "{} -> {} [style=dotted];",
                        expr_id, no_args_node
                    ));
                }
                return expr_id;
            }
        };
        self.writeln_fmt(format_args!(
            "{} [label=\"{}\", fillcolor={}];",
            expr_id, label, fillcolor
        ));
        expr_id
    }
}
