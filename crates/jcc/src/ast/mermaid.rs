use crate::ast::{
    Ast, BlockItem, DeclKind, DeclRef, ExprKind, ExprRef, ForInit, StmtKind, StmtRef,
};

use jcc_ssa::interner::Interner;

use std::fmt::Write;

pub struct AstMermaid<'a> {
    ast: &'a Ast,
    interner: &'a Interner,
    output: String,
    node_counter: u32,
}

impl<'a> AstMermaid<'a> {
    pub fn new(ast: &'a Ast, interner: &'a Interner) -> Self {
        AstMermaid {
            ast,
            interner,
            node_counter: 0,
            output: String::with_capacity(1024),
        }
    }

    #[inline]
    fn writeln(&mut self, s: &str) {
        self.output.push_str(s);
        self.output.push('\n');
    }

    #[inline]
    fn writeln_fmt(&mut self, args: std::fmt::Arguments) {
        self.output
            .write_fmt(args)
            .expect("Formatting AST to string failed");
        self.output.push('\n');
    }

    #[inline]
    fn escape_label(&self, label: &str) -> String {
        label.replace('"', "#quot;").replace('\n', "<br>")
    }

    #[inline]
    fn define_node(&mut self, id: &str, label: &str) {
        let label = self.escape_label(label);
        self.writeln_fmt(format_args!("{id}[\"{label}\"]"));
    }

    #[inline]
    fn fresh_aux_node_id(&mut self, hint: &str) -> String {
        let id = self.node_counter;
        self.node_counter += 1;
        format!("aux_{hint}_{id}")
    }

    #[inline]
    fn define_edge(&mut self, from_id: &str, to_id: &str, label: Option<&str>) {
        match label {
            None => self.writeln_fmt(format_args!("{from_id} --> {to_id}")),
            Some(label) => {
                let label = self.escape_label(label);
                self.writeln_fmt(format_args!("{from_id} -- \"{label}\" --> {to_id}"))
            }
        }
    }

    #[inline]
    fn define_edge_dotted(&mut self, from_id: &str, to_id: &str, label: Option<&str>) {
        match label {
            None => self.writeln_fmt(format_args!("{from_id} -.-> {to_id}")),
            Some(label) => {
                let label = self.escape_label(label);
                self.writeln_fmt(format_args!("{from_id} -. \"{label}\" .-> {to_id}"))
            }
        }
    }

    pub fn emit(mut self) -> String {
        self.writeln("graph TD");
        self.define_node("ast_root", "ASTRoot");
        self.ast.root().iter().for_each(|decl_ref| {
            let id = self.visit_decl(*decl_ref);
            self.define_edge("ast_root", &id, None);
        });
        self.output
    }

    fn visit_decl(&mut self, decl_ref: DeclRef) -> String {
        let decl_id = format!("decl_{}", decl_ref.0.get());
        let decl = self.ast.decl(decl_ref);
        match decl.kind {
            DeclKind::Var(init) => {
                let name = self.interner.lookup(decl.name.raw);
                let label = format!(
                    "VarDecl\nname: {}\n(int assumed)\nstorage: {:?}\nsema: {:?}",
                    name,
                    decl.storage,
                    decl.name.sema.get()
                );
                self.define_node(&decl_id, &label);
                if let Some(init) = init {
                    let init_id = self.visit_expr(init);
                    self.define_edge(&decl_id, &init_id, Some("initializer"));
                }
            }
            DeclKind::Func { params, body } => {
                let name = self.interner.lookup(decl.name.raw);
                let label = format!(
                    "FuncDecl\nname: {}\n(int assumed)\nstorage: {:?}\nsema: {:?}",
                    name,
                    decl.storage,
                    decl.name.sema.get()
                );
                self.define_node(&decl_id, &label);

                let params = self.ast.params(params);
                if !params.is_empty() {
                    let params_id = self.fresh_aux_node_id("params");
                    self.define_node(&params_id, "Parameters");
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
                        self.define_node(&fwd_decl_id, "Forward Declaration");
                        self.define_edge_dotted(&decl_id, &fwd_decl_id, Some("body (forward)"));
                    }
                    Some(body) => {
                        let body_id = self.fresh_aux_node_id("func_body");
                        self.define_node(&body_id, "Function Body");
                        self.define_edge(&decl_id, &body_id, Some("body"));
                        for (idx, item) in self.ast.block_items(body).iter().enumerate() {
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
        match &self.ast.stmt(stmt).kind {
            StmtKind::Empty => self.define_node(&stmt_id, "EmptyStmt"),
            StmtKind::Break => self.define_node(&stmt_id, "BreakStmt"),
            StmtKind::Continue => self.define_node(&stmt_id, "ContinueStmt"),
            StmtKind::Expr(expr) => {
                self.define_node(&stmt_id, "ExprStmt");
                let expr_id = self.visit_expr(*expr);
                self.define_edge(&stmt_id, &expr_id, None);
            }
            StmtKind::Goto(name) => {
                let name = self.interner.lookup(*name);
                let label = format!("GotoStmt\nlabel: {}", name);
                self.define_node(&stmt_id, &label);
            }
            StmtKind::Return(expr) => {
                self.define_node(&stmt_id, "ReturnStmt");
                let expr_id = self.visit_expr(*expr);
                self.define_edge(&stmt_id, &expr_id, Some("value"));
            }
            StmtKind::Default(inner) => {
                self.define_node(&stmt_id, "DefaultStmt (Switch)");
                let inner_id = self.visit_stmt(*inner);
                self.define_edge(&stmt_id, &inner_id, Some("stmt"));
            }
            StmtKind::Label { label, stmt: inner } => {
                let name = self.interner.lookup(*label);
                let label = format!("LabelStmt\nlabel: {}", name);
                self.define_node(&stmt_id, &label);
                let inner_id = self.visit_stmt(*inner);
                self.define_edge(&stmt_id, &inner_id, None);
            }
            StmtKind::Case { expr, stmt: inner } => {
                self.define_node(&stmt_id, "CaseStmt (Switch)");
                let expr_id = self.visit_expr(*expr);
                self.define_edge(&stmt_id, &expr_id, Some("condition"));
                let inner_id = self.visit_stmt(*inner);
                self.define_edge(&stmt_id, &inner_id, Some("stmt"));
            }
            StmtKind::Switch { cond, body } => {
                self.define_node(&stmt_id, "SwitchStmt");
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&stmt_id, &cond_id, Some("condition"));
                let body_id = self.visit_stmt(*body);
                self.define_edge(&stmt_id, &body_id, Some("body"));
            }
            StmtKind::While { cond, body } => {
                self.define_node(&stmt_id, "WhileStmt");
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&stmt_id, &cond_id, Some("condition"));
                let body_id = self.visit_stmt(*body);
                self.define_edge(&stmt_id, &body_id, Some("body"));
            }
            StmtKind::DoWhile { body, cond } => {
                self.define_node(&stmt_id, "DoWhileStmt");
                let body_id = self.visit_stmt(*body);
                self.define_edge(&stmt_id, &body_id, Some("body"));
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&stmt_id, &cond_id, Some("condition"));
            }
            StmtKind::If {
                cond,
                then,
                otherwise,
            } => {
                self.define_node(&stmt_id, "IfStmt");
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&stmt_id, &cond_id, Some("condition"));
                let then_id = self.visit_stmt(*then);
                self.define_edge(&stmt_id, &then_id, Some("then_branch"));
                if let Some(otherwise) = otherwise {
                    let otherwise_id = self.visit_stmt(*otherwise);
                    self.define_edge(&stmt_id, &otherwise_id, Some("else_branch"));
                }
            }
            StmtKind::Compound(items) => {
                self.define_node(&stmt_id, "CompoundStmt (Block)");
                let items = self.ast.block_items(*items);
                if items.is_empty() {
                    let empty_marker_id = format!("{stmt_id}_empty_marker");
                    self.define_node(&empty_marker_id, "(empty block)");
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
            StmtKind::For {
                init,
                cond,
                step,
                body,
            } => {
                self.define_node(&stmt_id, "ForStmt");
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
        match &self.ast.expr(expr).kind {
            ExprKind::Const(val) => {
                let label = format!("Const\nvalue: {val}");
                self.define_node(&expr_id, &label);
            }
            ExprKind::Grouped(inner) => {
                self.define_node(&expr_id, "GroupedExpr");
                let inner_id = self.visit_expr(*inner);
                self.define_edge(&expr_id, &inner_id, None);
            }
            ExprKind::Var(name) => {
                let n = self.interner.lookup(name.raw);
                let label = format!("VarRef\nname: {}\nsema: {:?}", n, name.sema.get());
                self.define_node(&expr_id, &label);
            }
            ExprKind::Unary { op, expr: inner } => {
                let label = format!("UnaryOp\nop: {:?}", op);
                self.define_node(&expr_id, &label);
                let inner_id = self.visit_expr(*inner);
                self.define_edge(&expr_id, &inner_id, Some("operand"));
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let label = format!("BinaryOp\nop: {:?}", op);
                self.define_node(&expr_id, &label);
                let lhs_id = self.visit_expr(*lhs);
                self.define_edge(&expr_id, &lhs_id, Some("lhs"));
                let rhs_id = self.visit_expr(*rhs);
                self.define_edge(&expr_id, &rhs_id, Some("rhs"));
            }
            ExprKind::Ternary {
                cond,
                then,
                otherwise,
            } => {
                self.define_node(&expr_id, "TernaryOp");
                let cond_id = self.visit_expr(*cond);
                self.define_edge(&expr_id, &cond_id, Some("condition"));
                let then_id = self.visit_expr(*then);
                self.define_edge(&expr_id, &then_id, Some("then_expr"));
                let otherwise_id = self.visit_expr(*otherwise);
                self.define_edge(&expr_id, &otherwise_id, Some("else_expr"));
            }
            ExprKind::Call { name, args } => {
                let n = self.interner.lookup(name.raw);
                let label = format!("FunctionCall\nname: {}\nsema: {:?}", n, name.sema.get());
                self.define_node(&expr_id, &label);

                let args = self.ast.args(*args);
                if !args.is_empty() {
                    let args_id = self.fresh_aux_node_id("args");
                    self.define_node(&args_id, "Arguments");
                    self.define_edge(&expr_id, &args_id, Some("args"));
                    for (idx, arg) in args.iter().enumerate() {
                        let arg_id = self.visit_expr(*arg);
                        let label = format!("arg {idx}");
                        self.define_edge(&args_id, &arg_id, Some(&label));
                    }
                } else {
                    let no_args_id = format!("{expr_id}_no_args_marker");
                    self.define_node(&no_args_id, "(no arguments)");
                    self.define_edge_dotted(&expr_id, &no_args_id, None);
                }
            }
        }
        expr_id
    }
}
