use crate::ast::{
    Ast, BlockItem, DeclKind, DeclRef, ExprKind, ExprRef, ForInit, StmtKind, StmtRef,
};

use jcc_ssa::{infra::emitter::IndentedEmitter, interner::Interner};

use std::fmt::Write;

pub struct AstGraphviz<'a> {
    ast: &'a Ast,
    node_counter: u32,
    interner: &'a Interner,
    e: IndentedEmitter<String>,
}

impl<'a> AstGraphviz<'a> {
    pub fn new(ast: &'a Ast, interner: &'a Interner) -> Self {
        AstGraphviz {
            ast,
            interner,
            node_counter: 0,
            e: IndentedEmitter::new(String::with_capacity(1024)),
        }
    }

    pub fn emit(mut self) -> Result<String, std::fmt::Error> {
        writeln!(self.e, "digraph AST {{")?;
        self.indented(|s| {
            writeln!(s.e, "rankdir=TB;")?;
            writeln!(
                s.e,
                "node [shape=box, style=\"rounded,filled\", fontname=\"Helvetica\", fontsize=10];",
            )?;
            writeln!(s.e, "edge [fontname=\"Helvetica\", fontsize=9];")?;
            s.define_node("ast_root", "ASTRoot", "lightblue")?;
            s.ast.root().iter().try_for_each(|decl_ref| {
                let id = s.visit_decl(*decl_ref)?;
                s.define_edge("ast_root", &id, None)
            })?;
            Ok(())
        })?;
        writeln!(self.e, "}}")?;
        Ok(self.e.into_inner())
    }

    fn visit_decl(&mut self, decl_ref: DeclRef) -> Result<String, std::fmt::Error> {
        let decl_id = format!("decl_{}", decl_ref.0.get());
        let decl = self.ast.decl(decl_ref);
        match decl.kind {
            DeclKind::Var(init) => {
                let name = self.interner.lookup(decl.name.raw).escape_default();
                let label = format!(
                    "VarDecl\\nname: {}\\ntype: {:?}\\nstorage: {:?}\\nsema: {:?}",
                    name,
                    decl.ty,
                    decl.storage,
                    decl.name.sema.get()
                );
                self.define_node(&decl_id, &label, "lightgoldenrodyellow")?;
                if let Some(init) = init {
                    let init_id = self.visit_expr(init)?;
                    self.define_edge(&decl_id, &init_id, Some("initializer"))?;
                }
            }
            DeclKind::Func { params, body } => {
                let name = self.interner.lookup(decl.name.raw).escape_default();
                let label = format!(
                    "FuncDecl\\nname: {}\\ntype: {:?}\\nstorage: {:?}\\nsema: {:?}",
                    name,
                    decl.ty,
                    decl.storage,
                    decl.name.sema.get()
                );
                self.define_node(&decl_id, &label, "palegreen")?;

                let params = self.ast.decls(params);
                if !params.is_empty() {
                    let params_id = self.fresh_aux_node_id("params");
                    self.define_node(&params_id, "Parameters", "aliceblue")?;
                    self.define_edge(&decl_id, &params_id, Some("params"))?;
                    for (idx, param) in params.iter().enumerate() {
                        let param_id = self.visit_decl(*param)?;
                        let edge_label = format!("param {idx}");
                        self.define_edge(&params_id, &param_id, Some(&edge_label))?;
                    }
                }

                match body {
                    None => {
                        let fwd_decl_id = format!("{decl_id}_fwd");
                        writeln!(self.e, "{fwd_decl_id} [label=\"Forward Declaration\", shape=plaintext, fillcolor=none];")?;
                        self.define_edge_dotted(&decl_id, &fwd_decl_id, Some("body (forward)"))?;
                    }
                    Some(body) => {
                        let body_id = self.fresh_aux_node_id("func_body");
                        self.define_node(&body_id, "Function Body", "whitesmoke")?;
                        self.define_edge(&decl_id, &body_id, Some("body"))?;
                        for (idx, item) in self.ast.bitems(body).iter().enumerate() {
                            let item_id = match item {
                                BlockItem::Decl(decl) => self.visit_decl(*decl)?,
                                BlockItem::Stmt(stmt) => self.visit_stmt(*stmt)?,
                            };
                            let edge_label = format!("item {idx}");
                            self.define_edge(&body_id, &item_id, Some(&edge_label))?;
                        }
                    }
                }
            }
        }
        Ok(decl_id)
    }

    fn visit_stmt(&mut self, stmt: StmtRef) -> Result<String, std::fmt::Error> {
        let stmt_id = format!("stmt_{}", stmt.0.get());
        match &self.ast.stmt(stmt).kind {
            StmtKind::Empty => self.define_node(&stmt_id, "EmptyStmt", "gray90")?,
            StmtKind::Break(_) => self.define_node(&stmt_id, "BreakStmt", "lightcoral")?,
            StmtKind::Continue(_) => self.define_node(&stmt_id, "ContinueStmt", "lightsalmon")?,
            StmtKind::Expr(expr) => {
                self.define_node(&stmt_id, "ExprStmt", "azure")?;
                let expr_id = self.visit_expr(*expr)?;
                self.define_edge(&stmt_id, &expr_id, None)?;
            }
            StmtKind::Return(expr) => {
                self.define_node(&stmt_id, "ReturnStmt", "mediumpurple1")?;
                let expr_id = self.visit_expr(*expr)?;
                self.define_edge(&stmt_id, &expr_id, Some("value"))?;
            }
            StmtKind::Default(inner) => {
                self.define_node(&stmt_id, "DefaultStmt (Switch)", "khaki")?;
                let inner_id = self.visit_stmt(*inner)?;
                self.define_edge(&stmt_id, &inner_id, Some("stmt"))?;
            }
            StmtKind::Goto { label, stmt } => {
                let label = self.interner.lookup(*label).escape_default();
                let label = format!("GotoStmt\\nlabel: {}\\nstmt: {:?}", label, stmt.get());
                self.define_node(&stmt_id, &label, "sandybrown")?;
            }
            StmtKind::Label { label, stmt: inner } => {
                let label = self.interner.lookup(*label).escape_default();
                let label = format!("LabelStmt\\nlabel: {}", label);
                self.define_node(&stmt_id, &label, "beige")?;
                let inner_id = self.visit_stmt(*inner)?;
                self.define_edge(&stmt_id, &inner_id, None)?;
            }
            StmtKind::Case { expr, stmt: inner } => {
                self.define_node(&stmt_id, "CaseStmt (Switch)", "khaki")?;
                let expr_id = self.visit_expr(*expr)?;
                self.define_edge(&stmt_id, &expr_id, Some("condition"))?;
                let inner_id = self.visit_stmt(*inner)?;
                self.define_edge(&stmt_id, &inner_id, Some("stmt"))?;
            }
            StmtKind::Switch { cond, body } => {
                self.define_node(&stmt_id, "SwitchStmt", "lightpink")?;
                let cond_id = self.visit_expr(*cond)?;
                self.define_edge(&stmt_id, &cond_id, Some("condition"))?;
                let body_id = self.visit_stmt(*body)?;
                self.define_edge(&stmt_id, &body_id, Some("body"))?;
            }
            StmtKind::While { cond, body } => {
                self.define_node(&stmt_id, "WhileStmt", "paleturquoise")?;
                let cond_id = self.visit_expr(*cond)?;
                self.define_edge(&stmt_id, &cond_id, Some("condition"))?;
                let body_id = self.visit_stmt(*body)?;
                self.define_edge(&stmt_id, &body_id, Some("body"))?;
            }
            StmtKind::DoWhile { body, cond } => {
                self.define_node(&stmt_id, "DoWhileStmt", "paleturquoise1")?;
                let body_id = self.visit_stmt(*body)?;
                self.define_edge(&stmt_id, &body_id, Some("body"))?;
                let cond_id = self.visit_expr(*cond)?;
                self.define_edge(&stmt_id, &cond_id, Some("condition"))?;
            }
            StmtKind::If {
                cond,
                then,
                otherwise,
            } => {
                self.define_node(&stmt_id, "IfStmt", "skyblue")?;
                let cond_id = self.visit_expr(*cond)?;
                self.define_edge(&stmt_id, &cond_id, Some("condition"))?;
                let then_id = self.visit_stmt(*then)?;
                self.define_edge(&stmt_id, &then_id, Some("then_branch"))?;
                if let Some(otherwise) = otherwise {
                    let otherwise_id = self.visit_stmt(*otherwise)?;
                    self.define_edge(&stmt_id, &otherwise_id, Some("else_branch"))?;
                }
            }
            StmtKind::Compound(items) => {
                self.define_node(&stmt_id, "CompoundStmt", "lightcyan")?;
                let items = self.ast.bitems(*items);
                if items.is_empty() {
                    let empty_marker_id = format!("{stmt_id}_empty_marker");
                    writeln!(
                        self.e,
                        "{empty_marker_id} [label=\"(empty block)\", shape=plaintext, fillcolor=none];"
                    )?;
                    self.define_edge(&stmt_id, &empty_marker_id, None)?;
                } else {
                    for (idx, item) in items.iter().enumerate() {
                        let item_id = match item {
                            BlockItem::Decl(decl) => self.visit_decl(*decl)?,
                            BlockItem::Stmt(stmt) => self.visit_stmt(*stmt)?,
                        };
                        let label = format!("item {idx}");
                        self.define_edge(&stmt_id, &item_id, Some(&label))?;
                    }
                }
            }
            StmtKind::For {
                init,
                cond,
                step,
                body,
            } => {
                self.define_node(&stmt_id, "ForStmt", "thistle")?;
                if let Some(init) = init {
                    match init {
                        ForInit::Expr(expr) => {
                            let init_id = self.visit_expr(*expr)?;
                            self.define_edge(&stmt_id, &init_id, Some("initializer (expr)"))?;
                        }
                        ForInit::VarDecl(decl) => {
                            let decl_id = self.visit_decl(*decl)?;
                            self.define_edge(&stmt_id, &decl_id, Some("initializer (decl)"))?;
                        }
                    }
                }
                if let Some(cond) = cond {
                    let cond_id = self.visit_expr(*cond)?;
                    self.define_edge(&stmt_id, &cond_id, Some("condition"))?;
                }
                if let Some(step) = step {
                    let step_id = self.visit_expr(*step)?;
                    self.define_edge(&stmt_id, &step_id, Some("step"))?;
                }
                let body_id = self.visit_stmt(*body)?;
                self.define_edge(&stmt_id, &body_id, Some("body"))?;
            }
        }
        Ok(stmt_id)
    }

    fn visit_expr(&mut self, expr: ExprRef) -> Result<String, std::fmt::Error> {
        let expr_id = format!("expr_{}", expr.0.get());
        match &self.ast.expr(expr).kind {
            ExprKind::Const(val) => {
                let label = format!("Const\\nvalue: {:?}", val);
                self.define_node(&expr_id, &label, "gold")?;
            }
            ExprKind::Grouped(inner) => {
                self.define_node(&expr_id, "GroupedExpr", "lightgrey")?;
                let inner_id = self.visit_expr(*inner)?;
                self.define_edge(&expr_id, &inner_id, None)?;
            }
            ExprKind::Var(name) => {
                let n = self.interner.lookup(name.raw).escape_default();
                let label = format!("VarRef\\nname: {}\\nsema: {:?}", n, name.sema.get());
                self.define_node(&expr_id, &label, "olivedrab1")?;
            }
            ExprKind::Cast { ty, expr: inner } => {
                let label = format!("Cast\\nto: {:?}", ty);
                self.define_node(&expr_id, &label, "lightyellow")?;
                let inner_id = self.visit_expr(*inner)?;
                self.define_edge(&expr_id, &inner_id, Some("operand"))?;
            }
            ExprKind::Unary { op, expr: inner } => {
                let label = format!("UnaryOp\\nop: {:?}", op);
                self.define_node(&expr_id, &label, "coral")?;
                let inner_id = self.visit_expr(*inner)?;
                self.define_edge(&expr_id, &inner_id, Some("operand"))?;
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let label = format!("BinaryOp\\nop: {:?}", op);
                self.define_node(&expr_id, &label, "orchid")?;
                let lhs_id = self.visit_expr(*lhs)?;
                self.define_edge(&expr_id, &lhs_id, Some("lhs"))?;
                let rhs_id = self.visit_expr(*rhs)?;
                self.define_edge(&expr_id, &rhs_id, Some("rhs"))?;
            }
            ExprKind::Ternary { cond, then, other } => {
                self.define_node(&expr_id, "TernaryOp", "mediumspringgreen")?;
                let cond_id = self.visit_expr(*cond)?;
                self.define_edge(&expr_id, &cond_id, Some("condition"))?;
                let then_id = self.visit_expr(*then)?;
                self.define_edge(&expr_id, &then_id, Some("then_expr"))?;
                let otherwise_id = self.visit_expr(*other)?;
                self.define_edge(&expr_id, &otherwise_id, Some("else_expr"))?;
            }
            ExprKind::Call { name, args } => {
                let n = self.interner.lookup(name.raw).escape_default();
                let label = format!("FunctionCall\\nname: {}\\nsema: {:?}", n, name.sema.get());
                self.define_node(&expr_id, &label, "deepskyblue")?;

                let args = self.ast.exprs(*args);
                if !args.is_empty() {
                    let args_id = self.fresh_aux_node_id("args");
                    self.define_node(&args_id, "Arguments", "aliceblue")?;
                    self.define_edge(&expr_id, &args_id, Some("args"))?;
                    for (idx, arg) in args.iter().enumerate() {
                        let arg_id = self.visit_expr(*arg)?;
                        let label = format!("arg {idx}");
                        self.define_edge(&args_id, &arg_id, Some(&label))?;
                    }
                } else {
                    let no_args_id = format!("{expr_id}_no_args_marker");
                    writeln!(
                        self.e,
                        "{no_args_id} [label=\"(no arguments)\", shape=plaintext, fillcolor=none];"
                    )?;
                    self.define_edge_dotted(&expr_id, &no_args_id, None)?;
                }
            }
        }
        Ok(expr_id)
    }

    // ---------------------------------------------------------------------------
    // Auxiliary methods
    // ---------------------------------------------------------------------------

    #[inline]
    pub fn indented<F>(&mut self, f: F) -> std::fmt::Result
    where
        F: FnOnce(&mut Self) -> std::fmt::Result,
    {
        self.e.indent();
        let res = f(self);
        self.e.unindent();
        res
    }

    #[inline]
    fn fresh_aux_node_id(&mut self, hint: &str) -> String {
        let id = self.node_counter;
        self.node_counter += 1;
        format!("aux_{hint}_{id}")
    }

    #[inline]
    fn define_node(&mut self, id: &str, label: &str, fillcolor: &str) -> std::fmt::Result {
        writeln!(self.e, "{id} [label=\"{label}\", fillcolor={fillcolor}];")
    }

    #[inline]
    fn define_edge(&mut self, from_id: &str, to_id: &str, label: Option<&str>) -> std::fmt::Result {
        match label {
            None => writeln!(self.e, "{from_id} -> {to_id};"),
            Some(label) => writeln!(self.e, "{from_id} -> {to_id} [label=\"{label}\"];",),
        }
    }

    #[inline]
    fn define_edge_dotted(
        &mut self,
        from_id: &str,
        to_id: &str,
        label: Option<&str>,
    ) -> std::fmt::Result {
        match label {
            None => writeln!(self.e, "{from_id} -> {to_id} [style=dotted];"),
            Some(label) => writeln!(
                self.e,
                "{from_id} -> {to_id} [label=\"{label}\", style=dotted];"
            ),
        }
    }
}
