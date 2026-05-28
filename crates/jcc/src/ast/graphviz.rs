use crate::ast::{Ast, BlockItem, Decl, DeclKind, Expr, ExprKind, ForInit, Stmt, StmtKind};

use jcc_backend::IdentInterner;

// ---------------------------------------------------------------------------
// AstGraphvizCtx
// ---------------------------------------------------------------------------

pub struct AstGraphvizCtx<'a, 'ctx> {
    ast: &'a Ast<'ctx>,
    interner: &'a IdentInterner,
}

impl std::fmt::Display for AstGraphvizCtx<'_, '_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        AstGraphvizEmitter::new(self, f).emit()
    }
}

impl<'a, 'ctx> AstGraphvizCtx<'a, 'ctx> {
    pub fn new(ast: &'a Ast<'ctx>, interner: &'a IdentInterner) -> Self {
        AstGraphvizCtx { ast, interner }
    }
}

// ---------------------------------------------------------------------------
// AstGraphvizEmitter
// ---------------------------------------------------------------------------

struct AstGraphvizEmitter<'a, 'ctx, W: std::fmt::Write> {
    dst: W,
    counter: u32,
    ctx: &'a AstGraphvizCtx<'a, 'ctx>,
}

impl<'a, 'ctx, W: std::fmt::Write> AstGraphvizEmitter<'a, 'ctx, W> {
    fn new(ctx: &'a AstGraphvizCtx<'a, 'ctx>, dst: W) -> Self {
        Self {
            dst,
            ctx,
            counter: 0,
        }
    }

    fn emit(mut self) -> std::fmt::Result {
        writeln!(self.dst, "digraph AST {{")?;
        writeln!(self.dst, "rankdir=TB;")?;
        writeln!(
            self.dst,
            "node [shape=box, style=\"rounded,filled\", fontname=\"Helvetica\", fontsize=10];",
        )?;
        writeln!(self.dst, "edge [fontname=\"Helvetica\", fontsize=9];")?;
        self.emit_node("ast_root", "ASTRoot", Some(Color::LightBlue))?;
        self.ctx.ast.root.iter().try_for_each(|decl| {
            let id = self.emit_decl(*decl)?;
            self.emit_edge("ast_root", &id, None)
        })?;
        writeln!(self.dst, "}}")
    }

    fn emit_decl(&mut self, decl: Decl) -> Result<String, std::fmt::Error> {
        let id = decl.to_string();
        let data = &self.ctx.ast.decl[decl];
        match data.kind {
            DeclKind::Var(init) => {
                let name = self.ctx.interner.lookup(data.name.name).escape_default();
                let label = format!(
                    "VarDecl\\nname: {}\\ntype: {:?}\\nstorage: {:?}\\nsema: {:?}",
                    name,
                    data.ty,
                    data.storage,
                    data.name.sema.get()
                );
                self.emit_node(&id, &label, Some(Color::LightGoldenrodYellow))?;
                if let Some(init) = init {
                    let init_id = self.emit_expr(init)?;
                    self.emit_edge(&id, &init_id, Some("initializer"))?;
                }
            }
            DeclKind::Func { params, body } => {
                let name = self.ctx.interner.lookup(data.name.name).escape_default();
                let label = format!(
                    "FuncDecl\\nname: {}\\ntype: {:?}\\nstorage: {:?}\\nsema: {:?}",
                    name,
                    data.ty,
                    data.storage,
                    data.name.sema.get()
                );
                self.emit_node(&id, &label, Some(Color::PaleGreen))?;

                let params = &self.ctx.ast.decls[params];
                if !params.is_empty() {
                    let params_id = self.fresh_aux_node_id("params");
                    self.emit_node(&params_id, "Parameters", Some(Color::AliceBlue))?;
                    self.emit_edge(&id, &params_id, Some("params"))?;
                    for (idx, param) in params.iter().enumerate() {
                        let param_id = self.emit_decl(*param)?;
                        let edge_label = format!("param {idx}");
                        self.emit_edge(&params_id, &param_id, Some(&edge_label))?;
                    }
                }

                match body {
                    None => {
                        let fwd_decl_id = format!("{id}_fwd");
                        self.emit_node(&fwd_decl_id, "Forward Declaration", None)?;
                        self.emit_edge_dotted(&id, &fwd_decl_id, Some("body (forward)"))?;
                    }
                    Some(body) => {
                        let body_id = self.fresh_aux_node_id("func_body");
                        self.emit_node(&body_id, "Function Body", Some(Color::WhiteSmoke))?;
                        self.emit_edge(&id, &body_id, Some("body"))?;
                        for (idx, item) in self.ctx.ast.items[body].iter().enumerate() {
                            let item_id = match item {
                                BlockItem::Decl(decl) => self.emit_decl(*decl)?,
                                BlockItem::Stmt(stmt) => self.emit_stmt(*stmt)?,
                            };
                            let edge_label = format!("item {idx}");
                            self.emit_edge(&body_id, &item_id, Some(&edge_label))?;
                        }
                    }
                }
            }
        }
        Ok(id)
    }

    fn emit_stmt(&mut self, stmt: Stmt) -> Result<String, std::fmt::Error> {
        let id = stmt.to_string();
        match &self.ctx.ast.stmt[stmt].kind {
            StmtKind::Empty => self.emit_node(&id, "EmptyStmt", Some(Color::Gray90))?,
            StmtKind::Expr(expr) => {
                self.emit_node(&id, "ExprStmt", Some(Color::Azure))?;
                let expr_id = self.emit_expr(*expr)?;
                self.emit_edge(&id, &expr_id, None)?;
            }
            StmtKind::Break(_) => self.emit_node(&id, "BreakStmt", Some(Color::LightCoral))?,
            StmtKind::Continue(_) => {
                self.emit_node(&id, "ContinueStmt", Some(Color::LightSalmon))?
            }
            StmtKind::Return(expr) => {
                self.emit_node(&id, "ReturnStmt", Some(Color::MediumPurple1))?;
                let expr_id = self.emit_expr(*expr)?;
                self.emit_edge(&id, &expr_id, Some("value"))?;
            }
            StmtKind::Default(inner) => {
                self.emit_node(&id, "DefaultStmt (Switch)", Some(Color::Khaki))?;
                let inner_id = self.emit_stmt(*inner)?;
                self.emit_edge(&id, &inner_id, Some("stmt"))?;
            }
            StmtKind::Goto { label, stmt } => {
                let label = self.ctx.interner.lookup(*label).escape_default();
                let label = format!("GotoStmt\\nlabel: {}\\nstmt: {:?}", label, stmt.get());
                self.emit_node(&id, &label, Some(Color::SandyBrown))?;
            }
            StmtKind::Label { label, stmt: inner } => {
                let label = self.ctx.interner.lookup(*label).escape_default();
                let label = format!("LabelStmt\\nlabel: {}", label);
                self.emit_node(&id, &label, Some(Color::Beige))?;
                let inner_id = self.emit_stmt(*inner)?;
                self.emit_edge(&id, &inner_id, None)?;
            }
            StmtKind::Switch { cond, body } => {
                self.emit_node(&id, "SwitchStmt", Some(Color::LightPink))?;
                let cond_id = self.emit_expr(*cond)?;
                self.emit_edge(&id, &cond_id, Some("condition"))?;
                let body_id = self.emit_stmt(*body)?;
                self.emit_edge(&id, &body_id, Some("body"))?;
            }
            StmtKind::Case { expr, stmt: inner } => {
                self.emit_node(&id, "CaseStmt (Switch)", Some(Color::Khaki))?;
                let expr_id = self.emit_expr(*expr)?;
                self.emit_edge(&id, &expr_id, Some("condition"))?;
                let inner_id = self.emit_stmt(*inner)?;
                self.emit_edge(&id, &inner_id, Some("stmt"))?;
            }
            StmtKind::While { cond, body } => {
                self.emit_node(&id, "WhileStmt", Some(Color::PaleTurquoise))?;
                let cond_id = self.emit_expr(*cond)?;
                self.emit_edge(&id, &cond_id, Some("condition"))?;
                let body_id = self.emit_stmt(*body)?;
                self.emit_edge(&id, &body_id, Some("body"))?;
            }
            StmtKind::DoWhile { body, cond } => {
                self.emit_node(&id, "DoWhileStmt", Some(Color::PaleTurquoise1))?;
                let body_id = self.emit_stmt(*body)?;
                self.emit_edge(&id, &body_id, Some("body"))?;
                let cond_id = self.emit_expr(*cond)?;
                self.emit_edge(&id, &cond_id, Some("condition"))?;
            }
            StmtKind::If {
                cond,
                then,
                otherwise,
            } => {
                self.emit_node(&id, "IfStmt", Some(Color::SkyBlue))?;
                let cond_id = self.emit_expr(*cond)?;
                self.emit_edge(&id, &cond_id, Some("condition"))?;
                let then_id = self.emit_stmt(*then)?;
                self.emit_edge(&id, &then_id, Some("then_branch"))?;
                if let Some(otherwise) = otherwise {
                    let otherwise_id = self.emit_stmt(*otherwise)?;
                    self.emit_edge(&id, &otherwise_id, Some("else_branch"))?;
                }
            }
            StmtKind::Compound(items) => {
                self.emit_node(&id, "CompoundStmt", Some(Color::LightCyan))?;
                let items = &self.ctx.ast.items[*items];
                if items.is_empty() {
                    let empty_marker_id = format!("{id}_empty_marker");
                    self.emit_node(&empty_marker_id, "(empty block)", None)?;
                    self.emit_edge(&id, &empty_marker_id, None)?;
                } else {
                    for (idx, item) in items.iter().enumerate() {
                        let item_id = match item {
                            BlockItem::Decl(decl) => self.emit_decl(*decl)?,
                            BlockItem::Stmt(stmt) => self.emit_stmt(*stmt)?,
                        };
                        let label = format!("item {idx}");
                        self.emit_edge(&id, &item_id, Some(&label))?;
                    }
                }
            }
            StmtKind::For {
                init,
                cond,
                step,
                body,
            } => {
                self.emit_node(&id, "ForStmt", Some(Color::Thistle))?;
                if let Some(init) = init {
                    match init {
                        ForInit::Expr(expr) => {
                            let init_id = self.emit_expr(*expr)?;
                            self.emit_edge(&id, &init_id, Some("initializer (expr)"))?;
                        }
                        ForInit::VarDecl(decl) => {
                            let decl_id = self.emit_decl(*decl)?;
                            self.emit_edge(&id, &decl_id, Some("initializer (decl)"))?;
                        }
                    }
                }
                if let Some(cond) = cond {
                    let cond_id = self.emit_expr(*cond)?;
                    self.emit_edge(&id, &cond_id, Some("condition"))?;
                }
                if let Some(step) = step {
                    let step_id = self.emit_expr(*step)?;
                    self.emit_edge(&id, &step_id, Some("step"))?;
                }
                let body_id = self.emit_stmt(*body)?;
                self.emit_edge(&id, &body_id, Some("body"))?;
            }
        }
        Ok(id)
    }

    fn emit_expr(&mut self, expr: Expr) -> Result<String, std::fmt::Error> {
        let id = expr.to_string();
        match &self.ctx.ast.expr[expr].kind {
            ExprKind::Const(val) => {
                let label = format!("Const\\nvalue: {:?}", val);
                self.emit_node(&id, &label, Some(Color::Gold))?;
            }
            ExprKind::Grouped(inner) => {
                self.emit_node(&id, "GroupedExpr", Some(Color::LightGrey))?;
                let inner_id = self.emit_expr(*inner)?;
                self.emit_edge(&id, &inner_id, None)?;
            }
            ExprKind::Var(name) => {
                let n = self.ctx.interner.lookup(name.name).escape_default();
                let label = format!("VarRef\\nname: {}\\nsema: {:?}", n, name.sema.get());
                self.emit_node(&id, &label, Some(Color::OliveDrab1))?;
            }
            ExprKind::Cast { ty, expr: inner } => {
                let label = format!("Cast\\nto: {:?}", ty);
                self.emit_node(&id, &label, Some(Color::LightYellow))?;
                let inner_id = self.emit_expr(*inner)?;
                self.emit_edge(&id, &inner_id, Some("operand"))?;
            }
            ExprKind::Unary { op, expr: inner } => {
                let label = format!("UnaryOp\\nop: {:?}", op);
                self.emit_node(&id, &label, Some(Color::Coral))?;
                let inner_id = self.emit_expr(*inner)?;
                self.emit_edge(&id, &inner_id, Some("operand"))?;
            }
            ExprKind::Binary { op, lhs, rhs } => {
                let label = format!("BinaryOp\\nop: {:?}", op);
                self.emit_node(&id, &label, Some(Color::Orchid))?;
                let lhs_id = self.emit_expr(*lhs)?;
                self.emit_edge(&id, &lhs_id, Some("lhs"))?;
                let rhs_id = self.emit_expr(*rhs)?;
                self.emit_edge(&id, &rhs_id, Some("rhs"))?;
            }
            ExprKind::Ternary { cond, then, other } => {
                self.emit_node(&id, "TernaryOp", Some(Color::MediumSpringGreen))?;
                let cond_id = self.emit_expr(*cond)?;
                self.emit_edge(&id, &cond_id, Some("condition"))?;
                let then_id = self.emit_expr(*then)?;
                self.emit_edge(&id, &then_id, Some("then_expr"))?;
                let otherwise_id = self.emit_expr(*other)?;
                self.emit_edge(&id, &otherwise_id, Some("else_expr"))?;
            }
            ExprKind::Call { name, args } => {
                let n = self.ctx.interner.lookup(name.name).escape_default();
                let label = format!("FunctionCall\\nname: {}\\nsema: {:?}", n, name.sema.get());
                self.emit_node(&id, &label, Some(Color::DeepSkyBlue))?;

                let args = &self.ctx.ast.exprs[*args];
                if !args.is_empty() {
                    let args_id = self.fresh_aux_node_id("args");
                    self.emit_node(&args_id, "Arguments", Some(Color::AliceBlue))?;
                    self.emit_edge(&id, &args_id, Some("args"))?;
                    for (idx, arg) in args.iter().enumerate() {
                        let arg_id = self.emit_expr(*arg)?;
                        let label = format!("arg {idx}");
                        self.emit_edge(&args_id, &arg_id, Some(&label))?;
                    }
                } else {
                    let no_args_id = format!("{id}_no_args_marker");
                    self.emit_node(&no_args_id, "(no arguments)", None)?;
                    self.emit_edge_dotted(&id, &no_args_id, None)?;
                }
            }
        }
        Ok(id)
    }

    // ---------------------------------------------------------------------------
    // Auxiliary methods
    // ---------------------------------------------------------------------------

    #[inline]
    fn fresh_aux_node_id(&mut self, hint: &str) -> String {
        let id = self.counter;
        self.counter += 1;
        format!("aux_{hint}_{id}")
    }

    #[inline]
    fn emit_node(&mut self, id: &str, label: &str, color: Option<Color>) -> std::fmt::Result {
        match color {
            Some(color) => writeln!(
                self.dst,
                "{id} [label=\"{label}\", fillcolor={}];",
                color.as_str()
            ),
            None => writeln!(
                self.dst,
                "{id} [label=\"{label}\", shape=plaintext, fillcolor=none];"
            ),
        }
    }

    #[inline]
    fn emit_edge(&mut self, from_id: &str, to_id: &str, label: Option<&str>) -> std::fmt::Result {
        match label {
            None => writeln!(self.dst, "{from_id} -> {to_id};"),
            Some(label) => writeln!(self.dst, "{from_id} -> {to_id} [label=\"{label}\"];",),
        }
    }

    #[inline]
    fn emit_edge_dotted(
        &mut self,
        from_id: &str,
        to_id: &str,
        label: Option<&str>,
    ) -> std::fmt::Result {
        match label {
            None => writeln!(self.dst, "{from_id} -> {to_id} [style=dotted];"),
            Some(label) => writeln!(
                self.dst,
                "{from_id} -> {to_id} [label=\"{label}\", style=dotted];"
            ),
        }
    }
}

// ---------------------------------------------------------------------------
// Color
// ---------------------------------------------------------------------------

enum Color {
    AliceBlue,
    Azure,
    Beige,
    Coral,
    DeepSkyBlue,
    Gold,
    Gray90,
    Khaki,
    LightBlue,
    LightCoral,
    LightCyan,
    LightGoldenrodYellow,
    LightGrey,
    LightPink,
    LightSalmon,
    LightYellow,
    MediumPurple1,
    MediumSpringGreen,
    OliveDrab1,
    Orchid,
    PaleGreen,
    PaleTurquoise,
    PaleTurquoise1,
    SandyBrown,
    SkyBlue,
    Thistle,
    WhiteSmoke,
}

impl Color {
    fn as_str(&self) -> &'static str {
        match self {
            Color::AliceBlue => "aliceblue",
            Color::Azure => "azure",
            Color::Beige => "beige",
            Color::Coral => "coral",
            Color::DeepSkyBlue => "deepskyblue",
            Color::Gold => "gold",
            Color::Gray90 => "gray90",
            Color::Khaki => "khaki",
            Color::LightBlue => "lightblue",
            Color::LightCoral => "lightcoral",
            Color::LightCyan => "lightcyan",
            Color::LightGoldenrodYellow => "lightgoldenrodyellow",
            Color::LightGrey => "lightgrey",
            Color::LightPink => "lightpink",
            Color::LightSalmon => "lightsalmon",
            Color::LightYellow => "lightyellow",
            Color::MediumPurple1 => "mediumpurple1",
            Color::MediumSpringGreen => "mediumspringgreen",
            Color::OliveDrab1 => "olivedrab1",
            Color::Orchid => "orchid",
            Color::PaleGreen => "palegreen",
            Color::PaleTurquoise => "paleturquoise",
            Color::PaleTurquoise1 => "paleturquoise1",
            Color::SandyBrown => "sandybrown",
            Color::SkyBlue => "skyblue",
            Color::Thistle => "thistle",
            Color::WhiteSmoke => "whitesmoke",
        }
    }
}
