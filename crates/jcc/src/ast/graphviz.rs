use crate::ast::parse::{Ast, Decl, DeclRef, Expr, ExprRef, ForInit, Stmt, StmtRef};

use tacky::{Interner, Symbol};

use std::fmt::Write;

use super::parse::BlockItem;

pub struct AstGraphviz<'a> {
    ast: &'a Ast,
    interner: &'a Interner,
    output: String,
    buffer: String,
    /// Used for nodes that don't have a direct Ref, like "Params" or "Args"
    node_counter: u32,
}

impl<'a> AstGraphviz<'a> {
    pub fn new(ast: &'a Ast, interner: &'a Interner) -> Self {
        AstGraphviz {
            ast,
            interner,
            node_counter: 0,
            output: String::new(),
            buffer: String::new(),
        }
    }

    #[inline]
    fn get_symbol_name(&mut self, symbol: Symbol) {
        self.buffer.clear();
        self.buffer
            .push_str(self.interner.resolve(symbol).unwrap_or("Unknown"));
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
    fn fresh_aux_node_id(&mut self, hint: &str) -> String {
        let id = self.node_counter;
        self.node_counter += 1;
        format!("aux_{}_{}", hint, id)
    }

    pub fn emit(mut self) -> String {
        writeln!(self.output, "digraph AST {{").unwrap();
        writeln!(self.output, "  rankdir=TB;").unwrap(); // Top-to-bottom layout
        writeln!(
            self.output,
            "  node [shape=box, style=\"rounded,filled\", fontname=\"Helvetica\", fontsize=10];"
        )
        .unwrap();
        writeln!(self.output, "  edge [fontname=\"Helvetica\", fontsize=9];").unwrap();

        let root_node_graph_id = "ast_root";
        writeln!(
            self.output,
            "  {} [label=\"ProgramRoot\", fillcolor=lightblue];",
            root_node_graph_id
        )
        .unwrap();

        for decl_ref in self.ast.root() {
            let decl_graph_id = self.visit_decl(*decl_ref);
            writeln!(
                self.output,
                "  {} -> {};",
                root_node_graph_id, decl_graph_id
            )
            .unwrap();
        }

        writeln!(self.output, "}}").unwrap();
        self.output
    }

    fn visit_decl(&mut self, decl_ref: DeclRef) -> String {
        let decl_node_graph_id = self.decl_node_id(decl_ref);
        let decl = self.ast.decl(decl_ref);

        match decl {
            Decl::Var { name, init } => {
                self.get_symbol_name(*name);
                writeln!(
                    self.output,
                    "  {} [label=\"VarDecl\\nname: {}\\n(int assumed)\", fillcolor=lightgoldenrodyellow];",
                    decl_node_graph_id,
                    self.buffer.escape_default()
                )
                .unwrap();
                if let Some(init_expr_ref) = init {
                    let init_expr_graph_id = self.visit_expr(*init_expr_ref);
                    writeln!(
                        self.output,
                        "  {} -> {} [label=\"initializer\"];",
                        decl_node_graph_id, init_expr_graph_id
                    )
                    .unwrap();
                }
            }
            Decl::Func { name, params, body } => {
                self.get_symbol_name(*name);
                writeln!(
                    self.output,
                    "  {} [label=\"FuncDecl\\nname: {}\\n(int assumed)\", fillcolor=palegreen];",
                    decl_node_graph_id,
                    self.buffer.escape_default()
                )
                .unwrap();

                let params_slice = self.ast.params(*params);
                if !params_slice.is_empty() {
                    let params_list_node_id = self.fresh_aux_node_id("params");
                    writeln!(
                        self.output,
                        "  {} [label=\"Parameters\", fillcolor=aliceblue];",
                        params_list_node_id
                    )
                    .unwrap();
                    writeln!(
                        self.output,
                        "  {} -> {} [label=\"params\"];",
                        decl_node_graph_id, params_list_node_id
                    )
                    .unwrap();
                    for (i, param_decl_ref) in params_slice.iter().enumerate() {
                        let param_decl_graph_id = self.visit_decl(*param_decl_ref);
                        writeln!(
                            self.output,
                            "  {} -> {} [label=\"param {}\"];",
                            params_list_node_id, param_decl_graph_id, i
                        )
                        .unwrap();
                    }
                }

                if let Some(body_slice_ref) = body {
                    let body_block_node_id = self.fresh_aux_node_id("func_body");
                    writeln!(
                        self.output,
                        "  {} [label=\"Function Body\", fillcolor=whitesmoke];",
                        body_block_node_id
                    )
                    .unwrap();
                    writeln!(
                        self.output,
                        "  {} -> {} [label=\"body\"];",
                        decl_node_graph_id, body_block_node_id
                    )
                    .unwrap();
                    let block_items = self.ast.block_items(*body_slice_ref);
                    for (i, item) in block_items.iter().enumerate() {
                        let item_graph_id = self.visit_block_item(*item);
                        writeln!(
                            self.output,
                            "  {} -> {} [label=\"item {}\"];",
                            body_block_node_id, item_graph_id, i
                        )
                        .unwrap();
                    }
                } else {
                    writeln!(
                        self.output,
                        "  {} -> {}_fwd [label=\"body (forward)\"];",
                        decl_node_graph_id, decl_node_graph_id
                    )
                    .unwrap();
                    writeln!(
                        self.output,
                        "  {}_fwd [label=\"Forward Declaration\", shape=plaintext, fillcolor=none];",
                         decl_node_graph_id
                    )
                    .unwrap();
                }
            }
        }
        decl_node_graph_id
    }

    fn visit_stmt(&mut self, stmt_ref: StmtRef) -> String {
        let stmt_node_graph_id = self.stmt_node_id(stmt_ref);
        let stmt = self.ast.stmt(stmt_ref);

        let node_label_and_color = match stmt {
            Stmt::Empty => ("EmptyStmt".to_string(), "gray90".to_string()),
            Stmt::Break => ("BreakStmt".to_string(), "lightcoral".to_string()),
            Stmt::Continue => ("ContinueStmt".to_string(), "lightsalmon".to_string()),
            Stmt::Goto(label_sym) => {
                self.get_symbol_name(*label_sym);
                (
                    format!("GotoStmt\\nlabel: {}", self.buffer.escape_default()),
                    "sandybrown".to_string(),
                )
            }
            Stmt::Expr(expr_ref) => {
                let expr_graph_id = self.visit_expr(*expr_ref);
                writeln!(
                    self.output,
                    "  {} [label=\"ExprStmt\", fillcolor=azure];",
                    stmt_node_graph_id,
                )
                .unwrap();
                writeln!(
                    self.output,
                    "  {} -> {};",
                    stmt_node_graph_id, expr_graph_id
                )
                .unwrap();
                return stmt_node_graph_id; // Specific handling
            }
            Stmt::Return(expr_ref) => {
                writeln!(
                    self.output,
                    "  {} [label=\"ReturnStmt\", fillcolor=mediumpurple1];",
                    stmt_node_graph_id
                )
                .unwrap();
                let ret_expr_graph_id = self.visit_expr(*expr_ref);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"value\"];",
                    stmt_node_graph_id, ret_expr_graph_id
                )
                .unwrap();
                return stmt_node_graph_id; // Specific handling
            }
            Stmt::Default(inner_stmt_ref) => {
                writeln!(
                    self.output,
                    "  {} [label=\"DefaultStmt (Switch)\", fillcolor=khaki];",
                    stmt_node_graph_id
                )
                .unwrap();
                let inner_stmt_graph_id = self.visit_stmt(*inner_stmt_ref);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"stmt\"];",
                    stmt_node_graph_id, inner_stmt_graph_id
                )
                .unwrap();
                return stmt_node_graph_id;
            }
            Stmt::Compound(block_items_slice) => {
                writeln!(
                    self.output,
                    "  {} [label=\"CompoundStmt (Block)\", fillcolor=lightcyan];",
                    stmt_node_graph_id
                )
                .unwrap();
                let block_items = self.ast.block_items(*block_items_slice);
                if block_items.is_empty() {
                    let empty_block_node = format!("{}_empty_marker", stmt_node_graph_id);
                    writeln!(
                        self.output,
                        "  {} [label=\"(empty block)\", shape=plaintext, fillcolor=none];",
                        empty_block_node
                    )
                    .unwrap();
                    writeln!(
                        self.output,
                        "  {} -> {};",
                        stmt_node_graph_id, empty_block_node
                    )
                    .unwrap();
                } else {
                    for (i, item) in block_items.iter().enumerate() {
                        let item_graph_id = self.visit_block_item(*item);
                        writeln!(
                            self.output,
                            "  {} -> {} [label=\"item {}\"];",
                            stmt_node_graph_id, item_graph_id, i
                        )
                        .unwrap();
                    }
                }
                return stmt_node_graph_id;
            }
            Stmt::Case {
                expr,
                stmt: case_stmt_ref,
            } => {
                writeln!(
                    self.output,
                    "  {} [label=\"CaseStmt (Switch)\", fillcolor=khaki];",
                    stmt_node_graph_id
                )
                .unwrap();
                let case_expr_graph_id = self.visit_expr(*expr);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"condition\"];",
                    stmt_node_graph_id, case_expr_graph_id
                )
                .unwrap();
                let case_stmt_graph_id = self.visit_stmt(*case_stmt_ref);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"stmt\"];",
                    stmt_node_graph_id, case_stmt_graph_id
                )
                .unwrap();
                return stmt_node_graph_id;
            }
            Stmt::Label {
                label,
                stmt: labeled_stmt_ref,
            } => {
                self.get_symbol_name(*label);
                writeln!(
                    self.output,
                    "  {} [label=\"LabelStmt\\nlabel: {}\", fillcolor=beige];",
                    stmt_node_graph_id,
                    self.buffer.escape_default()
                )
                .unwrap();
                let labeled_stmt_graph_id = self.visit_stmt(*labeled_stmt_ref);
                writeln!(
                    self.output,
                    "  {} -> {};",
                    stmt_node_graph_id, labeled_stmt_graph_id
                )
                .unwrap();
                return stmt_node_graph_id;
            }
            Stmt::Switch { cond, body } => {
                writeln!(
                    self.output,
                    "  {} [label=\"SwitchStmt\", fillcolor=lightpink];",
                    stmt_node_graph_id
                )
                .unwrap();
                let cond_expr_graph_id = self.visit_expr(*cond);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"condition\"];",
                    stmt_node_graph_id, cond_expr_graph_id
                )
                .unwrap();
                let body_stmt_graph_id = self.visit_stmt(*body);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"body\"];",
                    stmt_node_graph_id, body_stmt_graph_id
                )
                .unwrap();
                return stmt_node_graph_id;
            }
            Stmt::If {
                cond,
                then,
                otherwise,
            } => {
                writeln!(
                    self.output,
                    "  {} [label=\"IfStmt\", fillcolor=skyblue];",
                    stmt_node_graph_id
                )
                .unwrap();
                let cond_expr_graph_id = self.visit_expr(*cond);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"condition\"];",
                    stmt_node_graph_id, cond_expr_graph_id
                )
                .unwrap();
                let then_stmt_graph_id = self.visit_stmt(*then);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"then_branch\"];",
                    stmt_node_graph_id, then_stmt_graph_id
                )
                .unwrap();
                if let Some(else_stmt_ref) = otherwise {
                    let else_stmt_graph_id = self.visit_stmt(*else_stmt_ref);
                    writeln!(
                        self.output,
                        "  {} -> {} [label=\"else_branch\"];",
                        stmt_node_graph_id, else_stmt_graph_id
                    )
                    .unwrap();
                }
                return stmt_node_graph_id;
            }
            Stmt::While { cond, body } => {
                writeln!(
                    self.output,
                    "  {} [label=\"WhileStmt\", fillcolor=paleturquoise];",
                    stmt_node_graph_id
                )
                .unwrap();
                let cond_expr_graph_id = self.visit_expr(*cond);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"condition\"];",
                    stmt_node_graph_id, cond_expr_graph_id
                )
                .unwrap();
                let body_stmt_graph_id = self.visit_stmt(*body);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"body\"];",
                    stmt_node_graph_id, body_stmt_graph_id
                )
                .unwrap();
                return stmt_node_graph_id;
            }
            Stmt::DoWhile { body, cond } => {
                writeln!(
                    self.output,
                    "  {} [label=\"DoWhileStmt\", fillcolor=paleturquoise1];",
                    stmt_node_graph_id
                )
                .unwrap();
                let body_stmt_graph_id = self.visit_stmt(*body);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"body\"];",
                    stmt_node_graph_id, body_stmt_graph_id
                )
                .unwrap();
                let cond_expr_graph_id = self.visit_expr(*cond);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"condition\"];",
                    stmt_node_graph_id, cond_expr_graph_id
                )
                .unwrap();
                return stmt_node_graph_id;
            }
            Stmt::For {
                init,
                cond,
                step,
                body,
            } => {
                writeln!(
                    self.output,
                    "  {} [label=\"ForStmt\", fillcolor=thistle];",
                    stmt_node_graph_id
                )
                .unwrap();
                if let Some(for_init) = init {
                    match for_init {
                        ForInit::Expr(init_expr_ref) => {
                            let init_expr_graph_id = self.visit_expr(*init_expr_ref);
                            writeln!(
                                self.output,
                                "  {} -> {} [label=\"initializer (expr)\"];",
                                stmt_node_graph_id, init_expr_graph_id
                            )
                            .unwrap();
                        }
                        ForInit::VarDecl(init_decl_ref) => {
                            let init_decl_graph_id = self.visit_decl(*init_decl_ref);
                            writeln!(
                                self.output,
                                "  {} -> {} [label=\"initializer (decl)\"];",
                                stmt_node_graph_id, init_decl_graph_id
                            )
                            .unwrap();
                        }
                    }
                }
                if let Some(cond_expr_ref) = cond {
                    let cond_expr_graph_id = self.visit_expr(*cond_expr_ref);
                    writeln!(
                        self.output,
                        "  {} -> {} [label=\"condition\"];",
                        stmt_node_graph_id, cond_expr_graph_id
                    )
                    .unwrap();
                }
                if let Some(step_expr_ref) = step {
                    let step_expr_graph_id = self.visit_expr(*step_expr_ref);
                    writeln!(
                        self.output,
                        "  {} -> {} [label=\"step\"];",
                        stmt_node_graph_id, step_expr_graph_id
                    )
                    .unwrap();
                }
                let body_stmt_graph_id = self.visit_stmt(*body);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"body\"];",
                    stmt_node_graph_id, body_stmt_graph_id
                )
                .unwrap();
                return stmt_node_graph_id;
            }
        };

        writeln!(
            self.output,
            "  {} [label=\"{}\", fillcolor={}];",
            stmt_node_graph_id, node_label_and_color.0, node_label_and_color.1
        )
        .unwrap();
        stmt_node_graph_id
    }

    fn visit_expr(&mut self, expr_ref: ExprRef) -> String {
        let expr_node_graph_id = self.expr_node_id(expr_ref);
        let expr = self.ast.expr(expr_ref);

        let (label, fillcolor) = match expr {
            Expr::Const(val) => (format!("Const\\nvalue: {}", val), "gold".to_string()),
            Expr::Var(name_sym) => {
                self.get_symbol_name(*name_sym);
                (
                    format!("VarRef\\nname: {}", self.buffer.escape_default()),
                    "olivedrab1".to_string(),
                )
            }
            Expr::Grouped(inner_expr_ref) => {
                writeln!(
                    self.output,
                    "  {} [label=\"GroupedExpr\", fillcolor=lightgrey];",
                    expr_node_graph_id
                )
                .unwrap();
                let inner_expr_graph_id = self.visit_expr(*inner_expr_ref);
                writeln!(
                    self.output,
                    "  {} -> {};",
                    expr_node_graph_id, inner_expr_graph_id
                )
                .unwrap();
                return expr_node_graph_id;
            }
            Expr::Unary {
                op,
                expr: operand_expr_ref,
            } => {
                writeln!(
                    self.output,
                    "  {} [label=\"UnaryOp\\nop: {:?}\", fillcolor=coral];",
                    expr_node_graph_id, op
                )
                .unwrap();
                let operand_graph_id = self.visit_expr(*operand_expr_ref);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"operand\"];",
                    expr_node_graph_id, operand_graph_id
                )
                .unwrap();
                return expr_node_graph_id;
            }
            Expr::Binary { op, lhs, rhs } => {
                writeln!(
                    self.output,
                    "  {} [label=\"BinaryOp\\nop: {:?}\", fillcolor=orchid];",
                    expr_node_graph_id, op
                )
                .unwrap();
                let lhs_graph_id = self.visit_expr(*lhs);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"lhs\"];",
                    expr_node_graph_id, lhs_graph_id
                )
                .unwrap();
                let rhs_graph_id = self.visit_expr(*rhs);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"rhs\"];",
                    expr_node_graph_id, rhs_graph_id
                )
                .unwrap();
                return expr_node_graph_id;
            }
            Expr::Ternary {
                cond,
                then,
                otherwise,
            } => {
                writeln!(
                    self.output,
                    "  {} [label=\"TernaryOp\", fillcolor=mediumspringgreen];",
                    expr_node_graph_id
                )
                .unwrap();
                let cond_graph_id = self.visit_expr(*cond);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"condition\"];",
                    expr_node_graph_id, cond_graph_id
                )
                .unwrap();
                let then_graph_id = self.visit_expr(*then);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"then_expr\"];",
                    expr_node_graph_id, then_graph_id
                )
                .unwrap();
                let otherwise_graph_id = self.visit_expr(*otherwise);
                writeln!(
                    self.output,
                    "  {} -> {} [label=\"else_expr\"];",
                    expr_node_graph_id, otherwise_graph_id
                )
                .unwrap();
                return expr_node_graph_id;
            }
            Expr::Call { name, args } => {
                self.get_symbol_name(*name);
                writeln!(
                    self.output,
                    "  {} [label=\"FunctionCall\\nname: {}\", fillcolor=deepskyblue];",
                    expr_node_graph_id,
                    self.buffer.escape_default()
                )
                .unwrap();

                let args_slice = self.ast.args(*args);
                if !args_slice.is_empty() {
                    let args_list_node_id = self.fresh_aux_node_id("args");
                    writeln!(
                        self.output,
                        "  {} [label=\"Arguments\", fillcolor=aliceblue];",
                        args_list_node_id
                    )
                    .unwrap();
                    writeln!(
                        self.output,
                        "  {} -> {} [label=\"args\"];",
                        expr_node_graph_id, args_list_node_id
                    )
                    .unwrap();
                    for (i, arg_expr_ref) in args_slice.iter().enumerate() {
                        let arg_expr_graph_id = self.visit_expr(*arg_expr_ref);
                        writeln!(
                            self.output,
                            "  {} -> {} [label=\"arg {}\"];",
                            args_list_node_id, arg_expr_graph_id, i
                        )
                        .unwrap();
                    }
                } else {
                    // Optional: indicate no arguments if desired
                    let no_args_node = format!("{}_no_args_marker", expr_node_graph_id);
                    writeln!(
                        self.output,
                        "  {} [label=\"(no arguments)\", shape=plaintext, fillcolor=none];",
                        no_args_node
                    )
                    .unwrap();
                    writeln!(
                        self.output,
                        "  {} -> {} [style=dotted];",
                        expr_node_graph_id, no_args_node
                    )
                    .unwrap();
                }
                return expr_node_graph_id;
            }
        };

        writeln!(
            self.output,
            "  {} [label=\"{}\", fillcolor={}];",
            expr_node_graph_id, label, fillcolor
        )
        .unwrap();
        expr_node_graph_id
    }

    fn visit_block_item(&mut self, item: BlockItem) -> String {
        match item {
            BlockItem::Decl(decl_ref) => self.visit_decl(decl_ref),
            BlockItem::Stmt(stmt_ref) => self.visit_stmt(stmt_ref),
        }
    }
}

// To use this:
// 1. Make sure this file is part of your crate (e.g., `mod ast_graphviz;` in lib.rs or main.rs)
// 2. After parsing, get your `Ast` object and `Interner`.
//
// Example:
//
//   use crate::ast_graphviz::AstGraphviz;
//   // ... assume `parsed_ast` is your `Ast` instance
//   // ... assume `interner` is your `Interner` instance
//
//   let graphviz_generator = AstGraphviz::new(&parsed_ast, &interner);
//   let dot_representation = graphviz_generator.to_dot_string();
//
//   match std::fs::write("ast_output.dot", dot_representation) {
//       Ok(_) => println!("AST DOT file generated as ast_output.dot"),
//       Err(e) => eprintln!("Error writing AST DOT file: {}", e),
//   }
//
//   // Then, use a tool like Graphviz to render the .dot file:
//   // `dot -Tpng ast_output.dot -o ast_output.png`
//   // `dot -Tsvg ast_output.dot -o ast_output.svg`
