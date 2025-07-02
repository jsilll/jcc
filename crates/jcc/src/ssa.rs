use crate::{
    ast,
    sema::{self, SemaSymbol},
};

use jcc_ssa::{
    self as ssa,
    interner::{Interner, Symbol},
    sourcemap::SourceSpan,
    Block, Func, StaticVar,
};

use std::collections::HashMap;

// ---------------------------------------------------------------------------
// Building SSA from AST
// ---------------------------------------------------------------------------

pub fn build(ast: &ast::Ast, sema: &sema::SemaCtx, interner: Interner) -> ssa::Program {
    let mut prog = ssa::Program::new(interner);

    // Bootstrap the builder with the first function declaration
    let mut builder = None;
    for decl_ref in ast.root() {
        let decl = ast.decl(*decl_ref);
        match decl.kind {
            ast::DeclKind::Var(_) => continue,
            ast::DeclKind::Func { body: None, .. } => continue,
            ast::DeclKind::Func { body: Some(_), .. } => {
                builder = Some(SSAFuncBuilder::new(
                    ast, sema, &mut prog, *decl_ref, &decl.name,
                ));
                break;
            }
        }
    }

    if let Some(mut builder) = builder {
        for decl_ref in ast.root() {
            let decl = ast.decl(*decl_ref);
            match decl.kind {
                ast::DeclKind::Var(_) => {}
                ast::DeclKind::Func { params, body, .. } => {
                    builder.build_func(*decl_ref, &decl.name, params, body);
                }
            }
        }
    }

    for decl_ref in ast.root() {
        let decl = ast.decl(*decl_ref);
        match decl.kind {
            ast::DeclKind::Func { .. } => {}
            ast::DeclKind::Var(_) => {
                let info = sema
                    .symbol(decl.name.sema())
                    .expect("expected a sema symbol");
                match info.attr {
                    sema::Attribute::Local | sema::Attribute::Function { .. } => {}
                    sema::Attribute::Static { init, is_global } => match init {
                        sema::StaticValue::Tentative | sema::StaticValue::NoInitializer => {}
                        sema::StaticValue::Initialized(init) => {
                            prog.static_vars.insert(
                                decl.name.raw,
                                StaticVar {
                                    is_global,
                                    span: decl.span,
                                    init: Some(init),
                                    ty: ssa::Type::Int32,
                                },
                            );
                        }
                    },
                }
            }
        }
    }

    prog
}

// ---------------------------------------------------------------------------
// SSAFuncBuilder
// ---------------------------------------------------------------------------

struct SSAFuncBuilder<'a> {
    ast: &'a ast::Ast,
    sema: &'a sema::SemaCtx,
    prog: &'a mut ssa::Program,
    decl: ast::DeclRef,
    func: ssa::FuncRef,
    block: ssa::BlockRef,
    funcs: HashMap<Symbol, ssa::FuncRef>,
    vars: HashMap<SemaSymbol, ssa::InstRef>,
    labeled_blocks: HashMap<Symbol, ssa::BlockRef>,
    case_blocks: HashMap<ast::StmtRef, ssa::BlockRef>,
    break_blocks: HashMap<ast::StmtRef, ssa::BlockRef>,
    continue_blocks: HashMap<ast::StmtRef, ssa::BlockRef>,
}

impl<'a> SSAFuncBuilder<'a> {
    fn new(
        ast: &'a ast::Ast,
        sema: &'a sema::SemaCtx,
        prog: &'a mut ssa::Program,
        default_decl: ast::DeclRef,
        default_name: &ast::AstSymbol,
    ) -> Self {
        let span = ast.decl(default_decl).span;
        let is_global = sema
            .symbol(default_name.sema())
            .expect("expected a sema symbol")
            .is_global();
        let func = prog.new_func(Func {
            span,
            is_global,
            name: default_name.raw,
            ..Default::default()
        });
        let block_name = prog.interner.intern("entry");
        let block = prog.new_block(Block {
            span,
            name: block_name,
            ..Default::default()
        });
        prog.func_mut(func).blocks.push(block);
        let funcs = HashMap::from([(default_name.raw, func)]);
        Self {
            ast,
            sema,
            prog,
            func,
            funcs,
            block,
            decl: default_decl,
            vars: HashMap::new(),
            case_blocks: HashMap::new(),
            break_blocks: HashMap::new(),
            labeled_blocks: HashMap::new(),
            continue_blocks: HashMap::new(),
        }
    }

    #[inline]
    fn clear(&mut self) {
        self.vars.clear();
        self.case_blocks.clear();
        self.break_blocks.clear();
        self.labeled_blocks.clear();
        self.continue_blocks.clear();
    }

    fn build_func(
        &mut self,
        decl: ast::DeclRef,
        name: &ast::AstSymbol,
        params: ast::Slice<ast::DeclRef>,
        body: Option<ast::Slice<ast::BlockItem>>,
    ) {
        // If the function declaration has changed, we need to update the function reference
        if self.decl != decl {
            self.decl = decl;
            let span = self.ast.decl(decl).span;
            let is_global = self
                .sema
                .symbol(name.sema())
                .expect("expected a sema symbol")
                .is_global();
            self.func = *self
                .funcs
                .entry(name.raw)
                .or_insert(self.prog.new_func(Func {
                    span,
                    is_global,
                    name: name.raw,
                    ..Default::default()
                }));
            if body.is_some() {
                // Also create a new entry block for the function
                let block_name = self.prog.interner.intern("entry");
                self.block = self.prog.new_block(Block {
                    span,
                    name: block_name,
                    ..Default::default()
                });
                self.prog.func_mut(self.func).blocks.push(self.block);
            }
        }

        if let Some(body) = body {
            self.clear();
            self.ast.params(params).iter().for_each(|param| {
                let span = self.ast.decl(*param).span;
                let arg = self.prog.new_inst(ssa::Inst::arg(ssa::Type::Int32, span));
                self.vars.insert(
                    self.ast
                        .decl(*param)
                        .name
                        .sema
                        .get()
                        .expect("expected a sema symbol"),
                    arg,
                );
                self.append_to_block(arg);
            });

            self.ast
                .block_items(body)
                .iter()
                .for_each(|item| match item {
                    ast::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                    ast::BlockItem::Decl(decl) => self.visit_decl(*decl),
                });
            let append_return = match self.ast.block_items(body).last() {
                Some(ast::BlockItem::Stmt(stmt)) => {
                    !matches!(self.ast.stmt(*stmt).kind, ast::StmtKind::Return(_))
                }
                _ => true,
            };
            if append_return {
                let span = self.ast.decl(self.decl).span;
                let val = self.prog.new_inst(ssa::Inst::const_i32(0, span));
                let ret = self.prog.new_inst(ssa::Inst::ret(val, span));
                self.append_slice_to_block(&[val, ret]);
            }
        }
    }

    #[inline]
    fn append_to_block(&mut self, instr: ssa::InstRef) {
        self.prog.block_mut(self.block).insts.push(instr);
    }

    #[inline]
    fn get_var_ptr(&self, sym: SemaSymbol) -> ssa::InstRef {
        self.vars
            .get(&sym)
            .copied()
            .unwrap_or_else(|| panic!("expected a variable pointer for {sym:?}"))
    }

    #[inline]
    fn append_slice_to_block(&mut self, instrs: &[ssa::InstRef]) {
        self.prog
            .block_mut(self.block)
            .insts
            .extend_from_slice(instrs);
    }

    #[inline]
    fn get_or_make_labeled_block(&mut self, label: Symbol, span: SourceSpan) -> ssa::BlockRef {
        *self.labeled_blocks.entry(label).or_insert_with(|| {
            let block = self.prog.new_block(Block {
                span,
                name: label,
                ..Default::default()
            });
            self.prog.func_mut(self.func).blocks.push(block);
            block
        })
    }

    fn visit_decl(&mut self, decl: ast::DeclRef) {
        if let ast::DeclKind::Var(init) = self.ast.decl(decl).kind {
            let span = self.ast.decl(decl).span;
            let alloca = self.prog.new_inst(ssa::Inst::alloca(span));
            self.append_to_block(alloca);
            self.vars.insert(
                self.ast
                    .decl(decl)
                    .name
                    .sema
                    .get()
                    .expect("expected a sema symbol"),
                alloca,
            );
            if let Some(init) = init {
                let val = self.visit_expr(init, ExprMode::RightValue);
                let store = self.prog.new_inst(ssa::Inst::store(alloca, val, span));
                self.append_to_block(store);
            }
        }
    }

    fn visit_stmt(&mut self, stmt_ref: ast::StmtRef) {
        let stmt = self.ast.stmt(stmt_ref);
        match &stmt.kind {
            ast::StmtKind::Empty => {}
            ast::StmtKind::Break => {
                let block = self
                    .break_blocks
                    .get(
                        self.sema
                            .breaks
                            .get(&stmt_ref)
                            .expect("expected a break block"),
                    )
                    .expect("expected a break block");
                let jump = self.prog.new_inst(ssa::Inst::jump(*block, stmt.span));
                self.append_to_block(jump);
            }
            ast::StmtKind::Expr(expr) => {
                self.visit_expr(*expr, ExprMode::RightValue);
            }
            ast::StmtKind::Return(expr) => {
                let val = self.visit_expr(*expr, ExprMode::RightValue);
                let inst = self.prog.new_inst(ssa::Inst::ret(val, stmt.span));
                self.append_to_block(inst);
            }
            ast::StmtKind::Goto(label) => {
                let block = self.get_or_make_labeled_block(*label, stmt.span);
                let inst = self.prog.new_inst(ssa::Inst::jump(block, stmt.span));
                self.append_to_block(inst);
            }
            ast::StmtKind::Label { label, stmt: inner } => {
                let block = self.get_or_make_labeled_block(*label, stmt.span);
                let inst = self.prog.new_inst(ssa::Inst::jump(block, stmt.span));
                self.append_to_block(inst);

                // === Labeled Block ===
                self.block = block;
                self.visit_stmt(*inner);
            }
            ast::StmtKind::Compound(items) => {
                self.ast
                    .block_items(*items)
                    .iter()
                    .for_each(|item| match item {
                        ast::BlockItem::Decl(decl) => self.visit_decl(*decl),
                        ast::BlockItem::Stmt(stmt) => self.visit_stmt(*stmt),
                    });
            }
            ast::StmtKind::Default(inner) => {
                let block = self
                    .case_blocks
                    .remove(&stmt_ref)
                    .expect("expected a case block");

                // === Default Block ===
                self.block = block;
                self.visit_stmt(*inner);
            }
            ast::StmtKind::Case { stmt: inner, .. } => {
                let block = self
                    .case_blocks
                    .remove(&stmt_ref)
                    .expect("expected a case block");

                // === Previous Block ===
                let jump = self.prog.new_inst(ssa::Inst::jump(block, stmt.span));
                self.append_to_block(jump);

                // === Case Block ===
                self.block = block;
                self.visit_stmt(*inner);
            }
            ast::StmtKind::Continue => {
                let block = self
                    .continue_blocks
                    .get(
                        self.sema
                            .continues
                            .get(&stmt_ref)
                            .expect("expected a continue block"),
                    )
                    .expect("expected a continue block");
                let jump = self.prog.new_inst(ssa::Inst::jump(*block, stmt.span));
                self.append_to_block(jump);
            }
            ast::StmtKind::If {
                cond,
                then,
                otherwise: None,
            } => {
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let then_name = self.prog.interner.intern("if.then");
                let if_name = self.prog.interner.intern("if.cont");
                let then_block = self.prog.new_block(Block {
                    name: then_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let cont_block = self.prog.new_block(Block {
                    name: if_name,
                    span: stmt.span,
                    ..Default::default()
                });
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[then_block, cont_block]);

                let branch = self.prog.new_inst(ssa::Inst::branch(
                    cond_val, then_block, cont_block, stmt.span,
                ));
                self.append_to_block(branch);

                // === Then Block ===
                self.block = then_block;
                self.visit_stmt(*then);
                let jmp_then = self.prog.new_inst(ssa::Inst::jump(cont_block, stmt.span));
                self.append_to_block(jmp_then);

                // === Merge Block ===
                self.block = cont_block;
            }
            ast::StmtKind::If {
                cond,
                then,
                otherwise: Some(otherwise),
            } => {
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let then_name = self.prog.interner.intern("if.then");
                let else_name = self.prog.interner.intern("if.else");
                let if_name = self.prog.interner.intern("if.cont");
                let then_block = self.prog.new_block(Block {
                    name: then_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let else_block = self.prog.new_block(Block {
                    name: else_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let cont_block = self.prog.new_block(Block {
                    name: if_name,
                    span: stmt.span,
                    ..Default::default()
                });
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[then_block, else_block, cont_block]);

                let branch = self.prog.new_inst(ssa::Inst::branch(
                    cond_val, then_block, else_block, stmt.span,
                ));
                self.append_to_block(branch);

                // === Then Block ===
                self.block = then_block;
                self.visit_stmt(*then);
                let jmp_then = self.prog.new_inst(ssa::Inst::jump(cont_block, stmt.span));
                self.append_to_block(jmp_then);

                // === Else Block ===
                self.block = else_block;
                self.visit_stmt(*otherwise);
                let jmp_else = self.prog.new_inst(ssa::Inst::jump(cont_block, stmt.span));
                self.append_to_block(jmp_else);

                // === Merge Block ===
                self.block = cont_block;
            }
            ast::StmtKind::While { cond, body } => {
                let cond_name = self.prog.interner.intern("while.cond");
                let body_name = self.prog.interner.intern("while.cond");
                let cont_name = self.prog.interner.intern("while.cond");
                let cond_block = self.prog.new_block(Block {
                    name: cond_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let body_block = self.prog.new_block(Block {
                    name: body_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let cont_block = self.prog.new_block(Block {
                    name: cont_name,
                    span: stmt.span,
                    ..Default::default()
                });
                self.break_blocks.insert(stmt_ref, cont_block);
                self.continue_blocks.insert(stmt_ref, cond_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[cond_block, body_block, cont_block]);

                let jump = self.prog.new_inst(ssa::Inst::jump(cond_block, stmt.span));
                self.append_to_block(jump);

                // === Cond Block ===
                self.block = cond_block;
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let branch = self.prog.new_inst(ssa::Inst::branch(
                    cond_val, body_block, cont_block, stmt.span,
                ));
                self.append_to_block(branch);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self.prog.new_inst(ssa::Inst::jump(cond_block, stmt.span));
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt_ref);
                self.continue_blocks.remove(&stmt_ref);
            }
            ast::StmtKind::DoWhile { body, cond } => {
                let body_name = self.prog.interner.intern("do.body");
                let cond_name = self.prog.interner.intern("do.cond");
                let cont_name = self.prog.interner.intern("do.cont");
                let body_block = self.prog.new_block(Block {
                    name: body_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let cond_block = self.prog.new_block(Block {
                    name: cond_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let cont_block = self.prog.new_block(Block {
                    name: cont_name,
                    span: stmt.span,
                    ..Default::default()
                });
                self.break_blocks.insert(stmt_ref, cont_block);
                self.continue_blocks.insert(stmt_ref, cond_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[body_block, cond_block, cont_block]);

                // === Jump to Body Block ===
                let jump = self.prog.new_inst(ssa::Inst::jump(body_block, stmt.span));
                self.append_to_block(jump);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self.prog.new_inst(ssa::Inst::jump(cond_block, stmt.span));
                self.append_to_block(jump);

                // === Cond Block ===
                self.block = cond_block;
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let branch = self.prog.new_inst(ssa::Inst::branch(
                    cond_val, body_block, cont_block, stmt.span,
                ));
                self.append_to_block(branch);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt_ref);
                self.continue_blocks.remove(&stmt_ref);
            }
            ast::StmtKind::For {
                init,
                cond: None,
                step: None,
                body,
            } => {
                let body_name = self.prog.interner.intern("for.body");
                let cont_name = self.prog.interner.intern("for.cont");
                let body_block = self.prog.new_block(Block {
                    name: body_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let cont_block = self.prog.new_block(Block {
                    name: cont_name,
                    span: stmt.span,
                    ..Default::default()
                });
                self.break_blocks.insert(stmt_ref, cont_block);
                self.continue_blocks.insert(stmt_ref, body_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[body_block, cont_block]);

                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                let jump = self.prog.new_inst(ssa::Inst::jump(body_block, stmt.span));
                self.append_to_block(jump);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self.prog.new_inst(ssa::Inst::jump(body_block, stmt.span));
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt_ref);
                self.continue_blocks.remove(&stmt_ref);
            }
            ast::StmtKind::For {
                init,
                cond: Some(cond),
                step: None,
                body,
            } => {
                let cond_name = self.prog.interner.intern("for.cond");
                let body_name = self.prog.interner.intern("for.body");
                let cont_name = self.prog.interner.intern("for.cont");
                let cond_block = self.prog.new_block(Block {
                    name: cond_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let body_block = self.prog.new_block(Block {
                    name: body_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let cont_block = self.prog.new_block(Block {
                    name: cont_name,
                    span: stmt.span,
                    ..Default::default()
                });
                self.break_blocks.insert(stmt_ref, cont_block);
                self.continue_blocks.insert(stmt_ref, cond_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[cond_block, body_block, cont_block]);

                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                let jump = self.prog.new_inst(ssa::Inst::jump(cond_block, stmt.span));
                self.append_to_block(jump);

                // === Cond Block ===
                self.block = cond_block;
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let branch = self.prog.new_inst(ssa::Inst::branch(
                    cond_val, body_block, cont_block, stmt.span,
                ));
                self.append_to_block(branch);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self.prog.new_inst(ssa::Inst::jump(cond_block, stmt.span));
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt_ref);
                self.continue_blocks.remove(&stmt_ref);
            }
            ast::StmtKind::For {
                init,
                cond: None,
                step: Some(step),
                body,
            } => {
                let step_name = self.prog.interner.intern("for.step");
                let body_name = self.prog.interner.intern("for.body");
                let cont_name = self.prog.interner.intern("for.cont");
                let step_block = self.prog.new_block(Block {
                    name: step_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let body_block = self.prog.new_block(Block {
                    name: body_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let cont_block = self.prog.new_block(Block {
                    name: cont_name,
                    span: stmt.span,
                    ..Default::default()
                });
                self.break_blocks.insert(stmt_ref, cont_block);
                self.continue_blocks.insert(stmt_ref, step_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[step_block, body_block, cont_block]);

                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                let jump = self.prog.new_inst(ssa::Inst::jump(body_block, stmt.span));
                self.append_to_block(jump);

                // === Step Block ===
                self.block = step_block;
                self.visit_expr(*step, ExprMode::RightValue);
                let jump = self.prog.new_inst(ssa::Inst::jump(body_block, stmt.span));
                self.append_to_block(jump);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self.prog.new_inst(ssa::Inst::jump(step_block, stmt.span));
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt_ref);
                self.continue_blocks.remove(&stmt_ref);
            }
            ast::StmtKind::For {
                init,
                cond: Some(cond),
                step: Some(step),
                body,
            } => {
                let cond_name = self.prog.interner.intern("for.cond");
                let step_name = self.prog.interner.intern("for.step");
                let body_name = self.prog.interner.intern("for.body");
                let cont_name = self.prog.interner.intern("for.cont");
                let cond_block = self.prog.new_block(Block {
                    name: cond_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let step_block = self.prog.new_block(Block {
                    name: step_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let body_block = self.prog.new_block(Block {
                    name: body_name,
                    span: stmt.span,
                    ..Default::default()
                });
                let cont_block = self.prog.new_block(Block {
                    name: cont_name,
                    span: stmt.span,
                    ..Default::default()
                });
                self.break_blocks.insert(stmt_ref, cont_block);
                self.continue_blocks.insert(stmt_ref, step_block);
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[cond_block, step_block, body_block, cont_block]);

                if let Some(init) = init {
                    match init {
                        ast::ForInit::VarDecl(decl) => {
                            self.visit_decl(*decl);
                        }
                        ast::ForInit::Expr(expr) => {
                            self.visit_expr(*expr, ExprMode::RightValue);
                        }
                    }
                }
                let jump = self.prog.new_inst(ssa::Inst::jump(cond_block, stmt.span));
                self.append_to_block(jump);

                // === Cond Block ===
                self.block = cond_block;
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let branch = self.prog.new_inst(ssa::Inst::branch(
                    cond_val, body_block, cont_block, stmt.span,
                ));
                self.append_to_block(branch);

                // === Step Block ===
                self.block = step_block;
                self.visit_expr(*step, ExprMode::RightValue);
                let jump = self.prog.new_inst(ssa::Inst::jump(cond_block, stmt.span));
                self.append_to_block(jump);

                // === Body Block ===
                self.block = body_block;
                self.visit_stmt(*body);
                let jump = self.prog.new_inst(ssa::Inst::jump(step_block, stmt.span));
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
                self.break_blocks.remove(&stmt_ref);
                self.continue_blocks.remove(&stmt_ref);
            }
            ast::StmtKind::Switch { cond, body } => {
                let mut cases = Vec::new();
                let mut default_block = None;
                if let Some(switch) = self.sema.switches.get(&stmt_ref) {
                    cases.reserve(switch.cases.len());
                    switch.cases.iter().for_each(|inner| {
                        if let ast::StmtKind::Case { expr, .. } = &self.ast.stmt(*inner).kind {
                            let val = match self.ast.expr(*expr).kind {
                                ast::ExprKind::Const(c) => c,
                                _ => panic!("expected a constant expression"),
                            };
                            let case_name = self.prog.interner.intern("switch.case");
                            let case_block = self.prog.new_block(Block {
                                name: case_name,
                                span: stmt.span,
                                ..Default::default()
                            });
                            self.prog.func_mut(self.func).blocks.push(case_block);
                            self.case_blocks.insert(*inner, case_block);
                            cases.push((val, case_block));
                        }
                    });
                    if let Some(inner) = switch.default {
                        let default_name = self.prog.interner.intern("switch.default");
                        let block = self.prog.new_block(Block {
                            name: default_name,
                            span: stmt.span,
                            ..Default::default()
                        });
                        self.prog.func_mut(self.func).blocks.push(block);
                        self.case_blocks.insert(inner, block);
                        default_block = Some(block);
                    }
                }

                let cont_name = self.prog.interner.intern("switch.cont");
                let cont_block = self.prog.new_block(Block {
                    name: cont_name,
                    span: stmt.span,
                    ..Default::default()
                });
                self.prog.func_mut(self.func).blocks.push(cont_block);
                self.break_blocks.insert(stmt_ref, cont_block);

                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);
                let switch = self.prog.new_inst(ssa::Inst::switch(
                    cond_val,
                    default_block.unwrap_or(cont_block),
                    cases,
                    stmt.span,
                ));
                self.append_to_block(switch);

                self.visit_stmt(*body);
                let jump = self.prog.new_inst(ssa::Inst::jump(cont_block, stmt.span));
                self.append_to_block(jump);

                // === Merge Block ===
                self.block = cont_block;
            }
        }
    }

    fn visit_expr(&mut self, expr: ast::ExprRef, mode: ExprMode) -> ssa::InstRef {
        let expr = self.ast.expr(expr);
        match &expr.kind {
            ast::ExprKind::Grouped(expr) => self.visit_expr(*expr, mode),
            ast::ExprKind::Const(c) => {
                let inst = self.prog.new_inst(ssa::Inst::const_i32(*c, expr.span));
                self.append_to_block(inst);
                inst
            }
            ast::ExprKind::Call { name, args, .. } => {
                let args = self
                    .ast
                    .args(*args)
                    .iter()
                    .map(|arg| self.visit_expr(*arg, ExprMode::RightValue))
                    .collect::<Vec<_>>();
                let is_global = self
                    .sema
                    .symbol(name.sema())
                    .expect("expected a sema symbol")
                    .is_global();
                let func = *self
                    .funcs
                    .entry(name.raw)
                    .or_insert(self.prog.new_func(Func {
                        is_global,
                        name: name.raw,
                        span: expr.span,
                        ..Default::default()
                    }));
                let inst = self.prog.new_inst(ssa::Inst::call(func, args, expr.span));
                self.append_to_block(inst);
                inst
            }
            ast::ExprKind::Var(name) => {
                let info = self
                    .sema
                    .symbol(name.sema())
                    .expect("expected a sema symbol");
                match info.attr {
                    sema::Attribute::Local => {
                        let ptr = self.get_var_ptr(name.sema());
                        match mode {
                            ExprMode::LeftValue => ptr,
                            ExprMode::RightValue => {
                                let inst = self.prog.new_inst(ssa::Inst::load(ptr, expr.span));
                                self.append_to_block(inst);
                                inst
                            }
                        }
                    }
                    sema::Attribute::Static { .. } => todo!("handle static variables"),
                    sema::Attribute::Function { .. } => todo!("handle function variables"),
                }
            }
            ast::ExprKind::Unary { op, expr: inner } => match op {
                ast::UnaryOp::Neg => self.build_unary(ssa::UnaryOp::Neg, *inner, expr.span),
                ast::UnaryOp::BitNot => self.build_unary(ssa::UnaryOp::Not, *inner, expr.span),
                ast::UnaryOp::PreInc => {
                    self.build_prefix_unary(ssa::UnaryOp::Inc, *inner, expr.span)
                }
                ast::UnaryOp::PreDec => {
                    self.build_prefix_unary(ssa::UnaryOp::Dec, *inner, expr.span)
                }
                ast::UnaryOp::PostInc => {
                    self.build_postfix_unary(ssa::UnaryOp::Inc, *inner, expr.span)
                }
                ast::UnaryOp::PostDec => {
                    self.build_postfix_unary(ssa::UnaryOp::Dec, *inner, expr.span)
                }
                ast::UnaryOp::LogicalNot => {
                    let val = self.visit_expr(*inner, ExprMode::RightValue);

                    let zero = self.prog.new_inst(ssa::Inst::const_i32(0, expr.span));
                    let cmp = self.prog.new_inst(ssa::Inst::binary(
                        ssa::Type::Int32,
                        ssa::BinaryOp::Equal,
                        val,
                        zero,
                        expr.span,
                    ));

                    self.append_slice_to_block(&[zero, cmp]);
                    cmp
                }
            },
            ast::ExprKind::Binary { op, lhs, rhs } => match op {
                ast::BinaryOp::LogicalOr => {
                    self.build_short_circuit(LogicalOp::Or, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::LogicalAnd => {
                    self.build_short_circuit(LogicalOp::And, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::Equal => {
                    self.build_binary(ssa::BinaryOp::Equal, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::NotEqual => {
                    self.build_binary(ssa::BinaryOp::NotEqual, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::LessThan => {
                    self.build_binary(ssa::BinaryOp::LessThan, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::LessEqual => {
                    self.build_binary(ssa::BinaryOp::LessEqual, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::GreaterThan => {
                    self.build_binary(ssa::BinaryOp::GreaterThan, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::GreaterEqual => {
                    self.build_binary(ssa::BinaryOp::GreaterEqual, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::Add => self.build_binary(ssa::BinaryOp::Add, *lhs, *rhs, expr.span),
                ast::BinaryOp::Sub => self.build_binary(ssa::BinaryOp::Sub, *lhs, *rhs, expr.span),
                ast::BinaryOp::Mul => self.build_binary(ssa::BinaryOp::Mul, *lhs, *rhs, expr.span),
                ast::BinaryOp::Div => self.build_binary(ssa::BinaryOp::Div, *lhs, *rhs, expr.span),
                ast::BinaryOp::Rem => self.build_binary(ssa::BinaryOp::Rem, *lhs, *rhs, expr.span),
                ast::BinaryOp::BitOr => self.build_binary(ssa::BinaryOp::Or, *lhs, *rhs, expr.span),
                ast::BinaryOp::BitAnd => {
                    self.build_binary(ssa::BinaryOp::And, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitXor => {
                    self.build_binary(ssa::BinaryOp::Xor, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitShl => {
                    self.build_binary(ssa::BinaryOp::Shl, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitShr => {
                    self.build_binary(ssa::BinaryOp::Shr, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::AddAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Add, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::SubAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Sub, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::MulAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Mul, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::DivAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Div, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::RemAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Rem, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitOrAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Or, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitAndAssign => {
                    self.build_binary_assign(ssa::BinaryOp::And, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitXorAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Xor, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitShlAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Shl, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::BitShrAssign => {
                    self.build_binary_assign(ssa::BinaryOp::Shr, *lhs, *rhs, expr.span)
                }
                ast::BinaryOp::Assign => {
                    let lhs = self.visit_expr(*lhs, ExprMode::LeftValue);
                    let rhs = self.visit_expr(*rhs, ExprMode::RightValue);
                    let store = self.prog.new_inst(ssa::Inst::store(lhs, rhs, expr.span));
                    self.append_to_block(store);
                    match mode {
                        ExprMode::LeftValue => lhs,
                        ExprMode::RightValue => rhs,
                    }
                }
            },
            ast::ExprKind::Ternary {
                cond,
                then,
                otherwise,
            } => {
                let cond_val = self.visit_expr(*cond, ExprMode::RightValue);

                let then_name = self.prog.interner.intern("tern.then");
                let else_name = self.prog.interner.intern("tern.else");
                let cont_name = self.prog.interner.intern("tern.cont");
                let then_block = self.prog.new_block(Block {
                    name: then_name,
                    span: expr.span,
                    ..Default::default()
                });
                let else_block = self.prog.new_block(Block {
                    name: else_name,
                    span: expr.span,
                    ..Default::default()
                });
                let cont_block = self.prog.new_block(Block {
                    name: cont_name,
                    span: expr.span,
                    ..Default::default()
                });
                self.prog
                    .func_mut(self.func)
                    .blocks
                    .extend_from_slice(&[then_block, else_block, cont_block]);

                let phi = self
                    .prog
                    .new_inst(ssa::Inst::phi(ssa::Type::Int32, expr.span));

                let branch = self.prog.new_inst(ssa::Inst::branch(
                    cond_val, then_block, else_block, expr.span,
                ));
                self.append_to_block(branch);

                // === Then Block ===
                self.block = then_block;
                let then_val = self.visit_expr(*then, ExprMode::RightValue);
                let upsilon = self
                    .prog
                    .new_inst(ssa::Inst::upsilon(phi, then_val, expr.span));
                let jmp_then = self.prog.new_inst(ssa::Inst::jump(cont_block, expr.span));
                self.append_slice_to_block(&[upsilon, jmp_then]);

                // === Else Block ===
                self.block = else_block;
                let else_val = self.visit_expr(*otherwise, ExprMode::RightValue);
                let upsilon = self
                    .prog
                    .new_inst(ssa::Inst::upsilon(phi, else_val, expr.span));
                let jmp_else = self.prog.new_inst(ssa::Inst::jump(cont_block, expr.span));
                self.append_slice_to_block(&[upsilon, jmp_else]);

                // === Merge Block ===
                self.block = cont_block;
                self.append_to_block(phi);
                phi
            }
        }
    }

    fn build_unary(
        &mut self,
        op: ssa::UnaryOp,
        expr: ast::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst(ssa::Inst::unary(ssa::Type::Int32, op, val, span));
        self.append_to_block(inst);

        inst
    }

    fn build_prefix_unary(
        &mut self,
        op: ssa::UnaryOp,
        expr: ast::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst(ssa::Inst::unary(ssa::Type::Int32, op, val, span));
        self.append_to_block(inst);

        let ptr = self.visit_expr(expr, ExprMode::LeftValue);
        let store = self.prog.new_inst(ssa::Inst::store(ptr, inst, span));
        self.append_to_block(store);

        inst
    }

    fn build_postfix_unary(
        &mut self,
        op: ssa::UnaryOp,
        expr: ast::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let val = self.visit_expr(expr, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst(ssa::Inst::unary(ssa::Type::Int32, op, val, span));
        self.append_to_block(inst);

        let ptr = self.visit_expr(expr, ExprMode::LeftValue);
        let store = self.prog.new_inst(ssa::Inst::store(ptr, inst, span));
        self.append_to_block(store);

        val
    }

    fn build_binary(
        &mut self,
        op: ssa::BinaryOp,
        lhs: ast::ExprRef,
        rhs: ast::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let lhs = self.visit_expr(lhs, ExprMode::RightValue);
        let rhs = self.visit_expr(rhs, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst(ssa::Inst::binary(ssa::Type::Int32, op, lhs, rhs, span));
        self.append_to_block(inst);

        inst
    }

    fn build_binary_assign(
        &mut self,
        op: ssa::BinaryOp,
        lhs: ast::ExprRef,
        rhs: ast::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let l = self.visit_expr(lhs, ExprMode::RightValue);
        let r = self.visit_expr(rhs, ExprMode::RightValue);
        let inst = self
            .prog
            .new_inst(ssa::Inst::binary(ssa::Type::Int32, op, l, r, span));
        self.append_to_block(inst);

        let ptr = self.visit_expr(lhs, ExprMode::LeftValue);
        let store = self.prog.new_inst(ssa::Inst::store(ptr, inst, span));
        self.append_to_block(store);

        inst
    }

    fn build_short_circuit(
        &mut self,
        op: LogicalOp,
        lhs: ast::ExprRef,
        rhs: ast::ExprRef,
        span: SourceSpan,
    ) -> ssa::InstRef {
        let rhs_name = match op {
            LogicalOp::Or => "or",
            LogicalOp::And => "and",
        };
        let rhs_name = self.prog.interner.intern(rhs_name);
        let rhs_block = self.prog.new_block(Block {
            name: rhs_name,
            span,
            ..Default::default()
        });
        let cont_name = match op {
            LogicalOp::Or => "or.cont",
            LogicalOp::And => "and.cont",
        };
        let cont_name = self.prog.interner.intern(cont_name);
        let cont_block = self.prog.new_block(Block {
            name: cont_name,
            span,
            ..Default::default()
        });
        self.prog
            .func_mut(self.func)
            .blocks
            .extend_from_slice(&[rhs_block, cont_block]);

        let phi = self.prog.new_inst(ssa::Inst::phi(ssa::Type::Int32, span));

        // === LHS Block ===
        let lhs_val = self.visit_expr(lhs, ExprMode::RightValue);

        let short_circuit_val = self.prog.new_inst(match op {
            LogicalOp::Or => ssa::Inst::const_i32(1, span),
            LogicalOp::And => ssa::Inst::const_i32(0, span),
        });

        let upsilon = self
            .prog
            .new_inst(ssa::Inst::upsilon(phi, short_circuit_val, span));

        let (true_target, false_target) = match op {
            LogicalOp::Or => (cont_block, rhs_block),
            LogicalOp::And => (rhs_block, cont_block),
        };

        let branch =
            self.prog
                .new_inst(ssa::Inst::branch(lhs_val, true_target, false_target, span));

        self.append_slice_to_block(&[short_circuit_val, upsilon, branch]);

        // === RHS Block ===
        self.block = rhs_block;
        let rhs_val = self.visit_expr(rhs, ExprMode::RightValue);

        let zero_val = self.prog.new_inst(ssa::Inst::const_i32(0, span));

        let is_nonzero = self.prog.new_inst(ssa::Inst::binary(
            ssa::Type::Int32,
            ssa::BinaryOp::NotEqual,
            rhs_val,
            zero_val,
            span,
        ));

        let upsilon = self
            .prog
            .new_inst(ssa::Inst::upsilon(phi, is_nonzero, span));

        let jmp_rhs = self.prog.new_inst(ssa::Inst::jump(cont_block, span));

        self.append_slice_to_block(&[zero_val, is_nonzero, upsilon, jmp_rhs]);

        // === Merge Block ===
        self.block = cont_block;
        self.append_to_block(phi);
        phi
    }
}

// ---------------------------------------------------------------------------
// ExprMode
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ExprMode {
    LeftValue,
    RightValue,
}

// ---------------------------------------------------------------------------
// LogicalOp
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LogicalOp {
    Or,
    And,
}
