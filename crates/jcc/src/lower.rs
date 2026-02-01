use crate::ast::{ty::Ty, Ast, Expr, ExprKind};

// ---------------------------------------------------------------------------
// DesugarPass
// ---------------------------------------------------------------------------

pub struct DesugarPass<'ctx> {
    ast: Ast<'ctx>,
    actions: DesugarActions<'ctx>,
}

impl<'ctx> DesugarPass<'ctx> {
    pub fn new(ast: Ast<'ctx>, actions: DesugarActions<'ctx>) -> Self {
        Self { ast, actions }
    }

    pub fn build(mut self) -> Ast<'ctx> {
        self.actions
            .schedule
            .iter()
            .for_each(|action| match action {
                DesugarAction::Cast { ty, expr } => {
                    let copy = self.ast.expr.push(self.ast.expr[*expr].clone());
                    let cast = &mut self.ast.expr[*expr];
                    cast.ty.set(*ty);
                    cast.kind = ExprKind::Cast {
                        ty: *ty,
                        expr: copy,
                    }
                }
            });
        self.ast
    }
}

// ---------------------------------------------------------------------------
// DesugarAction
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum DesugarAction<'ctx> {
    Cast { ty: Ty<'ctx>, expr: Expr },
}

// ---------------------------------------------------------------------------
// DesugarActions
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct DesugarActions<'ctx> {
    schedule: Vec<DesugarAction<'ctx>>,
}

impl<'ctx> DesugarActions<'ctx> {
    #[inline]
    pub fn cast(&mut self, ty: Ty<'ctx>, expr: Expr) {
        self.schedule.push(DesugarAction::Cast { ty, expr })
    }
}
