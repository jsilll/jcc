use crate::{
    ast::{Ast, ExprKind, ExprRef},
    sema::Type,
};

// ---------------------------------------------------------------------------
// LoweringAction
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoweringAction {
    Cast { expr: ExprRef, ty: Type },
}

// ---------------------------------------------------------------------------
// LoweringActions
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LoweringActions {
    actions: Vec<LoweringAction>,
}

impl LoweringActions {
    #[inline]
    pub fn schedule_cast(&mut self, expr: ExprRef, ty: Type) {
        self.actions.push(LoweringAction::Cast { expr, ty })
    }
}

// ---------------------------------------------------------------------------
// LoweringPass
// ---------------------------------------------------------------------------

pub struct LoweringPass {
    ast: Ast,
    actions: LoweringActions,
}

impl LoweringPass {
    pub fn new(ast: Ast, actions: LoweringActions) -> Self {
        Self { ast, actions }
    }

    pub fn build(mut self) -> Ast {
        self.actions.actions.iter().for_each(|action| match action {
            LoweringAction::Cast { expr, ty } => {
                let copy = self.ast.new_expr(self.ast.expr(*expr).clone());
                let cast = self.ast.expr_mut(*expr);
                cast.ty = (*ty).into();
                cast.kind = ExprKind::Cast {
                    ty: *ty,
                    expr: copy,
                }
            }
        });
        self.ast
    }
}
