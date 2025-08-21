use crate::{
    ast::{Ast, ExprKind, ExprRef},
    sema::Type,
};

// ---------------------------------------------------------------------------
// LoweringPass
// ---------------------------------------------------------------------------

pub struct LoweringPass {
    ast: Ast,
    lowering_actions: LoweringActions,
}

impl LoweringPass {
    pub fn new(ast: Ast, lowering_actions: LoweringActions) -> Self {
        Self {
            ast,
            lowering_actions,
        }
    }

    pub fn build(mut self) -> Ast {
        self.lowering_actions
            .actions
            .iter()
            .for_each(|action| match action {
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

// ---------------------------------------------------------------------------
// LoweringActions
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LoweringActions {
    pub actions: Vec<LoweringAction>,
}

// ---------------------------------------------------------------------------
// LoweringAction
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoweringAction {
    Cast { expr: ExprRef, ty: Type },
}
