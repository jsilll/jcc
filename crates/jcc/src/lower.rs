use crate::{
    ast::{Ast, ExprKind, ExprRef},
    sema::Type,
};

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
        self.actions
            .schedule
            .iter()
            .for_each(|action| match action {
                LoweringAction::Cast { ty, expr } => {
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
// LoweringAction
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoweringAction {
    Cast { ty: Type, expr: ExprRef },
}

// ---------------------------------------------------------------------------
// LoweringActions
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LoweringActions {
    schedule: Vec<LoweringAction>,
}

impl LoweringActions {
    #[inline]
    fn schedule(&mut self, action: LoweringAction) {
        self.schedule.push(action)
    }

    #[inline]
    pub fn cast(&mut self, ty: Type, expr: ExprRef) {
        self.schedule(LoweringAction::Cast { ty, expr })
    }
}
