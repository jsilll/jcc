use crate::ast::{ty::Ty, Ast, ExprKind, ExprRef};

// ---------------------------------------------------------------------------
// LoweringPass
// ---------------------------------------------------------------------------

pub struct LoweringPass<'ctx> {
    ast: Ast<'ctx>,
    actions: LoweringActions<'ctx>,
}

impl<'ctx> LoweringPass<'ctx> {
    pub fn new(ast: Ast<'ctx>, actions: LoweringActions<'ctx>) -> Self {
        Self { ast, actions }
    }

    pub fn build(mut self) -> Ast<'ctx> {
        self.actions
            .schedule
            .iter()
            .for_each(|action| match action {
                LoweringAction::Cast { ty, expr } => {
                    let copy = self.ast.exprs.push(self.ast.exprs[*expr].clone());
                    let cast = &mut self.ast.exprs[*expr];
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
// LoweringAction
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum LoweringAction<'ctx> {
    Cast { ty: Ty<'ctx>, expr: ExprRef },
}

// ---------------------------------------------------------------------------
// LoweringActions
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct LoweringActions<'ctx> {
    schedule: Vec<LoweringAction<'ctx>>,
}

impl<'ctx> LoweringActions<'ctx> {
    #[inline]
    pub fn cast(&mut self, ty: Ty<'ctx>, expr: ExprRef) {
        self.schedule.push(LoweringAction::Cast { ty, expr })
    }
}
