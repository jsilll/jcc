use crate::{InstKind, InstRef, Program, Type};

// ---------------------------------------------------------------------------
// SSAVerifierResult
// ---------------------------------------------------------------------------

#[derive(Default, Clone, PartialEq, Eq)]
pub struct SSAVerifierResult {
    pub diagnostics: Vec<SSAVerifierDiagnostic>,
}

// ---------------------------------------------------------------------------
// SSAVerifierDiagnostic
// ---------------------------------------------------------------------------

#[derive(Clone, PartialEq, Eq)]
pub enum SSAVerifierDiagnostic {
    InvalidType(InstRef),
}

// ---------------------------------------------------------------------------
// SSAVerifier
// ---------------------------------------------------------------------------

pub struct SSAVerifier<'a> {
    ssa: &'a Program,
    result: SSAVerifierResult,
}

impl<'a> SSAVerifier<'a> {
    pub fn new(ssa: &'a Program) -> Self {
        Self {
            ssa,
            result: SSAVerifierResult::default(),
        }
    }

    pub fn verify(mut self) -> SSAVerifierResult {
        // TODO: Check SSA dominance rule: each use must be dominated by its definition
        // This requires implementing dominance calculation first
        //
        // For now, iterate over all the instructions and check their types:
        // TODO: this will iterate over deleted/invalidated instructions, fix it
        for (i, inst) in self.ssa.insts_iter2() {
            match inst.kind {
                InstKind::Nop | InstKind::Phi | InstKind::Arg => {}
                InstKind::Alloca => {
                    if inst.ty != Type::IntPtr {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Const(_) => {
                    if inst.ty != Type::Int32 {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Ret(_) => {
                    if inst.ty != Type::Void {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Jump(_) => {
                    if inst.ty != Type::Void {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Load(_) => {
                    if inst.ty != Type::Int32 {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Identity(val) => {
                    if inst.ty != self.ssa.inst(val).ty {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Store { .. } => {
                    if inst.ty != Type::Void {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Upsilon { .. } => {
                    if inst.ty != Type::Void {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Unary { .. } => {
                    if inst.ty != Type::Int32 {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Binary { .. } => {
                    if inst.ty != Type::Int32 {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Select { .. } => {
                    if inst.ty != Type::Int32 {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Branch { .. } => {
                    if inst.ty != Type::Void {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
                InstKind::Switch { .. } => {
                    if inst.ty != Type::Void {
                        self.result
                            .diagnostics
                            .push(SSAVerifierDiagnostic::InvalidType(i));
                    }
                }
            }
        }
        self.result
    }
}
