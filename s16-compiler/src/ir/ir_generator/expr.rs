use crate::ast::ast_ir::{self, BinOp as AstBinOp, Expr, UnOp};
use crate::ir::*;
use super::context::Gen;

impl Gen {
    /// Lower condition: if false → GOTO target (fall through on true)
    pub fn lower_condition_branch_false(&mut self, cond: &Expr, target: &str) {
        match cond {
            Expr::Binary { op: AstBinOp::And, left, right, .. } => {
                self.lower_condition_branch_false(left, target);
                self.lower_condition_branch_false(right, target);
            }
            Expr::Binary { op: AstBinOp::Or, left, right, .. } => {
                let mid = self.new_label();
                self.lower_condition_branch_false(left, &mid);
                self.lower_condition_branch_false(right, target);
                self.emit(Instr::Label(mid));
            }
            Expr::Unary { op: UnOp::Not, operand, .. } => {
                self.lower_condition_branch_true(operand, target);
            }
            Expr::Binary { op, left, right, .. } if is_rel(*op) => {
                let l = self.eval_as_value(left);
                let r = self.eval_as_value(right);
                let inv_op = invert_rel(*op);
                self.emit(Instr::IfCmpGoto {
                    left: l,
                    op: inv_op,
                    right: r,
                    target: target.to_string(),
                });
            }
            _ => {
                let v = self.eval_as_value(cond);
                self.emit(Instr::IfCmpGoto {
                    left: v,
                    op: RelOp::Eq,
                    right: Value::Imm(0),
                    target: target.to_string(),
                });
            }
        }
    }

    /// Lower condition: if true → GOTO target (fall through on false)
    pub fn lower_condition_branch_true(&mut self, cond: &Expr, target: &str) {
        match cond {
            Expr::Binary { op: AstBinOp::And, left, right, .. } => {
                let after = self.new_label();
                self.lower_condition_branch_false(left, &after);
                self.lower_condition_branch_true(right, target);
                self.emit(Instr::Label(after));
            }
            Expr::Binary { op: AstBinOp::Or, left, right, .. } => {
                self.lower_condition_branch_true(left, target);
                self.lower_condition_branch_true(right, target);
            }
            Expr::Unary { op: UnOp::Not, operand, .. } => {
                self.lower_condition_branch_false(operand, target);
            }
            Expr::Binary { op, left, right, .. } if is_rel(*op) => {
                let l = self.eval_as_value(left);
                let r = self.eval_as_value(right);
                let direct_op = map_rel(*op);
                self.emit(Instr::IfCmpGoto {
                    left: l,
                    op: direct_op,
                    right: r,
                    target: target.to_string(),
                });
            }
            _ => {
                let v = self.eval_as_value(cond);
                self.emit(Instr::IfCmpGoto {
                    left: v,
                    op: RelOp::Neq,
                    right: Value::Imm(0),
                    target: target.to_string(),
                });
            }
        }
    }

    /// Materialize a boolean expression to a temp containing 0 or 1 (with short-circuit where possible)
    pub fn lower_bool_expr(&mut self, cond: &Expr) -> Var {
        let tmp = self.new_temp();
        let true_label = self.new_label();
        let end_label = self.new_label();

        self.lower_condition_branch_true(cond, &true_label);

        self.emit(Instr::Assign {
            dst: tmp.clone(),
            src: Rhs::Value(Value::Imm(0)),
        });
        self.emit(Instr::Goto(end_label.clone()));

        self.emit(Instr::Label(true_label));
        self.emit(Instr::Assign {
            dst: tmp.clone(),
            src: Rhs::Value(Value::Imm(1)),
        });

        self.emit(Instr::Label(end_label));
        tmp
    }

    pub fn eval_as_value(&mut self, e: &Expr) -> Value {
        self.with_ast_context(e.id(), None, |this| match e {
            Expr::Number(_, n) => Value::Imm(*n),
            Expr::Variable(_, name) => Value::Var(this.get_var(name.clone())),
            Expr::Unary { op: ast_ir::UnOp::Not, .. } => {
                let tmp = this.lower_bool_expr(e);
                Value::Var(tmp)
            }
            Expr::Unary { op: ast_ir::UnOp::Neg, operand, .. } => {
                let v = this.eval_as_value(operand);
                let tmp = this.new_temp();
                this.emit(Instr::Assign {
                    dst: tmp.clone(),
                    src: Rhs::Binary {
                        op: ArithOp::Sub,
                        left: Value::Imm(0),
                        right: v,
                    },
                });
                Value::Var(tmp)
            }
            Expr::Binary { left, op, right, .. } => {
                if matches!(op, AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul | AstBinOp::Div) {
                    let lv = this.eval_as_value(left);
                    let rv = this.eval_as_value(right);
                    let arith_op = map_arith(*op);
                    let tmp = this.new_temp();
                    this.emit(Instr::Assign {
                        dst: tmp.clone(),
                        src: Rhs::Binary {
                            op: arith_op,
                            left: lv,
                            right: rv,
                        },
                    });
                    Value::Var(tmp)
                } else {
                    // Relational or logical → materialize to 0/1
                    let tmp = this.lower_bool_expr(e);
                    Value::Var(tmp)
                }
            }
            Expr::AddrOf(_, name) => Value::AddrOf(name.clone()),
            Expr::Deref(_, ptr) => {
                let addr = this.eval_as_pointer_expr(ptr);
                let tmp = this.new_temp();
                this.emit(Instr::Load { dst: tmp.clone(), addr });
                Value::Var(tmp)
            }
            Expr::Index { base, index, .. } => {
                let index_v = this.eval_as_value(index);
                let tmp = this.new_temp();
                this.emit(Instr::ArrayLoad {
                    dst: tmp.clone(),
                    base: base.clone(),
                    index: index_v,
                });
                Value::Var(tmp)
            }
            Expr::Call { name, args, .. } => {
                let mut vs = Vec::new();
                for a in args {
                    vs.push(this.eval_as_value(a));
                }
                let tmp = this.new_temp();
                this.emit(Instr::Call {
                    func: name.clone(),
                    args: vs,
                    ret: Some(tmp.clone()),
                });
                Value::Var(tmp)
            }
        })
    }

    pub fn eval_as_pointer_expr(&mut self, e: &Expr) -> Value {
        match e {
            Expr::Variable(_, name) => Value::AddrOf(name.clone()),
            Expr::AddrOf(_, name) => Value::AddrOf(name.clone()),
            _ => self.eval_as_value(e), // fallback (e.g., complex pointer arithmetic)
        }
    }
}

pub fn is_rel(op: AstBinOp) -> bool {
    matches!(op, AstBinOp::Eq | AstBinOp::Neq | AstBinOp::Lt | AstBinOp::Gt | AstBinOp::Le | AstBinOp::Ge)
}

pub fn map_arith(op: AstBinOp) -> ArithOp {
    match op {
        AstBinOp::Add => ArithOp::Add,
        AstBinOp::Sub => ArithOp::Sub,
        AstBinOp::Mul => ArithOp::Mul,
        AstBinOp::Div => ArithOp::Div,
        _ => unreachable!(),
    }
}

pub fn map_rel(op: AstBinOp) -> RelOp {
    match op {
        AstBinOp::Eq => RelOp::Eq,
        AstBinOp::Neq => RelOp::Neq,
        AstBinOp::Lt => RelOp::Lt,
        AstBinOp::Gt => RelOp::Gt,
        AstBinOp::Le => RelOp::Le,
        AstBinOp::Ge => RelOp::Ge,
        _ => unreachable!(),
    }
}

pub fn invert_rel(op: AstBinOp) -> RelOp {
    match op {
        AstBinOp::Eq => RelOp::Neq,
        AstBinOp::Neq => RelOp::Eq,
        AstBinOp::Lt => RelOp::Ge,
        AstBinOp::Gt => RelOp::Le,
        AstBinOp::Le => RelOp::Gt,
        AstBinOp::Ge => RelOp::Lt,
        _ => unreachable!(),
    }
}
