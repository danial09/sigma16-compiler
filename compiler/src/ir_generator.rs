use crate::ast::{self, BinOp as AstBinOp, Expr, Program, Stmt};
use crate::ir::*;

pub fn lower(program: &Program) -> ProgramIR {
    let mut g = Gen::new();
    g.lower_program(program);
    g.finish()
}

struct Gen {
    out: ProgramIR,
    temp_count: usize,
    label_count: usize,
}

impl Gen {
    fn new() -> Self {
        Self { out: ProgramIR::new(), temp_count: 0, label_count: 0 }
    }

    fn finish(self) -> ProgramIR { self.out }

    fn new_temp(&mut self) -> String {
        let t = format!("__t{}", self.temp_count);
        self.temp_count += 1;
        t
    }

    fn new_label(&mut self) -> String {
        let l = format!("L{}", self.label_count);
        self.label_count += 1;
        l
    }

    fn emit(&mut self, i: Instr) {
        self.out.instrs.push(i);
    }

    // ========== Top-Level Lowering ==========

    fn lower_program(&mut self, p: &Program) {
        for s in &p.statements {
            self.lower_stmt(s);
        }
    }

    fn lower_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Assign { name, value } => self.lower_assign(name, value),
            Stmt::If { condition, then_branch, else_branch } => {
                self.lower_if(condition, then_branch, else_branch.as_ref())
            }
            Stmt::While { condition, body } => self.lower_while(condition, body),
        }
    }

    // ========== Statement Lowering ==========

    fn lower_assign(&mut self, name: &str, rhs: &Expr) {
        let src = match rhs {
            Expr::Number(n) => Rhs::Value(Value::Imm(*n)),
            Expr::Variable(v) => Rhs::Value(Value::Var(v.clone())),
            Expr::Binary { op, left, right } if is_arith(*op) => {
                let (l, r, aop) = self.lower_arith_expr(left, right, *op);
                Rhs::Binary { op: aop, left: l, right: r }
            }
            other => Rhs::Value(self.eval_as_value(other)),
        };

        self.emit(Instr::Assign {
            dst: name.to_string(),
            src,
        });
    }

    fn lower_if(&mut self, cond: &Expr, then_blk: &[Stmt], else_blk: Option<&Vec<Stmt>>) {
        if is_simple_condition(cond) {
            self.lower_if_simple(cond, then_blk, else_blk);
        } else {
            self.lower_if_complex(cond, then_blk, else_blk);
        }
    }

    fn lower_if_simple(&mut self, cond: &Expr, then_blk: &[Stmt], else_blk: Option<&Vec<Stmt>>) {
        let end_label = self.new_label();
        let else_label = else_blk.as_ref().map(|_| self.new_label());
        let target = else_label.as_ref().unwrap_or(&end_label);

        self.emit_simple_branch_on_false(cond, target);
        self.emit_block(then_blk);

        if let Some(label) = else_label {
            self.emit(Instr::Goto(end_label.clone()));
            self.emit(Instr::Label(label));
            if let Some(stmts) = else_blk {
                self.emit_block(stmts);
            }
        }

        self.emit(Instr::Label(end_label));
    }

    fn lower_if_complex(&mut self, cond: &Expr, then_blk: &[Stmt], else_blk: Option<&Vec<Stmt>>) {
        let cond_temp = self.materialize_condition(cond);
        let end_label = self.new_label();
        let else_label = else_blk.as_ref().map(|_| self.new_label());
        let target = else_label.as_ref().unwrap_or(&end_label).clone();

        self.emit(Instr::IfCmpGoto {
            left: cond_temp,
            op: RelOp::Eq,
            right: Value::Imm(0),
            target,
        });

        self.emit_block(then_blk);

        if let Some(label) = else_label {
            self.emit(Instr::Goto(end_label.clone()));
            self.emit(Instr::Label(label));
            if let Some(stmts) = else_blk {
                self.emit_block(stmts);
            }
        }

        self.emit(Instr::Label(end_label));
    }

    fn lower_while(&mut self, cond: &Expr, body: &[Stmt]) {
        let start = self.new_label();
        let end = self.new_label();

        self.emit(Instr::Label(start.clone()));

        if is_simple_condition(cond) {
            self.emit_simple_branch_on_false(cond, &end);
        } else {
            let cond_temp = self.materialize_condition(cond);
            self.emit(Instr::IfCmpGoto {
                left: cond_temp,
                op: RelOp::Eq,
                right: Value::Imm(0),
                target: end.clone(),
            });
        }

        self.emit_block(body);
        self.emit(Instr::Goto(start));
        self.emit(Instr::Label(end));
    }

    fn emit_block(&mut self, stmts: &[Stmt]) {
        for s in stmts {
            self.lower_stmt(s);
        }
    }

    // ========== Simple Condition Branching ==========

    fn emit_simple_branch_on_false(&mut self, cond: &Expr, target: &str) {
        match cond {
            Expr::Unary { op: ast::UnOp::Not, operand } => {
                self.emit_simple_branch_on_true(operand, target);
            }
            Expr::Binary { op, left, right } if is_rel(*op) => {
                let l = self.eval_as_value(left);
                let r = self.eval_as_value(right);
                self.emit(Instr::IfCmpGoto {
                    left: l,
                    op: invert_rel(*op),
                    right: r,
                    target: target.to_string(),
                });
            }
            _ => unreachable!("emit_simple_branch_on_false called with complex condition"),
        }
    }

    fn emit_simple_branch_on_true(&mut self, cond: &Expr, target: &str) {
        match cond {
            Expr::Unary { op: ast::UnOp::Not, operand } => {
                self.emit_simple_branch_on_false(operand, target);
            }
            Expr::Binary { op, left, right } if is_rel(*op) => {
                let l = self.eval_as_value(left);
                let r = self.eval_as_value(right);
                self.emit(Instr::IfCmpGoto {
                    left: l,
                    op: map_rel(*op),
                    right: r,
                    target: target.to_string(),
                });
            }
            _ => unreachable!("emit_simple_branch_on_true called with complex condition"),
        }
    }

    // ========== Complex Condition Materialization ==========

    fn materialize_condition(&mut self, cond: &Expr) -> Value {
        match cond {
            Expr::Number(n) => Value::Imm(*n),
            Expr::Variable(v) => Value::Var(v.clone()),

            Expr::Unary { op: ast::UnOp::Not, operand } => self.materialize_not(operand),

            Expr::Binary { op, left, right } if is_rel(*op) => {
                self.materialize_relational(left, right, *op)
            }

            Expr::Binary { op: AstBinOp::And, left, right } => {
                self.materialize_and(left, right)
            }

            Expr::Binary { op: AstBinOp::Or, left, right } => {
                self.materialize_or(left, right)
            }

            Expr::Binary { op, .. } if is_arith(*op) => self.eval_as_value(cond),

            _ => unreachable!("Unhandled condition type: {:?}", cond),
        }
    }

    fn materialize_not(&mut self, operand: &Expr) -> Value {
        let v = self.materialize_condition(operand);
        let t = self.new_temp();
        let skip = self.new_label();

        // t = 1 if v == 0, else t = 0
        self.emit(Instr::Assign {
            dst: t.clone(),
            src: Rhs::Value(Value::Imm(1)),
        });
        self.emit(Instr::IfCmpGoto {
            left: v,
            op: RelOp::Eq,
            right: Value::Imm(0),
            target: skip.clone(),
        });
        self.emit(Instr::Assign {
            dst: t.clone(),
            src: Rhs::Value(Value::Imm(0)),
        });
        self.emit(Instr::Label(skip));

        Value::Var(t)
    }

    fn materialize_relational(&mut self, left: &Expr, right: &Expr, op: AstBinOp) -> Value {
        let l = self.eval_as_value(left);
        let r = self.eval_as_value(right);
        let t = self.new_temp();
        let set_true = self.new_label();
        let done = self.new_label();

        self.emit(Instr::Assign {
            dst: t.clone(),
            src: Rhs::Value(Value::Imm(0)),
        });
        self.emit(Instr::IfCmpGoto {
            left: l,
            op: map_rel(op),
            right: r,
            target: set_true.clone(),
        });
        self.emit(Instr::Goto(done.clone()));
        self.emit(Instr::Label(set_true));
        self.emit(Instr::Assign {
            dst: t.clone(),
            src: Rhs::Value(Value::Imm(1)),
        });
        self.emit(Instr::Label(done));

        Value::Var(t)
    }

    fn materialize_and(&mut self, left: &Expr, right: &Expr) -> Value {
        let t = self.new_temp();
        let done = self.new_label();

        self.emit(Instr::Assign {
            dst: t.clone(),
            src: Rhs::Value(Value::Imm(0)),
        });

        // Short-circuit: if left is false, done
        let left_val = self.materialize_condition(left);
        self.emit(Instr::IfCmpGoto {
            left: left_val,
            op: RelOp::Eq,
            right: Value::Imm(0),
            target: done.clone(),
        });

        // Left is true, check right
        let right_val = self.materialize_condition(right);
        self.emit(Instr::IfCmpGoto {
            left: right_val,
            op: RelOp::Eq,
            right: Value::Imm(0),
            target: done.clone(),
        });

        // Both true
        self.emit(Instr::Assign {
            dst: t.clone(),
            src: Rhs::Value(Value::Imm(1)),
        });
        self.emit(Instr::Label(done));

        Value::Var(t)
    }

    fn materialize_or(&mut self, left: &Expr, right: &Expr) -> Value {
        let t = self.new_temp();
        let set_true = self.new_label();
        let done = self.new_label();

        self.emit(Instr::Assign {
            dst: t.clone(),
            src: Rhs::Value(Value::Imm(0)),
        });

        // Short-circuit: if left is true, set true
        let left_val = self.materialize_condition(left);
        self.emit(Instr::IfCmpGoto {
            left: left_val,
            op: RelOp::Neq,
            right: Value::Imm(0),
            target: set_true.clone(),
        });

        // Left is false, check right
        let right_val = self.materialize_condition(right);
        self.emit(Instr::IfCmpGoto {
            left: right_val,
            op: RelOp::Eq,
            right: Value::Imm(0),
            target: done.clone(),
        });

        // At least one is true
        self.emit(Instr::Label(set_true));
        self.emit(Instr::Assign {
            dst: t.clone(),
            src: Rhs::Value(Value::Imm(1)),
        });
        self.emit(Instr::Label(done));

        Value::Var(t)
    }

    // ========== Expression Evaluation ==========

    fn eval_as_value(&mut self, e: &Expr) -> Value {
        match e {
            Expr::Number(n) => Value::Imm(*n),
            Expr::Variable(v) => Value::Var(v.clone()),

            Expr::Unary { op: ast::UnOp::Not, .. } => self.materialize_condition(e),

            Expr::Binary { op, left, right } if is_arith(*op) => {
                let (l, r, aop) = self.lower_arith_expr(left, right, *op);
                let t = self.new_temp();
                self.emit(Instr::Assign {
                    dst: t.clone(),
                    src: Rhs::Binary { op: aop, left: l, right: r },
                });
                Value::Var(t)
            }

            Expr::Binary { op, .. } if is_rel(*op) || matches!(op, AstBinOp::And | AstBinOp::Or) => {
                self.materialize_condition(e)
            }

            _ => unreachable!("Unhandled expression form: {:?}", e),
        }
    }

    fn lower_arith_expr(&mut self, l: &Expr, r: &Expr, op: AstBinOp) -> (Value, Value, ArithOp) {
        let left = self.eval_atom_or_complex(l);
        let right = self.eval_atom_or_complex(r);
        (left, right, map_arith(op))
    }

    fn eval_atom_or_complex(&mut self, e: &Expr) -> Value {
        match e {
            Expr::Number(n) => Value::Imm(*n),
            Expr::Variable(v) => Value::Var(v.clone()),
            _ => self.eval_as_value(e),
        }
    }
}

// ========== Helper Functions ==========

fn is_simple_condition(e: &Expr) -> bool {
    match e {
        Expr::Binary { op, .. } if is_rel(*op) => true,
        Expr::Unary { op: ast::UnOp::Not, operand } => is_simple_condition(operand),
        _ => false,
    }
}

fn is_rel(op: AstBinOp) -> bool {
    matches!(
        op,
        AstBinOp::Eq | AstBinOp::Neq | AstBinOp::Lt | AstBinOp::Gt
    )
}

fn is_arith(op: AstBinOp) -> bool {
    matches!(
        op,
        AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul | AstBinOp::Div
    )
}

fn map_arith(op: AstBinOp) -> ArithOp {
    match op {
        AstBinOp::Add => ArithOp::Add,
        AstBinOp::Sub => ArithOp::Sub,
        AstBinOp::Mul => ArithOp::Mul,
        AstBinOp::Div => ArithOp::Div,
        _ => unreachable!("not arithmetic"),
    }
}

fn map_rel(op: AstBinOp) -> RelOp {
    match op {
        AstBinOp::Eq => RelOp::Eq,
        AstBinOp::Neq => RelOp::Neq,
        AstBinOp::Lt => RelOp::Lt,
        AstBinOp::Gt => RelOp::Gt,
        _ => unreachable!("not relational"),
    }
}

fn invert_rel(op: AstBinOp) -> RelOp {
    match op {
        AstBinOp::Eq => RelOp::Neq,
        AstBinOp::Neq => RelOp::Eq,
        AstBinOp::Lt => RelOp::Ge,
        AstBinOp::Gt => RelOp::Le,
        _ => unreachable!("not relational"),
    }
}
