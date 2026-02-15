use super::context::Gen;
use crate::ir::ast::{self, BinOp as AstBinOp, Expr, UnOp};
use crate::ir::symbol_table::SymbolKind;
use crate::ir::*;
use crate::{CompileError, SemanticErrorKind};

impl Gen {
    /// Lower condition: if false → GOTO target (fall through on true)
    pub fn lower_condition_branch_false(
        &mut self,
        cond: &Expr,
        target: &str,
    ) -> Result<(), CompileError> {
        match cond {
            Expr::Binary {
                op: AstBinOp::And,
                left,
                right,
                ..
            } => {
                self.lower_condition_branch_false(left, target)?;
                self.lower_condition_branch_false(right, target)?;
                Ok(())
            }
            Expr::Binary {
                op: AstBinOp::Or,
                left,
                right,
                ..
            } => {
                let mid = self.new_label();
                self.lower_condition_branch_false(left, &mid)?;
                self.lower_condition_branch_false(right, target)?;
                self.emit(Instr::Label(mid));
                Ok(())
            }
            Expr::Unary {
                op: UnOp::Not,
                operand,
                ..
            } => self.lower_condition_branch_true(operand, target),
            Expr::Binary {
                op, left, right, ..
            } if is_rel(*op) => {
                let l = self.eval_as_value(left)?;
                let r = self.eval_as_value(right)?;
                let inv_op = invert_rel(*op);
                self.emit(Instr::IfCmpGoto {
                    left: l,
                    op: inv_op,
                    right: r,
                    target: target.to_string(),
                });
                Ok(())
            }
            _ => {
                let v = self.eval_as_value(cond)?;
                self.emit(Instr::IfCmpGoto {
                    left: v,
                    op: RelOp::Eq,
                    right: Value::Imm(0),
                    target: target.to_string(),
                });
                Ok(())
            }
        }
    }

    /// Lower condition: if true → GOTO target (fall through on false)
    pub fn lower_condition_branch_true(
        &mut self,
        cond: &Expr,
        target: &str,
    ) -> Result<(), CompileError> {
        match cond {
            Expr::Binary {
                op: AstBinOp::And,
                left,
                right,
                ..
            } => {
                let after = self.new_label();
                self.lower_condition_branch_false(left, &after)?;
                self.lower_condition_branch_true(right, target)?;
                self.emit(Instr::Label(after));
                Ok(())
            }
            Expr::Binary {
                op: AstBinOp::Or,
                left,
                right,
                ..
            } => {
                self.lower_condition_branch_true(left, target)?;
                self.lower_condition_branch_true(right, target)?;
                Ok(())
            }
            Expr::Unary {
                op: UnOp::Not,
                operand,
                ..
            } => self.lower_condition_branch_false(operand, target),
            Expr::Binary {
                op, left, right, ..
            } if is_rel(*op) => {
                let l = self.eval_as_value(left)?;
                let r = self.eval_as_value(right)?;
                let direct_op = map_rel(*op);
                self.emit(Instr::IfCmpGoto {
                    left: l,
                    op: direct_op,
                    right: r,
                    target: target.to_string(),
                });
                Ok(())
            }
            _ => {
                let v = self.eval_as_value(cond)?;
                self.emit(Instr::IfCmpGoto {
                    left: v,
                    op: RelOp::Neq,
                    right: Value::Imm(0),
                    target: target.to_string(),
                });
                Ok(())
            }
        }
    }

    /// Materialize a boolean expression to a temp containing 0 or 1 (with short-circuit where possible)
    pub fn lower_bool_expr(&mut self, cond: &Expr) -> Result<Var, CompileError> {
        let tmp = self.new_temp();
        let true_label = self.new_label();
        let end_label = self.new_label();

        self.lower_condition_branch_true(cond, &true_label)?;

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
        Ok(tmp)
    }

    pub fn eval_as_value(&mut self, e: &Expr) -> Result<Value, CompileError> {
        self.with_ast_context(e.id(), None, |this| match e {
            Expr::Number(_, n) => Ok(Value::Imm(*n)),

            Expr::Variable(id, name) => {
                // Validate: Check if variable is defined
                match this.symbols.lookup(name) {
                    Some(info) => match info.kind {
                        SymbolKind::Variable => Ok(Value::Var(this.get_var(name.clone()))),
                        SymbolKind::Array => {
                            // Array decays to pointer (address) when used as a value.
                            // This allows passing arrays to functions as pointer arguments.
                            Ok(Value::AddrOf(name.clone()))
                        }
                        SymbolKind::Function => Err(this.make_error(
                            SemanticErrorKind::FunctionUsedAsVariable,
                            *id,
                            format!("Function '{}' used as variable (try {}())", name, name),
                        )),
                    },
                    None => {
                        // If not found in symbol table, treat as implicit variable declaration
                        // This matches the original behavior where variables don't need explicit declaration
                        Ok(Value::Var(this.get_var(name.clone())))
                    }
                }
            }

            Expr::Unary {
                op: ast::UnOp::Not, ..
            } => {
                let tmp = this.lower_bool_expr(e)?;
                Ok(Value::Var(tmp))
            }

            Expr::Unary {
                op: ast::UnOp::Neg,
                operand,
                ..
            } => {
                let v = this.eval_as_value(operand)?;
                let tmp = this.new_temp();
                this.emit(Instr::Assign {
                    dst: tmp.clone(),
                    src: Rhs::Binary {
                        op: ArithOp::Sub,
                        left: Value::Imm(0),
                        right: v,
                    },
                });
                Ok(Value::Var(tmp))
            }

            Expr::Binary {
                left, op, right, ..
            } => {
                if matches!(
                    op,
                    AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul | AstBinOp::Div | AstBinOp::Mod
                ) {
                    let lv = this.eval_as_value(left)?;
                    let rv = this.eval_as_value(right)?;
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
                    Ok(Value::Var(tmp))
                } else {
                    // Relational or logical → materialize to 0/1
                    let tmp = this.lower_bool_expr(e)?;
                    Ok(Value::Var(tmp))
                }
            }

            Expr::AddrOf(_, name) => Ok(Value::AddrOf(name.clone())),

            Expr::Deref(_, ptr) => {
                let addr = this.eval_as_pointer_expr(ptr)?;
                let tmp = this.new_temp();
                this.emit(Instr::Load {
                    dst: tmp.clone(),
                    addr,
                });
                Ok(Value::Var(tmp))
            }

            Expr::Index { id, base, index } => {
                // Check if base is a known symbol (locals first, then globals)
                match this.symbols.lookup(base) {
                    Some(info) if matches!(info.kind, SymbolKind::Array) => {
                        // Global array: use ArrayLoad
                        let index_v = this.eval_as_value(index)?;
                        let tmp = this.new_temp();
                        this.emit(Instr::ArrayLoad {
                            dst: tmp.clone(),
                            base: base.clone(),
                            index: index_v,
                        });
                        Ok(Value::Var(tmp))
                    }
                    Some(info) if matches!(info.kind, SymbolKind::Variable) => {
                        // Local variable (e.g. function parameter) used with indexing:
                        // treat as pointer arithmetic: *(base + index)
                        let base_v = Value::Var(this.get_var(base.clone()));
                        let index_v = this.eval_as_value(index)?;
                        let addr_tmp = this.new_temp();
                        this.emit(Instr::Assign {
                            dst: addr_tmp.clone(),
                            src: Rhs::Binary {
                                op: ArithOp::Add,
                                left: base_v,
                                right: index_v,
                            },
                        });
                        let tmp = this.new_temp();
                        this.emit(Instr::Load {
                            dst: tmp.clone(),
                            addr: Value::Var(addr_tmp),
                        });
                        Ok(Value::Var(tmp))
                    }
                    Some(_info) => Err(this.make_error(
                        SemanticErrorKind::VariableUsedAsArray,
                        *id,
                        format!("Variable '{}' used as array", base),
                    )),
                    None => Err(this.make_error(
                        SemanticErrorKind::UndefinedArray,
                        *id,
                        format!("Array '{}' is not defined", base),
                    )),
                }
            }

            Expr::Call { id, name, args } => {
                // Validate: Check if function exists
                match this.symbols.lookup_global(name) {
                    Some(info) if matches!(info.kind, SymbolKind::Function) => {
                        // Validate: Check argument count
                        if let Some(expected) = info.param_count {
                            if args.len() != expected {
                                return Err(this.make_error(
                                    SemanticErrorKind::ArgumentCountMismatch,
                                    *id,
                                    format!(
                                        "Function '{}' expects {} argument{}, got {}",
                                        name,
                                        expected,
                                        if expected == 1 { "" } else { "s" },
                                        args.len()
                                    ),
                                ));
                            }
                        }

                        let mut vs = Vec::new();
                        for a in args {
                            vs.push(this.eval_as_value(a)?);
                        }
                        let tmp = this.new_temp();
                        this.emit(Instr::Call {
                            func: name.clone(),
                            args: vs,
                            ret: Some(tmp.clone()),
                        });
                        Ok(Value::Var(tmp))
                    }
                    Some(_info) => Err(this.make_error(
                        SemanticErrorKind::ArrayUsedAsFunction,
                        *id,
                        format!("Array '{}' used as function", name),
                    )),
                    None => Err(this.make_error(
                        SemanticErrorKind::UndefinedFunction,
                        *id,
                        format!("Function '{}' is not defined", name),
                    )),
                }
            }
        })
    }

    pub fn eval_as_pointer_expr(&mut self, e: &Expr) -> Result<Value, CompileError> {
        match e {
            // Only &x produces an AddrOf — the address of the named variable.
            Expr::AddrOf(_, name) => Ok(Value::AddrOf(name.clone())),
            // Everything else (variables, complex expressions) is evaluated
            // normally — the resulting value IS the pointer (an address).
            _ => self.eval_as_value(e),
        }
    }
}

pub fn is_rel(op: AstBinOp) -> bool {
    matches!(
        op,
        AstBinOp::Eq | AstBinOp::Neq | AstBinOp::Lt | AstBinOp::Gt | AstBinOp::Le | AstBinOp::Ge
    )
}

pub fn map_arith(op: AstBinOp) -> ArithOp {
    match op {
        AstBinOp::Add => ArithOp::Add,
        AstBinOp::Sub => ArithOp::Sub,
        AstBinOp::Mul => ArithOp::Mul,
        AstBinOp::Div => ArithOp::Div,
        AstBinOp::Mod => ArithOp::Mod,
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
