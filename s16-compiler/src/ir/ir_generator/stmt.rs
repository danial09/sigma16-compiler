use crate::ast::ast_ir::{Expr, LValue, Program, Stmt, BinOp as AstBinOp};
use crate::ir::*;
use super::context::{Gen, FunctionCtx};
use super::expr::map_arith;

impl Gen {
    pub fn lower_program(&mut self, p: &Program) {
        for s in &p.statements {
            self.lower_stmt(s);
        }
    }

    pub fn lower_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Assign { id, target, value } => {
                self.with_ast_context(*id, None, |this| this.lower_assign_lvalue(target, value));
            }
            Stmt::If { id, condition, then_branch, else_branch } => {
                self.lower_if(*id, condition, then_branch, else_branch.as_ref());
            }
            Stmt::While { id, condition, body } => {
                self.lower_while(*id, condition, body);
            }
            Stmt::For { id, var, from, to, body } => {
                self.lower_for(*id, var, from, to, body);
            }
            Stmt::ArrayDecl { id, name, size, initial_values } => {
                self.with_ast_context(*id, None, |this| {
                    this.out.arrays.push((name.clone(), *size, initial_values.clone()));
                    this.emit(Instr::ArrayDecl {
                        name: name.clone(),
                        size: *size,
                        initial_values: initial_values.clone(),
                    });
                });
            }
            Stmt::Function { id, name, params, body } => {
                self.lower_function(*id, name, params, body);
            }
            Stmt::Return { id, value } => {
                self.with_ast_context(*id, None, |this| {
                    if let Some(ctx) = &mut this.fn_ctx {
                        ctx.had_return = true;
                    }
                    let v = this.eval_as_value(value);
                    this.emit(Instr::Return { value: Some(v) });
                });
            }
            Stmt::ExprStmt { id, expr } => {
                self.with_ast_context(*id, None, |this| {
                    let _ = this.eval_as_value(expr);
                });
            }
        }
    }

    pub fn lower_assign_lvalue(&mut self, target: &LValue, rhs: &Expr) {
        match target {
            LValue::Var(name) => self.lower_assign(name, rhs),
            LValue::Deref(ptr_expr) => {
                let addr = self.eval_as_pointer_expr(ptr_expr);
                let v = self.eval_as_value(rhs);
                self.emit(Instr::Store { addr, src: v });
            }
            LValue::Index { base, index } => {
                let index_v = self.eval_as_value(index);
                let src_v = self.eval_as_value(rhs);
                self.emit(Instr::ArrayStore {
                    base: base.clone(),
                    index: index_v,
                    src: src_v,
                });
            }
        }
    }

    pub fn lower_assign(&mut self, name: &str, rhs: &Expr) {
        // Prefer direct binary assignment for arithmetic to avoid unnecessary temps
        if let Expr::Binary { left, op, right, .. } = rhs {
            if matches!(op, AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul | AstBinOp::Div) {
                let l = self.eval_as_value(left);
                let r = self.eval_as_value(right);
                let op = map_arith(*op);
                self.emit(Instr::Assign {
                    dst: self.get_var(name.to_string()),
                    src: Rhs::Binary { op, left: l, right: r },
                });
                return;
            }
        }

        let v = self.eval_as_value(rhs);
        self.emit(Instr::Assign {
            dst: self.get_var(name.to_string()),
            src: Rhs::Value(v),
        });
    }

    pub fn lower_if(
        &mut self,
        ast_id: AstNodeId,
        cond: &Expr,
        then_blk: &[Stmt],
        else_blk: Option<&Vec<Stmt>>,
    ) {
        let endif = self.new_label();
        let else_label = else_blk.map(|_| self.new_label());
        let false_target = else_label.as_deref().unwrap_or(&endif);

        self.with_ast_context(ast_id, Some(ControlFlowComponent::Condition), |this| {
            this.lower_condition_branch_false(cond, false_target);
        });

        self.with_parent_component(ControlFlowComponent::ThenBranch, |this| this.emit_block(then_blk));

        if else_label.is_some() {
            self.with_ast_context(ast_id, Some(ControlFlowComponent::ControlFlowGlue), |this| {
                this.emit(Instr::Goto(endif.clone()));
            });
        }

        if let Some(lbl) = else_label {
            self.with_ast_context(ast_id, Some(ControlFlowComponent::ControlFlowGlue), |this| {
                this.emit(Instr::Label(lbl));
            });
            if let Some(blk) = else_blk {
                self.with_parent_component(ControlFlowComponent::ElseBranch, |this| this.emit_block(blk));
            }
        }

        self.with_ast_context(ast_id, Some(ControlFlowComponent::ControlFlowGlue), |this| {
            this.emit(Instr::Label(endif));
        });
    }

    pub fn lower_while(&mut self, ast_id: AstNodeId, cond: &Expr, body: &[Stmt]) {
        let start = self.new_label();
        let end = self.new_label();

        self.emit(Instr::Label(start.clone()));

        self.with_ast_context(ast_id, Some(ControlFlowComponent::Condition), |this| {
            this.lower_condition_branch_false(cond, &end);
        });

        self.with_parent_component(ControlFlowComponent::LoopBody, |this| this.emit_block(body));

        self.emit(Instr::Goto(start));
        self.emit(Instr::Label(end));
    }

    pub fn lower_for(
        &mut self,
        ast_id: AstNodeId,
        var: &str,
        from: &Expr,
        to: &Expr,
        body: &[Stmt],
    ) {
        let start = self.new_label();
        let end = self.new_label();

        // Loop variable follows the global-by-default rule
        let var_v = self.get_var(var.to_string());

        // Initialize loop variable
        let from_v = self.eval_as_value(from);
        self.emit(Instr::Assign {
            dst: var_v.clone(),
            src: Rhs::Value(from_v),
        });

        self.emit(Instr::Label(start.clone()));

        // Condition: var > to → exit (equivalent to !(var <= to))
        let to_v = self.eval_as_value(to);
        self.with_ast_context(ast_id, Some(ControlFlowComponent::Condition), |this| {
            this.emit(Instr::IfCmpGoto {
                left: Value::Var(var_v.clone()),
                op: RelOp::Gt,
                right: to_v,
                target: end.clone(),
            });
        });

        self.with_parent_component(ControlFlowComponent::LoopBody, |this| this.emit_block(body));

        // Increment
        self.emit(Instr::Assign {
            dst: var_v.clone(),
            src: Rhs::Binary {
                op: ArithOp::Add,
                left: Value::Var(var_v),
                right: Value::Imm(1),
            },
        });

        self.emit(Instr::Goto(start));
        self.emit(Instr::Label(end));
    }

    pub fn emit_block(&mut self, stmts: &[Stmt]) {
        for s in stmts {
            self.lower_stmt(s);
        }
    }

    pub fn lower_function(&mut self, _ast_id: AstNodeId, name: &str, params: &[String], body: &[Stmt]) {
        self.emit(Instr::FuncStart {
            name: name.to_string(),
            params: params.to_vec(),
        });

        let prev_ctx = self.fn_ctx.take();
        self.fn_ctx = Some(FunctionCtx { had_return: false });

        self.emit_block(body);

        self.fn_ctx = prev_ctx;

        self.emit(Instr::FuncEnd {
            name: name.to_string(),
        });
    }
}
