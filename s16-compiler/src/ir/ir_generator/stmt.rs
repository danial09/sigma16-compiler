use super::context::{FunctionCtx, Gen};
use super::expr::map_arith;
use crate::ir::ast::{BinOp as AstBinOp, Expr, LValue, Program, Stmt};
use crate::ir::symbol_table::{SymbolInfo, SymbolKind};
use crate::ir::*;
use crate::{CompileError, SemanticErrorKind};

impl Gen {
    pub fn lower_program(&mut self, p: &Program) -> Result<(), CompileError> {
        // First pass: collect all global declarations (arrays and functions) for forward references
        for s in &p.statements {
            match s {
                Stmt::ArrayDecl { id, name, .. } => {
                    if let Err(existing) = self.symbols.declare_global(
                        name.clone(),
                        SymbolInfo {
                            kind: SymbolKind::Array,
                            ast_id: *id,
                            param_count: None,
                        },
                    ) {
                        let kind_str = match existing.kind {
                            SymbolKind::Array => "array",
                            SymbolKind::Variable => "variable",
                            SymbolKind::Function => "function",
                        };
                        return Err(self.make_error(
                            SemanticErrorKind::ArrayRedefinition,
                            *id,
                            format!("Array '{}' already defined as {}", name, kind_str),
                        ));
                    }
                }
                Stmt::Function {
                    id, name, params, ..
                } => {
                    if let Err(existing) = self.symbols.declare_global(
                        name.clone(),
                        SymbolInfo {
                            kind: SymbolKind::Function,
                            ast_id: *id,
                            param_count: Some(params.len()),
                        },
                    ) {
                        let kind_str = match existing.kind {
                            SymbolKind::Function => "function",
                            SymbolKind::Array => "array",
                            SymbolKind::Variable => "variable",
                        };
                        return Err(self.make_error(
                            SemanticErrorKind::FunctionRedefinition,
                            *id,
                            format!("Function '{}' already defined as {}", name, kind_str),
                        ));
                    }
                }
                Stmt::StringDecl { id, name, .. } => {
                    if let Err(existing) = self.symbols.declare_global(
                        name.clone(),
                        SymbolInfo {
                            kind: SymbolKind::Array,
                            ast_id: *id,
                            param_count: None,
                        },
                    ) {
                        let kind_str = match existing.kind {
                            SymbolKind::Array => "array",
                            SymbolKind::Variable => "variable",
                            SymbolKind::Function => "function",
                        };
                        return Err(self.make_error(
                            SemanticErrorKind::ArrayRedefinition,
                            *id,
                            format!("String '{}' already defined as {}", name, kind_str),
                        ));
                    }
                }
                _ => {}
            }
        }

        // Second pass: lower all statements to IR
        for s in &p.statements {
            self.lower_stmt(s)?;
        }
        Ok(())
    }

    pub fn lower_stmt(&mut self, s: &Stmt) -> Result<(), CompileError> {
        match s {
            Stmt::Assign { id, target, value } => {
                self.with_ast_context(*id, None, |this| this.lower_assign_lvalue(target, value))
            }
            Stmt::If {
                id,
                condition,
                then_branch,
                else_branch,
            } => self.lower_if(*id, condition, then_branch, else_branch.as_ref()),
            Stmt::While {
                id,
                condition,
                body,
            } => self.lower_while(*id, condition, body),
            Stmt::For {
                id,
                var,
                from,
                to,
                body,
            } => self.lower_for(*id, var, from, to, body),
            Stmt::ArrayDecl {
                id,
                name,
                size,
                initial_values,
            } => {
                self.with_ast_context(*id, None, |this| {
                    // Declaration already registered in first pass
                    let ir_idx = this.out.instrs.len();
                    this.out.arrays.push((
                        name.clone(),
                        *size,
                        initial_values.clone(),
                        Some(ir_idx),
                    ));
                    this.emit(Instr::ArrayDecl {
                        name: name.clone(),
                        size: *size,
                        initial_values: initial_values.clone(),
                    });
                    Ok(())
                })
            }
            Stmt::Function {
                id,
                name,
                params,
                body,
            } => {
                // Declaration already registered in first pass
                self.lower_function(*id, name, params, body)?;
                Ok(())
            }
            Stmt::Return { id, value } => {
                self.with_ast_context(*id, None, |this| {
                    // Validate: Check if inside function
                    if this.fn_ctx.is_none() {
                        return Err(this.make_error(
                            SemanticErrorKind::ReturnOutsideFunction,
                            *id,
                            "Return statement outside of function body".to_string(),
                        ));
                    }

                    if let Some(ctx) = &mut this.fn_ctx {
                        ctx.had_return = true;
                    }
                    let v = this.eval_as_value(value)?;
                    this.emit(Instr::Return { value: Some(v) });
                    Ok(())
                })
            }
            Stmt::ExprStmt { id, expr } => self.with_ast_context(*id, None, |this| {
                let _ = this.eval_as_value(expr)?;
                Ok(())
            }),
            Stmt::StringDecl { id, name, value } => {
                self.with_ast_context(*id, None, |this| {
                    // Declaration already registered in first pass
                    let mut vals: Vec<i64> = value.chars().map(|c| c as i64).collect();
                    vals.push(0); // null terminator
                    let size = vals.len();
                    let ir_idx = this.out.instrs.len();
                    this.out
                        .arrays
                        .push((name.clone(), size, Some(vals.clone()), Some(ir_idx)));
                    this.emit(Instr::ArrayDecl {
                        name: name.clone(),
                        size,
                        initial_values: Some(vals),
                    });
                    Ok(())
                })
            }
        }
    }

    pub fn lower_assign_lvalue(&mut self, target: &LValue, rhs: &Expr) -> Result<(), CompileError> {
        match target {
            LValue::Var(name) => self.lower_assign(name, rhs),
            LValue::Deref(ptr_expr) => {
                let addr = self.eval_as_pointer_expr(ptr_expr)?;
                let v = self.eval_as_value(rhs)?;
                self.emit(Instr::Store { addr, src: v });
                Ok(())
            }
            LValue::Index { base, index } => {
                let index_v = self.eval_as_value(index)?;
                let src_v = self.eval_as_value(rhs)?;
                self.emit(Instr::ArrayStore {
                    base: base.clone(),
                    index: index_v,
                    src: src_v,
                });
                Ok(())
            }
        }
    }

    pub fn lower_assign(&mut self, name: &str, rhs: &Expr) -> Result<(), CompileError> {
        // Prefer direct binary assignment for arithmetic to avoid unnecessary temps
        if let Expr::Binary {
            left, op, right, ..
        } = rhs
        {
            if matches!(
                op,
                AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul | AstBinOp::Div | AstBinOp::Mod
            ) {
                let l = self.eval_as_value(left)?;
                let r = self.eval_as_value(right)?;
                let op = map_arith(*op);
                self.emit(Instr::Assign {
                    dst: self.get_var(name.to_string()),
                    src: Rhs::Binary {
                        op,
                        left: l,
                        right: r,
                    },
                });
                return Ok(());
            }
        }

        let v = self.eval_as_value(rhs)?;
        self.emit(Instr::Assign {
            dst: self.get_var(name.to_string()),
            src: Rhs::Value(v),
        });
        Ok(())
    }

    pub fn lower_if(
        &mut self,
        ast_id: AstNodeId,
        cond: &Expr,
        then_blk: &[Stmt],
        else_blk: Option<&Vec<Stmt>>,
    ) -> Result<(), CompileError> {
        let endif = self.new_label();
        let else_label = else_blk.map(|_| self.new_label());
        let false_target = else_label.as_deref().unwrap_or(&endif);

        self.with_ast_context(ast_id, Some(ControlFlowComponent::Condition), |this| {
            this.lower_condition_branch_false(cond, false_target)
        })?;

        self.with_parent_component(ControlFlowComponent::ThenBranch, |this| {
            this.emit_block(then_blk)
        })?;

        if else_label.is_some() {
            self.with_ast_context(
                ast_id,
                Some(ControlFlowComponent::ControlFlowGlue),
                |this| -> Result<(), CompileError> {
                    this.emit(Instr::Goto(endif.clone()));
                    Ok(())
                },
            )?;
        }

        if let Some(lbl) = else_label {
            self.with_ast_context(
                ast_id,
                Some(ControlFlowComponent::ControlFlowGlue),
                |this| -> Result<(), CompileError> {
                    this.emit(Instr::Label(lbl));
                    Ok(())
                },
            )?;
            if let Some(blk) = else_blk {
                self.with_parent_component(ControlFlowComponent::ElseBranch, |this| {
                    this.emit_block(blk)
                })?;
            }
        }

        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| -> Result<(), CompileError> {
                this.emit(Instr::Label(endif));
                Ok(())
            },
        )?;

        Ok(())
    }

    pub fn lower_while(
        &mut self,
        ast_id: AstNodeId,
        cond: &Expr,
        body: &[Stmt],
    ) -> Result<(), CompileError> {
        let start = self.new_label();
        let end = self.new_label();

        // Map the loop entry label to the while AST node
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| -> Result<(), CompileError> {
                this.emit(Instr::Label(start.clone()));
                Ok(())
            },
        )?;

        self.with_ast_context(ast_id, Some(ControlFlowComponent::Condition), |this| {
            this.lower_condition_branch_false(cond, &end)
        })?;

        self.with_parent_component(ControlFlowComponent::LoopBody, |this| this.emit_block(body))?;

        // Back-edge and exit label are also attributed to the while node
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| -> Result<(), CompileError> {
                this.emit(Instr::Goto(start.clone()));
                Ok(())
            },
        )?;
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| -> Result<(), CompileError> {
                this.emit(Instr::Label(end.clone()));
                Ok(())
            },
        )?;

        Ok(())
    }

    pub fn lower_for(
        &mut self,
        ast_id: AstNodeId,
        var: &str,
        from: &Expr,
        to: &Expr,
        body: &[Stmt],
    ) -> Result<(), CompileError> {
        let start = self.new_label();
        let end = self.new_label();

        // Loop variable follows the global-by-default rule
        let var_v = self.get_var(var.to_string());

        // Initialize loop variable
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| -> Result<(), CompileError> {
                let from_v = this.eval_as_value(from)?;
                this.emit(Instr::Assign {
                    dst: var_v.clone(),
                    src: Rhs::Value(from_v),
                });
                Ok(())
            },
        )?;

        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| -> Result<(), CompileError> {
                this.emit(Instr::Label(start.clone()));
                Ok(())
            },
        )?;

        // Condition: var > to → exit (equivalent to !(var <= to))
        let to_v = self.eval_as_value(to)?;
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::Condition),
            |this| -> Result<(), CompileError> {
                this.emit(Instr::IfCmpGoto {
                    left: Value::Var(var_v.clone()),
                    op: RelOp::Gt,
                    right: to_v,
                    target: end.clone(),
                });
                Ok(())
            },
        )?;

        self.with_parent_component(ControlFlowComponent::LoopBody, |this| this.emit_block(body))?;

        // Increment
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| -> Result<(), CompileError> {
                this.emit(Instr::Assign {
                    dst: var_v.clone(),
                    src: Rhs::Binary {
                        op: ArithOp::Add,
                        left: Value::Var(var_v.clone()),
                        right: Value::Imm(1),
                    },
                });
                Ok(())
            },
        )?;

        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| -> Result<(), CompileError> {
                this.emit(Instr::Goto(start.clone()));
                Ok(())
            },
        )?;
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| -> Result<(), CompileError> {
                this.emit(Instr::Label(end.clone()));
                Ok(())
            },
        )?;

        Ok(())
    }

    pub fn emit_block(&mut self, stmts: &[Stmt]) -> Result<(), CompileError> {
        for s in stmts {
            self.lower_stmt(s)?;
        }
        Ok(())
    }

    pub fn lower_function(
        &mut self,
        ast_id: AstNodeId,
        name: &str,
        params: &[String],
        body: &[Stmt],
    ) -> Result<(), CompileError> {
        self.with_ast_context(ast_id, None, |this| {
            this.emit(Instr::FuncStart {
                name: name.to_string(),
                params: params.to_vec(),
            });

            let prev_ctx = this.fn_ctx.take();
            this.fn_ctx = Some(FunctionCtx { had_return: false });

            // Enter function scope
            this.symbols.enter_function();

            // Validate: Check for duplicate parameters
            for param in params {
                if let Err(_existing) = this.symbols.declare(
                    param.clone(),
                    SymbolInfo {
                        kind: SymbolKind::Variable,
                        ast_id,
                        param_count: None,
                    },
                ) {
                    return Err(this.make_error(
                        SemanticErrorKind::ParameterRedefinition,
                        ast_id,
                        format!("Parameter '{}' defined multiple times", param),
                    ));
                }
            }

            this.emit_block(body)?;

            // Exit function scope
            this.symbols.exit_function();
            this.fn_ctx = prev_ctx;

            this.emit(Instr::FuncEnd {
                name: name.to_string(),
            });
            Ok(())
        })
    }
}
