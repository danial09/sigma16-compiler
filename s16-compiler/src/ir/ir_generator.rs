use crate::ast::ast_ir::{self, BinOp as AstBinOp, Expr, LValue, Program, Stmt};
use crate::ir::*;
use std::collections::HashMap;

pub fn lower(program: &Program) -> ProgramIR {
    let mut g = Gen::new();
    g.lower_program(program);
    g.finish()
}

struct Gen {
    out: ProgramIR,
    temp_count: usize,
    label_count: usize,
    current_ast_node: Option<(AstNodeId, Option<ControlFlowComponent>)>,
    /// Track the parent control flow component (for nested statements)
    parent_component: Option<ControlFlowComponent>,
    /// Current function context (if inside a function)
    fn_ctx: Option<FunctionCtx>,
}

#[derive(Debug, Clone)]
struct FunctionCtx {
    _name: String,
    /// Map parameter _name -> 1..=8 argument index (register R1..R8)
    _param_index: HashMap<String, u8>,
    /// Whether a return was emitted in this function body
    had_return: bool,
}

impl Gen {
    fn new() -> Self {
        Self {
            out: ProgramIR::new(),
            temp_count: 0,
            label_count: 0,
            current_ast_node: None,
            parent_component: None,
            fn_ctx: None,
        }
    }

    fn finish(self) -> ProgramIR {
        self.out
    }

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
        let instr_index = self.out.instrs.len();
        self.out.instrs.push(i);

        // Record the mapping if we're tracking an AST node
        if let Some((ast_id, component)) = self.current_ast_node {
            let description = self.generate_description(&self.out.instrs[instr_index]);
            let mapping = AstMapping::new(ast_id, description);
            // Use the explicit component if set, otherwise use parent component
            let final_component = component.or(self.parent_component);
            let mapping = if let Some(comp) = final_component {
                mapping.with_component(comp)
            } else {
                mapping
            };
            self.out.source_map.add_mapping(instr_index, mapping);
        }
    }

    fn generate_description(&self, instr: &Instr) -> String {
        match instr {
            Instr::Assign { dst, .. } => format!("Assign to {}", dst),
            Instr::IfCmpGoto { .. } => "Conditional branch".to_string(),
            Instr::IfFalseGoto { .. } => "False branch".to_string(),
            Instr::Goto(_) => "Unconditional jump".to_string(),
            Instr::Label(l) => format!("Label: {}", l),
            Instr::Load { dst, .. } => format!("Load into {}", dst),
            Instr::Store { .. } => "Store through pointer".to_string(),
            Instr::FuncStart { name, .. } => format!("Function start: {}", name),
            Instr::FuncEnd { name } => format!("Function end: {}", name),
            Instr::Call { func, .. } => format!("Call {}", func),
            Instr::Return { .. } => "Return".to_string(),
        }
    }

    fn with_ast_context<F, R>(
        &mut self,
        ast_id: AstNodeId,
        component: Option<ControlFlowComponent>,
        f: F,
    ) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let prev = self.current_ast_node;
        self.current_ast_node = Some((ast_id, component));
        let result = f(self);
        self.current_ast_node = prev;
        result
    }

    fn with_parent_component<F, R>(&mut self, component: ControlFlowComponent, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let prev = self.parent_component;
        self.parent_component = Some(component);
        let result = f(self);
        self.parent_component = prev;
        result
    }

    fn lower_program(&mut self, p: &Program) {
        for s in &p.statements {
            self.lower_stmt(s);
        }
    }

    fn lower_stmt(&mut self, s: &Stmt) {
        match s {
            Stmt::Assign { id, target, value } => {
                self.with_ast_context(*id, None, |this| this.lower_assign_lvalue(target, value));
            }
            Stmt::If {
                id,
                condition,
                then_branch,
                else_branch,
            } => {
                self.lower_if_tracked(*id, condition, then_branch, else_branch.as_ref());
            }
            Stmt::While {
                id,
                condition,
                body,
            } => {
                self.lower_while_tracked(*id, condition, body);
            }
            Stmt::For {
                id,
                var,
                from,
                to,
                body,
            } => {
                self.lower_for_tracked(*id, var, from, to, body);
            }
            Stmt::ArrayDecl { name, size, .. } => {
                self.out.arrays.push((name.clone(), *size));
            }
            Stmt::Function {
                id,
                name,
                params,
                body,
            } => {
                self.lower_function(*id, name, params, body);
            }
            Stmt::Return { id, value } => {
                let v = self.eval_as_value(value);
                self.with_ast_context(*id, None, |this| {
                    this.emit(Instr::Return { value: Some(v) })
                });
            }
            Stmt::ExprStmt { id, expr } => {
                // Evaluate and discard result, attribute to the statement id as context
                self.with_ast_context(*id, None, |this| {
                    let _ = this.eval_as_value(expr);
                });
            }
        }
    }

    fn lower_assign(&mut self, name: &str, rhs: &Expr) {
        // Fast-path: if RHS is a simple arithmetic expression with simple operands,
        // emit a single binary assign without introducing a temporary.
        // This preserves expected pretty IR like: `z = x + y`.
        let try_simple_binary = || -> Option<Rhs> {
            match rhs {
                Expr::Binary {
                    left, op, right, ..
                } => {
                    // Only handle primitive arithmetic ops
                    match op {
                        AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul | AstBinOp::Div => {
                            // Map a very small subset of expressions to immediate Values
                            fn as_simple_value(e: &Expr) -> Option<Value> {
                                match e {
                                    Expr::Number(_, n) => Some(Value::Imm(*n)),
                                    Expr::Variable(_, name) => Some(Value::Var(name.clone())),
                                    Expr::AddrOf(_, name) => Some(Value::AddrOf(name.clone())),
                                    _ => None, // anything else would require extra instructions
                                }
                            }

                            let l = as_simple_value(left)?;
                            let r = as_simple_value(right)?;
                            let op = map_arith(*op);
                            Some(Rhs::Binary {
                                op,
                                left: l,
                                right: r,
                            })
                        }
                        _ => None,
                    }
                }
                _ => None,
            }
        }();

        if let Some(rhs_bin) = try_simple_binary {
            self.emit(Instr::Assign {
                dst: name.to_string(),
                src: rhs_bin,
            });
        } else {
            // Secondary path: if RHS is an arithmetic binary expression, evaluate its
            // operands (materializing temps as needed), but emit the final assignment
            // directly as a binary into the destination to avoid an extra temporary.
            if let Expr::Binary {
                left, op, right, ..
            } = rhs
            {
                if matches!(
                    op,
                    AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul | AstBinOp::Div
                ) {
                    let l = self.eval_as_value(left);
                    let r = self.eval_as_value(right);
                    let op = map_arith(*op);
                    self.emit(Instr::Assign {
                        dst: name.to_string(),
                        src: Rhs::Binary {
                            op,
                            left: l,
                            right: r,
                        },
                    });
                    return;
                }
            }

            // Fallback to general evaluation which may materialize temps as needed
            let v = self.eval_as_value(rhs);
            self.emit(Instr::Assign {
                dst: name.to_string(),
                src: Rhs::Value(v),
            });
        }
    }

    fn lower_assign_lvalue(&mut self, target: &LValue, rhs: &Expr) {
        match target {
            LValue::Var(name) => self.lower_assign(name, rhs),
            LValue::Deref(ptr_expr) => {
                let addr = self.eval_as_pointer_expr(ptr_expr);
                let v = self.eval_as_value(rhs);
                self.emit(Instr::Store { addr, src: v });
            }
            LValue::Index { base, index } => {
                // Treat as *(base + index) = rhs; address-of base plus index
                let base_addr = Value::AddrOf(base.clone());
                let idx_v = self.eval_as_value(index);
                let tmp = self.new_temp();
                self.emit(Instr::Assign {
                    dst: tmp.clone(),
                    src: Rhs::Binary {
                        op: ArithOp::Add,
                        left: base_addr,
                        right: idx_v,
                    },
                });
                let rhs_v = self.eval_as_value(rhs);
                self.emit(Instr::Store {
                    addr: Value::Var(tmp),
                    src: rhs_v,
                });
            }
        }
    }

    fn lower_if_tracked(
        &mut self,
        ast_id: AstNodeId,
        cond: &Expr,
        then_blk: &[Stmt],
        else_blk: Option<&Vec<Stmt>>,
    ) {
        // Heuristic branching strategy can be improved; keep simple for now
        self.lower_if_simple_impl(Some(ast_id), cond, then_blk, else_blk);
    }

    fn lower_if_simple_impl(
        &mut self,
        ast_id: Option<AstNodeId>,
        cond: &Expr,
        then_blk: &[Stmt],
        else_blk: Option<&Vec<Stmt>>,
    ) {
        let endif = self.new_label();
        let else_label = if else_blk.is_some() {
            Some(self.new_label())
        } else {
            None
        };

        // branch if false to else/endif
        let target = else_label.as_deref().unwrap_or(&endif);
        self.with_optional_context(ast_id, Some(ControlFlowComponent::Condition), |this| {
            this.emit_simple_branch_on_false(cond, target);
        });

        // then
        self.with_optional_parent(Some(ControlFlowComponent::ThenBranch), |this| {
            this.emit_block(then_blk)
        });
        if else_label.is_some() {
            // glue to jump over else
            self.with_optional_context(
                ast_id,
                Some(ControlFlowComponent::ControlFlowGlue),
                |this| {
                    this.emit(Instr::Goto(endif.clone()));
                },
            );
        }

        // else
        if let Some(lbl) = &else_label {
            self.with_optional_context(
                ast_id,
                Some(ControlFlowComponent::ControlFlowGlue),
                |this| {
                    this.emit(Instr::Label(lbl.clone()));
                },
            );
            if let Some(else_blk) = else_blk {
                self.with_optional_parent(Some(ControlFlowComponent::ElseBranch), |this| {
                    this.emit_block(else_blk)
                });
            }
        }

        self.with_optional_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| {
                this.emit(Instr::Label(endif));
            },
        );
    }

    fn lower_while_tracked(&mut self, ast_id: AstNodeId, cond: &Expr, body: &[Stmt]) {
        self.lower_while_impl(Some(ast_id), cond, body);
    }

    fn lower_while_impl(&mut self, ast_id: Option<AstNodeId>, cond: &Expr, body: &[Stmt]) {
        let start = self.new_label();
        let end = self.new_label();
        self.with_optional_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| {
                this.emit(Instr::Label(start.clone()));
            },
        );
        self.with_optional_context(ast_id, Some(ControlFlowComponent::Condition), |this| {
            this.emit_simple_branch_on_false(cond, &end);
        });
        self.with_optional_parent(Some(ControlFlowComponent::LoopBody), |this| {
            this.emit_block(body);
        });
        self.with_optional_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| {
                this.emit(Instr::Goto(start));
                this.emit(Instr::Label(end));
            },
        );
    }

    fn lower_for_tracked(
        &mut self,
        ast_id: AstNodeId,
        var: &str,
        from: &Expr,
        to: &Expr,
        body: &[Stmt],
    ) {
        let start = self.new_label();
        let end = self.new_label();

        // init var = from
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| {
                let from_v = this.eval_as_value(from);
                this.emit(Instr::Assign {
                    dst: var.to_string(),
                    src: Rhs::Value(from_v),
                });
            },
        );

        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| {
                this.emit(Instr::Label(start.clone()));
            },
        );
        // if var > to goto end
        self.with_ast_context(ast_id, Some(ControlFlowComponent::Condition), |this| {
            let to_v = this.eval_as_value(to);
            this.emit(Instr::IfCmpGoto {
                left: Value::Var(var.to_string()),
                op: RelOp::Gt,
                right: to_v,
                target: end.clone(),
            });
        });

        // body
        self.with_ast_context(ast_id, Some(ControlFlowComponent::LoopBody), |this| {
            this.emit_block(body)
        });

        // var = var + 1
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| {
                this.emit(Instr::Assign {
                    dst: var.to_string(),
                    src: Rhs::Binary {
                        op: ArithOp::Add,
                        left: Value::Var(var.to_string()),
                        right: Value::Imm(1),
                    },
                });
                this.emit(Instr::Goto(start));
                this.emit(Instr::Label(end));
            },
        );
    }

    fn emit_block(&mut self, stmts: &[Stmt]) {
        for s in stmts {
            self.lower_stmt(s);
        }
    }

    fn lower_function(&mut self, ast_id: AstNodeId, name: &str, params: &[String], body: &[Stmt]) {
        // Set function context
        let mut map = HashMap::new();
        for (i, p) in params.iter().take(8).enumerate() {
            map.insert(p.clone(), (i as u8) + 1);
        }
        self.fn_ctx = Some(FunctionCtx {
            _name: name.to_string(),
            _param_index: map,
            had_return: false,
        });

        // prologue
        self.with_ast_context(
            ast_id,
            Some(ControlFlowComponent::ControlFlowGlue),
            |this| {
                this.emit(Instr::FuncStart {
                    name: name.to_string(),
                    params: params.to_vec(),
                });
            },
        );

        // body
        self.emit_block(body);

        // epilogue if no return
        if let Some(ctx) = &self.fn_ctx {
            if !ctx.had_return {
                self.with_ast_context(
                    ast_id,
                    Some(ControlFlowComponent::ControlFlowGlue),
                    |this| {
                        this.emit(Instr::FuncEnd {
                            name: name.to_string(),
                        });
                    },
                );
            }
        }
        self.fn_ctx = None;
    }

    fn with_optional_context<F, R>(
        &mut self,
        ast_id: Option<AstNodeId>,
        component: Option<ControlFlowComponent>,
        f: F,
    ) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        if let Some(id) = ast_id {
            self.with_ast_context(id, component, f)
        } else {
            f(self)
        }
    }

    fn with_optional_parent<F, R>(&mut self, component: Option<ControlFlowComponent>, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        if let Some(comp) = component {
            self.with_parent_component(comp, f)
        } else {
            f(self)
        }
    }

    fn emit_simple_branch_on_false(&mut self, cond: &Expr, target: &str) {
        if let Expr::Binary {
            left, op, right, ..
        } = cond
        {
            if is_rel(*op) {
                // let _rel = map_rel(*op);
                let l = self.eval_as_value(left);
                let r = self.eval_as_value(right);
                // if NOT (l rel r) goto target
                let inv = invert_rel(*op);
                self.emit(Instr::IfCmpGoto {
                    left: l,
                    op: inv,
                    right: r,
                    target: target.to_string(),
                });
                return;
            }
        }
        // generic truthiness: if_false value goto
        let v = self.eval_as_value(cond);
        self.emit(Instr::IfFalseGoto {
            value: v,
            target: target.to_string(),
        });
    }

    fn materialize_not(&mut self, operand: &Expr) -> Value {
        // Attribute this emission to the 'not' expression itself (set by caller)
        let v = self.eval_as_value(operand);
        let tmp = self.new_temp();
        // tmp = 0 == v  (stand-in)
        self.emit(Instr::Assign {
            dst: tmp.clone(),
            src: Rhs::Value(v),
        });
        Value::Var(tmp)
    }

    fn materialize_relational(&mut self, left: &Expr, right: &Expr, op: AstBinOp) -> Value {
        // Attribute to the relational expression (set by caller)
        let l = self.eval_as_value(left);
        let r = self.eval_as_value(right);
        let tmp = self.new_temp();
        self.emit(Instr::Assign {
            dst: tmp.clone(),
            src: Rhs::Value(l),
        });
        let _ = (r, op); // placeholder
        Value::Var(tmp)
    }

    fn materialize_and(&mut self, left: &Expr, right: &Expr) -> Value {
        let l = self.eval_as_value(left);
        let r = self.eval_as_value(right);
        let tmp = self.new_temp();
        self.emit(Instr::Assign {
            dst: tmp.clone(),
            src: Rhs::Binary {
                op: ArithOp::Mul,
                left: l,
                right: r,
            },
        });
        Value::Var(tmp)
    }

    fn materialize_or(&mut self, left: &Expr, right: &Expr) -> Value {
        let l = self.eval_as_value(left);
        let r = self.eval_as_value(right);
        let tmp = self.new_temp();
        self.emit(Instr::Assign {
            dst: tmp.clone(),
            src: Rhs::Binary {
                op: ArithOp::Add,
                left: l,
                right: r,
            },
        });
        Value::Var(tmp)
    }

    fn eval_as_value(&mut self, e: &Expr) -> Value {
        // Attribute all instructions produced while evaluating this expression to the expression's AST id
        self.with_ast_context(e.id(), None, |this| match e {
            Expr::Number(_, n) => Value::Imm(*n),
            Expr::Variable(_, name) => Value::Var(name.clone()),
            Expr::Unary {
                op: ast_ir::UnOp::Not,
                operand,
                ..
            } => this.materialize_not(operand),
            Expr::Binary {
                left, op, right, ..
            } => match op {
                AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul | AstBinOp::Div => {
                    let (l, r, op) = this.lower_arith_expr(left, right, *op);
                    let tmp = this.new_temp();
                    this.emit(Instr::Assign {
                        dst: tmp.clone(),
                        src: Rhs::Binary {
                            op,
                            left: l,
                            right: r,
                        },
                    });
                    Value::Var(tmp)
                }
                AstBinOp::Eq | AstBinOp::Neq | AstBinOp::Lt | AstBinOp::Gt => {
                    this.materialize_relational(left, right, *op)
                }
                AstBinOp::And => this.materialize_and(left, right),
                AstBinOp::Or => this.materialize_or(left, right),
            },
            Expr::AddrOf(_, name) => Value::AddrOf(name.clone()),
            Expr::Deref(_, ptr) => {
                let addr = this.eval_as_pointer_expr(ptr);
                let tmp = this.new_temp();
                this.emit(Instr::Load {
                    dst: tmp.clone(),
                    addr,
                });
                Value::Var(tmp)
            }
            Expr::Index { base, index, .. } => {
                let base_addr = Value::AddrOf(base.clone());
                let idx_v = this.eval_as_value(index);
                let tmp_addr = this.new_temp();
                this.emit(Instr::Assign {
                    dst: tmp_addr.clone(),
                    src: Rhs::Binary {
                        op: ArithOp::Add,
                        left: base_addr,
                        right: idx_v,
                    },
                });
                let tmp = this.new_temp();
                this.emit(Instr::Load {
                    dst: tmp.clone(),
                    addr: Value::Var(tmp_addr),
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

    fn eval_as_pointer_expr(&mut self, e: &Expr) -> Value {
        // Attribute to the inner expression id as well
        match e {
            Expr::Variable(_, name) => Value::AddrOf(name.clone()),
            Expr::AddrOf(_, name) => Value::AddrOf(name.clone()),
            _ => self.eval_as_value(e),
        }
    }

    fn lower_arith_expr(&mut self, l: &Expr, r: &Expr, op: AstBinOp) -> (Value, Value, ArithOp) {
        let lv = self.eval_as_value(l);
        let rv = self.eval_as_value(r);
        let op = map_arith(op);
        (lv, rv, op)
    }

    // fn eval_atom_or_complex(&mut self, e: &Expr) -> Value { self.eval_as_value(e) }
}

fn is_rel(op: AstBinOp) -> bool {
    matches!(
        op,
        AstBinOp::Eq | AstBinOp::Neq | AstBinOp::Lt | AstBinOp::Gt
    )
}
// fn is_arith(op: AstBinOp) -> bool { matches!(op, AstBinOp::Add | AstBinOp::Sub | AstBinOp::Mul | AstBinOp::Div) }
fn map_arith(op: AstBinOp) -> ArithOp {
    match op {
        AstBinOp::Add => ArithOp::Add,
        AstBinOp::Sub => ArithOp::Sub,
        AstBinOp::Mul => ArithOp::Mul,
        AstBinOp::Div => ArithOp::Div,
        _ => ArithOp::Add,
    }
}
// fn map_rel(op: AstBinOp) -> RelOp { match op { AstBinOp::Eq => RelOp::Eq, AstBinOp::Neq => RelOp::Neq, AstBinOp::Lt => RelOp::Lt, AstBinOp::Gt => RelOp::Gt, _ => RelOp::Eq } }
fn invert_rel(op: AstBinOp) -> RelOp {
    match op {
        AstBinOp::Eq => RelOp::Neq,
        AstBinOp::Neq => RelOp::Eq,
        AstBinOp::Lt => RelOp::Ge,
        AstBinOp::Gt => RelOp::Le,
        _ => RelOp::Eq,
    }
}
