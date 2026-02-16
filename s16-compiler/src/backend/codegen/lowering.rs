//! IR instruction → Sigma16 assembly lowering.
//!
//! Implements `Codegen::emit_instr` which translates a single IR instruction
//! into one or more typed machine instructions.

use super::Codegen;
use crate::backend::abi::Register;
use crate::backend::instruction::{Cond, Disp, S16Instr};
use crate::ir::{ArithOp, Instr, RelOp, Rhs, Value, Var, VarKind};

/// Short description of an IR value for assembly comments.
fn describe_val(v: &Value) -> String {
    match v {
        Value::Imm(n) => format!("{n}"),
        Value::Var(var) => var.name.clone(),
        Value::AddrOf(name) => format!("&{name}"),
    }
}

/// Operator symbol for arithmetic operations.
fn op_sym(op: &ArithOp) -> &'static str {
    match op {
        ArithOp::Add => "+",
        ArithOp::Sub => "-",
        ArithOp::Mul => "*",
        ArithOp::Div => "/",
        ArithOp::Mod => "%",
    }
}

/// Operator symbol for relational operations.
fn rel_sym(op: &RelOp) -> &'static str {
    match op {
        RelOp::Eq => "==",
        RelOp::Neq => "!=",
        RelOp::Lt => "<",
        RelOp::Gt => ">",
        RelOp::Le => "<=",
        RelOp::Ge => ">=",
    }
}

impl Codegen {
    /// Lower a single IR instruction into typed machine instructions.
    pub(crate) fn emit_instr(&mut self, instr: &Instr) {
        match instr {
            Instr::FuncStart { name, params } => {
                self.start_function(name.clone());
                for (i, p) in params.iter().enumerate().take(8) {
                    let var = Var::local(p.clone());
                    let ri = Register::PARAM_REGS[i];
                    self.reg.bind_var_to_reg(var, ri);
                }
            }
            Instr::FuncEnd { .. } => {
                let mut out = Vec::new();
                self.reg.flush_globals(&mut out);
                self.drain_regalloc(out);
                self.current_func_end_ir = self.current_ir;
                self.flush_function();
            }
            Instr::Label(lbl) => {
                self.flush_at_control_flow();
                self.push_label(lbl.clone());
            }
            Instr::Goto(t) => {
                self.flush_at_control_flow();
                self.push_asm(S16Instr::jump_label(t.as_str()));
            }
            Instr::IfCmpGoto {
                left,
                op,
                right,
                target,
            } => {
                self.emit_conditional_branch(left, op, right, target);
            }
            Instr::Assign { dst, src } => match src {
                Rhs::Value(v) => self.emit_assign_value(dst, v),
                Rhs::Binary { op, left, right } => self.emit_assign_binary(dst, op, left, right),
            },
            Instr::Load { dst, addr } => {
                let (ra, free_a) = self.ensure_in_reg(addr);
                let rd = self.prepare_def_reg(dst);
                self.push_commented(
                    S16Instr::load_disp(rd, 0, ra),
                    format!("{} = *{}", dst.name, describe_val(addr)),
                );
                self.reg.bind_var_to_reg(dst.clone(), rd);
                self.reg.mark_dirty(dst);
                if free_a && ra != rd {
                    self.reg.free_reg(ra);
                }
            }
            Instr::Store { addr, src } => {
                self.emit_store(addr, src);
            }
            Instr::ArrayLoad { dst, base, index } => {
                self.emit_array_load(dst, base, index);
            }
            Instr::ArrayStore { base, index, src } => {
                self.emit_array_store(base, index, src);
            }
            Instr::Call { func, args, ret } => {
                self.emit_call(func, args, ret);
            }
            Instr::Return { value } => {
                self.emit_return(value);
            }
            Instr::ArrayDecl { .. } => {
                // Handled by finish_codegen.
            }
        }
    }

    // ── Helper: flush at control-flow boundary ──────────────────────────

    fn flush_at_control_flow(&mut self) {
        if self.advanced_mode {
            let mut out = Vec::new();
            self.reg.flush_dirty(&mut out);
            self.drain_regalloc(out);
            self.reg.clear_bindings();
        } else {
            let mut out = Vec::new();
            self.reg.flush_all(&mut out);
            self.drain_regalloc(out);
        }
    }

    // ── Conditional branch ──────────────────────────────────────────────

    fn emit_conditional_branch(
        &mut self,
        left: &Value,
        op: &RelOp,
        right: &Value,
        target: &str,
    ) {
        let (rl, free_l) = self.ensure_in_reg(left);
        let (rr, free_r) = self.ensure_in_reg(right);
        self.push_commented(
            S16Instr::Cmp { a: rl, b: rr },
            format!(
                "{} {} {}?",
                describe_val(left),
                rel_sym(op),
                describe_val(right)
            ),
        );
        if free_l {
            self.reg.free_reg(rl);
        }
        if free_r {
            self.reg.free_reg(rr);
        }

        if self.advanced_mode {
            let mut out = Vec::new();
            self.reg.flush_dirty(&mut out);
            self.drain_regalloc(out);
        } else {
            let mut out = Vec::new();
            self.reg.flush_all(&mut out);
            self.drain_regalloc(out);
        }

        let cond = match op {
            RelOp::Eq => Cond::Eq,
            RelOp::Neq => Cond::Ne,
            RelOp::Lt => Cond::Lt,
            RelOp::Gt => Cond::Gt,
            RelOp::Le => Cond::Le,
            RelOp::Ge => Cond::Ge,
        };
        self.push_asm(S16Instr::JumpCond {
            cond,
            disp: Disp::Label(target.to_string()),
            idx: Register::ZERO_REG,
        });
    }

    // ── Simple assignment (Rhs::Value) ──────────────────────────────────

    fn emit_assign_value(&mut self, dst: &Var, v: &Value) {
        let assign_comment = format!("{} = {}", dst.name, describe_val(v));
        let is_top_level = self.func_buf.is_none();

        // Top-level global with immediate: fold into data section.
        if is_top_level && dst.kind == VarKind::Global {
            if let Value::Imm(imm) = v {
                if !self.user_vars_set.contains(&dst.name) {
                    self.note_user_var_init(&dst.name, *imm);
                    return;
                }
            }
        }

        let (rs, free_s) = self.ensure_in_reg(v);

        if dst.kind == VarKind::Global && !self.advanced_mode {
            // Basic mode: store globals directly to memory.
            self.note_user_var(&dst.name);
            self.push_commented(S16Instr::store_label(rs, &dst.name), &assign_comment);

            if let Value::Var(src_var) = v {
                if src_var.is_reg_allocated() {
                    if let Some(src_reg) = self.get_var_reg(src_var) {
                        if src_reg == rs {
                            self.reg.free_reg(rs);
                        }
                    }
                }
            }
            if free_s {
                self.reg.free_reg(rs);
            }
        } else {
            // Advanced mode (or local/temp): register-allocate.
            if dst.kind == VarKind::Global {
                self.note_user_var(&dst.name);
            }

            let fixed_dst = self.reg.get_fixed_reg(dst);
            let can_rebind = {
                let base = if free_s {
                    true
                } else if let Value::Var(src_var) = v {
                    src_var != dst && self.reg.is_var_dead(src_var)
                } else {
                    false
                };
                base && match fixed_dst {
                    Some(fr) => rs == fr,
                    None => true,
                }
            };

            if can_rebind {
                self.reg.bind_var_to_reg(dst.clone(), rs);
                self.reg.mark_dirty(dst);
                self.push_commented(S16Instr::mov(rs, rs), &assign_comment);
            } else {
                let rd = self.prepare_def_reg(dst);
                if rd != rs {
                    self.push_commented(S16Instr::mov(rd, rs), &assign_comment);
                }
                self.reg.bind_var_to_reg(dst.clone(), rd);
                self.reg.mark_dirty(dst);
                if free_s && rs != rd {
                    self.reg.free_reg(rs);
                }
            }
        }
    }

    // ── Binary assignment (Rhs::Binary) ─────────────────────────────────

    fn emit_assign_binary(
        &mut self,
        dst: &Var,
        op: &ArithOp,
        left: &Value,
        right: &Value,
    ) {
        let is_mod = *op == ArithOp::Mod;
        let bin_comment = format!(
            "{} = {} {} {}",
            dst.name,
            describe_val(left),
            op_sym(op),
            describe_val(right)
        );

        if dst.kind == VarKind::Global {
            self.note_user_var(&dst.name);
        }

        let dst_is_left = matches!(left, Value::Var(v) if v == dst);
        let dst_is_right = matches!(right, Value::Var(v) if v == dst);

        let (rl, free_l, rr, free_r, rd) = if is_mod {
            let (rl, free_l) = self.ensure_in_reg(left);
            let (rr, free_r) = self.ensure_in_reg(right);
            let rd = self.prepare_def_reg(dst);
            (rl, free_l, rr, free_r, rd)
        } else if dst_is_left {
            let (rl, free_l) = self.ensure_in_reg(left);
            let (rr, free_r) = self.ensure_in_reg(right);
            (rl, free_l, rr, free_r, rl)
        } else if dst_is_right && (*op == ArithOp::Add || *op == ArithOp::Mul) {
            let (rr, free_r) = self.ensure_in_reg(right);
            let (rl, free_l) = self.ensure_in_reg(left);
            (rl, free_l, rr, free_r, rr)
        } else {
            let (rl, free_l) = self.ensure_in_reg(left);
            let (rr, free_r) = self.ensure_in_reg(right);
            let rd = if free_l {
                rl
            } else if free_r && (*op == ArithOp::Add || *op == ArithOp::Mul) {
                rr
            } else {
                self.prepare_def_reg(dst)
            };
            (rl, free_l, rr, free_r, rd)
        };

        if is_mod {
            self.push_commented(
                S16Instr::Div {
                    d: Register::ZERO_REG,
                    a: rl,
                    b: rr,
                },
                format!(
                    "{} {} {}",
                    describe_val(left),
                    op_sym(op),
                    describe_val(right)
                ),
            );
            self.push_commented(
                S16Instr::Add {
                    d: rd,
                    a: Register::ZERO_REG,
                    b: Register::R15,
                },
                format!("{} = remainder", dst.name),
            );
        } else {
            let instr = match op {
                ArithOp::Add => S16Instr::Add {
                    d: rd,
                    a: rl,
                    b: rr,
                },
                ArithOp::Sub => S16Instr::Sub {
                    d: rd,
                    a: rl,
                    b: rr,
                },
                ArithOp::Mul => S16Instr::Mul {
                    d: rd,
                    a: rl,
                    b: rr,
                },
                ArithOp::Div => S16Instr::Div {
                    d: rd,
                    a: rl,
                    b: rr,
                },
                ArithOp::Mod => unreachable!(),
            };
            self.push_commented(instr, &bin_comment);
        }

        self.reg.bind_var_to_reg(dst.clone(), rd);
        self.reg.mark_dirty(dst);

        if free_l && rl != rd {
            self.reg.free_reg(rl);
        }
        if free_r && rr != rd {
            self.reg.free_reg(rr);
        }
    }

    // ── Store ───────────────────────────────────────────────────────────

    fn emit_store(&mut self, addr: &Value, src: &Value) {
        let is_indirect = !matches!(addr, Value::AddrOf(_));
        if is_indirect && self.advanced_mode {
            let mut out = Vec::new();
            self.reg.flush_globals(&mut out);
            self.drain_regalloc(out);
        }

        let (ra, free_a) = self.ensure_in_reg(addr);
        let (rs, free_s) = self.ensure_in_reg(src);
        self.push_commented(
            S16Instr::store_disp(rs, 0, ra),
            format!("*{} = {}", describe_val(addr), describe_val(src)),
        );

        if let Value::AddrOf(name) = addr {
            if self.advanced_mode {
                let var = Var::global(name.clone());
                if let Some(r) = self.reg.get_var_reg(&var) {
                    self.reg.free_reg(r);
                }
            }
        }

        if free_a {
            self.reg.free_reg(ra);
        }
        if free_s {
            self.reg.free_reg(rs);
        }
    }

    // ── Array access ────────────────────────────────────────────────────

    fn emit_array_load(&mut self, dst: &Var, base: &str, index: &Value) {
        let (ri, free_i) = self.ensure_in_reg(index);
        let addr = self.allocate_temp_reg();
        self.note_user_var(base);
        self.push_commented(
            S16Instr::lea_label(addr, base),
            format!("&{}", base),
        );
        self.push_asm(S16Instr::Add {
            d: addr,
            a: addr,
            b: ri,
        });

        let rd = self.prepare_def_reg(dst);
        self.push_commented(
            S16Instr::load_disp(rd, 0, addr),
            format!("{} = {}[{}]", dst.name, base, describe_val(index)),
        );
        self.reg.bind_var_to_reg(dst.clone(), rd);
        self.reg.mark_dirty(dst);

        self.reg.free_reg(addr);
        if free_i {
            self.reg.free_reg(ri);
        }
    }

    fn emit_array_store(&mut self, base: &str, index: &Value, src: &Value) {
        let (ri, free_i) = self.ensure_in_reg(index);
        let (rs, free_s) = self.ensure_in_reg(src);
        let addr = self.allocate_temp_reg();
        self.note_user_var(base);
        self.push_commented(
            S16Instr::lea_label(addr, base),
            format!("&{}", base),
        );
        self.push_asm(S16Instr::Add {
            d: addr,
            a: addr,
            b: ri,
        });
        self.push_commented(
            S16Instr::store_disp(rs, 0, addr),
            format!("{}[{}] = {}", base, describe_val(index), describe_val(src)),
        );
        self.reg.free_reg(addr);
        if free_i {
            self.reg.free_reg(ri);
        }
        if free_s {
            self.reg.free_reg(rs);
        }
    }

    // ── Function call ───────────────────────────────────────────────────

    fn emit_call(&mut self, func: &str, args: &[Value], ret: &Option<Var>) {
        // Step 1: Evaluate arguments.
        let mut arg_info: Vec<Option<(Register, bool, bool)>> = Vec::new();
        for a in args.iter().take(8) {
            match a {
                Value::Imm(i) if *i != 0 => {
                    arg_info.push(None); // deferred
                }
                _ => {
                    let len_before = self.current_buf_len();
                    let (ra, free_a) = self.ensure_in_reg(a);
                    let emitted = self.current_buf_len() > len_before;
                    arg_info.push(Some((ra, free_a, emitted)));
                }
            }
        }

        // Step 2: Spill caller-saved registers.
        let mut spill_out = Vec::new();
        self.reg.spill_caller_saved(&mut spill_out);
        self.drain_regalloc(spill_out);

        // Step 3: Place arguments into parameter registers.
        for (i, evaluated) in arg_info.iter().enumerate() {
            let target_r = Register::PARAM_REGS[i];
            match evaluated {
                Some((ra, free_a, emitted)) => {
                    if *ra != target_r {
                        self.push_commented(
                            S16Instr::mov(target_r, *ra),
                            format!("arg {} = {}", i + 1, describe_val(&args[i])),
                        );
                    } else if *emitted {
                        self.annotate_last_write(
                            target_r,
                            format!("arg {} = {}", i + 1, describe_val(&args[i])),
                        );
                    }
                    if *free_a {
                        self.reg.free_reg(*ra);
                    }
                }
                None => {
                    if let Value::Imm(imm) = &args[i] {
                        self.push_commented(
                            S16Instr::lea_imm(target_r, *imm),
                            format!("arg {} = {}", i + 1, imm),
                        );
                    }
                }
            }
        }

        // Step 4: Emit call.
        self.push_commented(
            S16Instr::jal_label(Register::LINK_REG, func),
            format!("call {}", func),
        );

        // Step 5: Free stale caller-saved bindings.
        if self.advanced_mode {
            for &r in &Register::CALLER_SAVED {
                self.reg.free_reg(r);
            }
        }

        // Step 6: Bind return value.
        if let Some(dst) = ret {
            if dst.kind == VarKind::Global {
                self.note_user_var(&dst.name);
            }
            self.reg.bind_var_to_reg(dst.clone(), Register::R1);
            self.reg.mark_dirty(dst);
        }
    }

    // ── Return ──────────────────────────────────────────────────────────

    fn emit_return(&mut self, value: &Option<Value>) {
        if let Some(v) = value {
            let (rv, free_v) = self.ensure_in_reg(v);
            if rv != Register::R1 {
                self.push_commented(
                    S16Instr::mov(Register::R1, rv),
                    format!("return {}", describe_val(v)),
                );
            }
            if free_v {
                self.reg.free_reg(rv);
            }
        }
        let mut out = Vec::new();
        self.reg.flush_globals(&mut out);
        self.drain_regalloc(out);
        if let Some(func_name) = &self.current_func_name {
            let epilogue_label = format!("ret_{}", func_name);
            self.push_asm(S16Instr::jump_label(&epilogue_label));
        }
    }
}
