use super::super::abi::Register;
use super::emitter::Codegen;
use super::item::{Cond, Disp, S16Instr};
use crate::ir::{ArithOp, Instr, RelOp, Rhs, Value, Var, VarKind};

impl Codegen {
    pub fn emit_instr(&mut self, instr: &Instr) {
        match instr {
            Instr::FuncStart { name, params } => {
                self.start_function(name.clone());
                // Parameters are passed in R1..R8. Bind them to local variables.
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
                // Record the IR index for epilogue mapping
                self.current_func_end_ir = self.current_ir;
                self.flush_function();
            }
            Instr::Label(lbl) => {
                if self.advanced_mode {
                    // Flush dirty vars (handles fall-through from previous code),
                    // then clear all bindings (merge point: unknown incoming state).
                    let mut out = Vec::new();
                    self.reg.flush_dirty(&mut out);
                    self.drain_regalloc(out);
                    self.reg.clear_bindings();
                } else {
                    let mut out = Vec::new();
                    self.reg.flush_all(&mut out);
                    self.drain_regalloc(out);
                }
                self.push_label(lbl.clone());
            }
            Instr::Goto(t) => {
                if self.advanced_mode {
                    // Write dirty vars to home locations, then clear bindings.
                    let mut out = Vec::new();
                    self.reg.flush_dirty(&mut out);
                    self.drain_regalloc(out);
                    self.reg.clear_bindings();
                } else {
                    let mut out = Vec::new();
                    self.reg.flush_all(&mut out);
                    self.drain_regalloc(out);
                }
                self.push_asm(S16Instr::jump_label(t.as_str()));
            }
            Instr::IfCmpGoto {
                left,
                op,
                right,
                target,
            } => {
                let (rl, free_l) = self.ensure_in_reg(left);
                let (rr, free_r) = self.ensure_in_reg(right);
                self.push_asm(S16Instr::Cmp { a: rl, b: rr });
                if free_l {
                    self.reg.free_reg(rl);
                }
                if free_r {
                    self.reg.free_reg(rr);
                }

                if self.advanced_mode {
                    // Flush dirty vars (branch target needs consistent memory),
                    // but KEEP bindings for the fall-through path.
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
                    disp: Disp::Label(target.clone()),
                    idx: Register::ZERO_REG,
                });
            }
            Instr::Assign { dst, src } => match src {
                Rhs::Value(v) => {
                    let is_top_level = self.func_buf.is_none();
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
                        // Basic mode: store globals directly to memory
                        self.note_user_var(&dst.name);
                        self.push_asm(S16Instr::store_label(rs, &dst.name));

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
                        // Advanced mode: register-allocate all variables (including globals).
                        // Basic mode: register-allocate locals/temps only.
                        if dst.kind == VarKind::Global {
                            self.note_user_var(&dst.name);
                        }

                        // Optimization: if the source is a temporary (from an
                        // immediate) or a dead variable, rebind its register
                        // directly to the destination — no copy needed.
                        let can_rebind = if free_s {
                            true
                        } else if let Value::Var(src_var) = v {
                            src_var != dst && self.reg.is_var_dead(src_var)
                        } else {
                            false
                        };

                        if can_rebind {
                            self.reg.bind_var_to_reg(dst.clone(), rs);
                            self.reg.mark_dirty(dst);
                            // Emit a no-op move to preserve source→IR→ASM mapping
                            self.push_asm(S16Instr::mov(rs, rs));
                        } else {
                            // Use prepare_def_reg to avoid loading old value of dst
                            let rd = self.prepare_def_reg(dst);
                            if rd != rs {
                                self.push_asm(S16Instr::mov(rd, rs));
                            }
                            self.reg.bind_var_to_reg(dst.clone(), rd);
                            self.reg.mark_dirty(dst);
                            if free_s && rs != rd {
                                self.reg.free_reg(rs);
                            }
                        }
                    }
                }
                Rhs::Binary { op, left, right } => {
                    let is_mod = *op == ArithOp::Mod;

                    if dst.kind == VarKind::Global {
                        self.note_user_var(&dst.name);
                    }

                    // Check if dst is already one of the operands
                    let dst_is_left = matches!(left, Value::Var(v) if v == dst);
                    let dst_is_right = matches!(right, Value::Var(v) if v == dst);

                    let (rl, free_l, rr, free_r, rd) = if is_mod {
                        // For modulo, the div result is discarded; remainder lands in R15.
                        // We must not clobber an operand register with the div destination.
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
                            // Use prepare_def_reg to avoid loading old value
                            self.prepare_def_reg(dst)
                        };
                        (rl, free_l, rr, free_r, rd)
                    };

                    if is_mod {
                        // div R0,rl,rr  — quotient discarded into R0, remainder in R15
                        self.push_asm(S16Instr::Div {
                            d: Register::ZERO_REG,
                            a: rl,
                            b: rr,
                        });
                        // move remainder from R15 into the destination register
                        self.push_asm(S16Instr::Add {
                            d: rd,
                            a: Register::ZERO_REG,
                            b: Register::R15,
                        });
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
                        self.push_asm(instr);
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
            },
            Instr::Load { dst, addr } => {
                let (ra, free_a) = self.ensure_in_reg(addr);
                let rd = self.prepare_def_reg(dst);
                self.push_asm(S16Instr::load_disp(rd, 0, ra));
                self.reg.bind_var_to_reg(dst.clone(), rd);
                self.reg.mark_dirty(dst);
                if free_a && ra != rd {
                    self.reg.free_reg(ra);
                }
            }
            Instr::Store { addr, src } => {
                let (ra, free_a) = self.ensure_in_reg(addr);
                let (rs, free_s) = self.ensure_in_reg(src);
                self.push_asm(S16Instr::store_disp(rs, 0, ra));

                // If addr is AddrOf(name), the store writes to the memory
                // location of global variable `name`. Any cached register
                // binding for that variable is now stale - free it without
                // writing back (memory has the authoritative value).
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
            Instr::ArrayLoad { dst, base, index } => {
                let (ri, free_i) = self.ensure_in_reg(index);
                let addr = self.allocate_temp_reg();
                self.note_user_var(base);
                self.push_asm(S16Instr::lea_label(addr, base.as_str()));
                self.push_asm(S16Instr::Add {
                    d: addr,
                    a: addr,
                    b: ri,
                });

                let rd = self.prepare_def_reg(dst);
                self.push_asm(S16Instr::load_disp(rd, 0, addr));
                self.reg.bind_var_to_reg(dst.clone(), rd);
                self.reg.mark_dirty(dst);

                self.reg.free_reg(addr);
                if free_i {
                    self.reg.free_reg(ri);
                }
            }
            Instr::ArrayStore { base, index, src } => {
                let (ri, free_i) = self.ensure_in_reg(index);
                let (rs, free_s) = self.ensure_in_reg(src);
                let addr = self.allocate_temp_reg();
                self.note_user_var(base);
                self.push_asm(S16Instr::lea_label(addr, base.as_str()));
                self.push_asm(S16Instr::Add {
                    d: addr,
                    a: addr,
                    b: ri,
                });
                self.push_asm(S16Instr::store_disp(rs, 0, addr));
                self.reg.free_reg(addr);
                if free_i {
                    self.reg.free_reg(ri);
                }
                if free_s {
                    self.reg.free_reg(rs);
                }
            }
            Instr::Call { func, args, ret } => {
                // Step 1: Evaluate all arguments into registers first.
                // This pins them as temp_busy so they survive spilling.
                let mut arg_info: Vec<(Register, bool)> = Vec::new();
                for a in args.iter().take(8) {
                    let (ra, free_a) = self.ensure_in_reg(a);
                    arg_info.push((ra, free_a));
                }

                // Step 2: Spill caller-saved registers that need saving
                // across the call. The advanced allocator skips temp_busy
                // registers holding dead variables (e.g. args consumed by
                // the call), while still spilling live variables.
                let mut spill_out = Vec::new();
                self.reg.spill_caller_saved(&mut spill_out);
                self.drain_regalloc(spill_out);

                // Step 3: Move evaluated args into parameter registers.
                for (i, &(ra, free_a)) in arg_info.iter().enumerate() {
                    let target_r = Register::PARAM_REGS[i];
                    if ra != target_r {
                        self.push_asm(S16Instr::mov(target_r, ra));
                    }
                    if free_a {
                        self.reg.free_reg(ra);
                    }
                }

                // Step 4: Emit the call.
                self.push_asm(S16Instr::jal_label(Register::LINK_REG, func.as_str()));

                // Step 5: After the call, all caller-saved registers may
                // be clobbered by the callee. Free any remaining stale
                // bindings (e.g. dead args that were kept in registers).
                if self.advanced_mode {
                    for &r in &Register::CALLER_SAVED {
                        self.reg.free_reg(r);
                    }
                }

                // Step 6: Bind return value.
                if let Some(dst) = ret {
                    self.reg.bind_var_to_reg(dst.clone(), Register::R1);
                    self.reg.mark_dirty(dst);
                }
            }
            Instr::Return { value } => {
                if let Some(v) = value {
                    let (rv, free_v) = self.ensure_in_reg(v);
                    if rv != Register::R1 {
                        self.push_asm(S16Instr::mov(Register::R1, rv));
                    }
                    if free_v {
                        self.reg.free_reg(rv);
                    }
                }
                let mut out = Vec::new();
                self.reg.flush_globals(&mut out);
                self.drain_regalloc(out);
            }
            Instr::ArrayDecl { .. } => {
                // Array declarations are handled by finish_codegen
            }
        }
    }
}
