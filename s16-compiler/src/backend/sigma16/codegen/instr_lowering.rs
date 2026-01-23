use crate::ir::{ArithOp, Instr,RelOp,Rhs, Value, Var, VarKind};
use super::emitter::Codegen;
use super::super::abi::Register;

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
                for l in out {
                    self.emit(l);
                }
                self.flush_function();
            }
            Instr::Label(lbl) => {
                let mut out = Vec::new();
                self.reg.flush_all(&mut out);
                for l in out {
                    self.emit(l);
                }
                self.emit(format!("{}:", lbl));
            }
            Instr::Goto(t) => {
                let mut out = Vec::new();
                self.reg.flush_all(&mut out);
                for l in out {
                    self.emit(l);
                }
                self.emit(format!("  jump {}[{}]", t, Register::ZERO_REG));
            }
            Instr::IfCmpGoto {
                left,
                op,
                right,
                target,
            } => {
                let (rl, free_l) = self.ensure_in_reg(left);
                let (rr, free_r) = self.ensure_in_reg(right);
                self.emit(format!("  cmp {rl},{rr}"));
                if free_l {
                    self.reg.free_reg(rl);
                }
                if free_r {
                    self.reg.free_reg(rr);
                }

                let mut out = Vec::new();
                self.reg.flush_all(&mut out);
                for l in out {
                    self.emit(l);
                }

                let j = match op {
                    RelOp::Eq => "jumpeq",
                    RelOp::Neq => "jumpne",
                    RelOp::Lt => "jumplt",
                    RelOp::Gt => "jumpgt",
                    RelOp::Le => "jumple",
                    RelOp::Ge => "jumpge",
                };
                self.emit(format!("  {j} {target}[{}]", Register::ZERO_REG));
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

                    // For global variables, store directly to memory instead of loading them
                    if dst.kind == VarKind::Global {
                        self.note_user_var(&dst.name);
                        self.emit(format!("  store {},{}[{}]", rs, dst.name,Register::ZERO_REG));

                        // If source is a variable that's now stored to global, we can clear its dirty bit
                        // since we've persisted it to memory
                        if let Value::Var(src_var) = v {
                            if src_var.is_reg_allocated() {
                                // Source is a local/temp, mark it as clean since we just stored it
                                if let Some(src_reg) = self.get_var_reg(src_var) {
                                    if src_reg == rs {
                                        // Unbind the temp variable - we don't need it anymore
                                        self.reg.free_reg(rs);
                                    }
                                }
                            }
                        }

                        if free_s {
                            self.reg.free_reg(rs);
                        }
                    } else {
                        // For local/temp variables, use register allocation
                        let (rd, _) = self.ensure_in_reg(&Value::Var(dst.clone()));
                        if rd != rs {
                            self.emit(format!("  add {},{},{}", rd,Register::ZERO_REG, rs));
                        }
                        self.reg.mark_dirty(dst);
                        if free_s && rs != rd {
                            self.reg.free_reg(rs);
                        }
                    }
                }
                Rhs::Binary { op, left, right } => {
                    let op_str = match op {
                        ArithOp::Add => "add",
                        ArithOp::Sub => "sub",
                        ArithOp::Mul => "mul",
                        ArithOp::Div => "div",
                    };

                    // Check if dst is already one of the operands
                    let dst_is_left = matches!(left, Value::Var(v) if v == dst);
                    let dst_is_right = matches!(right, Value::Var(v) if v == dst);

                    // Strategy: Load dst operand first if it exists, to reuse the register
                    let (rl, free_l, rr, free_r, rd) = if dst_is_left {
                        // Load left (which is dst) first, then right
                        let (rl, free_l) = self.ensure_in_reg(left);
                        let (rr, free_r) = self.ensure_in_reg(right);
                        (rl, free_l, rr, free_r, rl)
                    } else if dst_is_right && (*op == ArithOp::Add || *op == ArithOp::Mul) {
                        // For commutative ops, if dst is right, swap operands
                        let (rr, free_r) = self.ensure_in_reg(right);
                        let (rl, free_l) = self.ensure_in_reg(left);
                        (rl, free_l, rr, free_r, rr)
                    } else {
                        // Standard case: load operands, then allocate for dst
                        let (rl, free_l) = self.ensure_in_reg(left);
                        let (rr, free_r) = self.ensure_in_reg(right);

                        // Try to reuse one of the operand registers if it's temporary
                        let rd = if free_l {
                            rl
                        } else if free_r && (*op == ArithOp::Add || *op == ArithOp::Mul) {
                            rr
                        } else {
                            self.ensure_var_in_reg(dst, None)
                        };
                        (rl, free_l, rr, free_r, rd)
                    };

                    self.emit(format!("  {op_str} {rd},{rl},{rr}"));

                    // Bind destination to register if not already bound
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
                let (rd, _) = self.ensure_in_reg(&Value::Var(dst.clone()));
                self.emit(format!("  load {rd},0[{ra}]"));
                self.reg.mark_dirty(dst);
                if free_a && ra != rd {
                    self.reg.free_reg(ra);
                }
            }
            Instr::Store { addr, src } => {
                let (ra, free_a) = self.ensure_in_reg(addr);
                let (rs, free_s) = self.ensure_in_reg(src);
                self.emit(format!("  store {rs},0[{ra}]"));
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
                self.emit(format!("  lea {},{}[{}]", addr, base,Register::ZERO_REG));
                self.emit(format!("  add {},{},{}", addr, addr, ri));

                let (rd, _) = self.ensure_in_reg(&Value::Var(dst.clone()));
                self.emit(format!("  load {},0[{}]", rd, addr));
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
                self.emit(format!("  lea {},{}[{}]", addr, base,Register::ZERO_REG));
                self.emit(format!("  add {},{},{}", addr, addr, ri));
                self.emit(format!("  store {},0[{}]", rs, addr));
                self.reg.free_reg(addr);
                if free_i {
                    self.reg.free_reg(ri);
                }
                if free_s {
                    self.reg.free_reg(rs);
                }
            }
            Instr::Call { func, args, ret } => {
                let mut spill_out = Vec::new();
                self.reg.spill_caller_saved(&mut spill_out);
                for line in spill_out {
                    self.emit(line);
                }
                for (i, a) in args.iter().take(8).enumerate() {
                    let (ra, free_a) = self.ensure_in_reg(a);
                    let target_r = Register::PARAM_REGS[i];
                    if ra != target_r {
                        self.emit(format!("  add {},{},{}", target_r,Register::ZERO_REG, ra));
                    }
                    if free_a {
                        self.reg.free_reg(ra);
                    }
                }
                self.emit(format!("  jal {},{}[{}]",Register::LINK_REG, func,Register::ZERO_REG));
                if let Some(dst) = ret {
                    // Return value is in R1. Bind destination directly to R1.
                    self.reg.bind_var_to_reg(dst.clone(),Register::R1);
                    self.reg.mark_dirty(dst);
                }
            }
            Instr::Return { value } => {
                if let Some(v) = value {
                    let (rv, free_v) = self.ensure_in_reg(v);
                    // Only emit add if the value is not already in R1
                    if rv != Register::R1 {
                        self.emit(format!("  add {},{},{}",Register::R1,Register::ZERO_REG, rv));
                    }
                    if free_v {
                        self.reg.free_reg(rv);
                    }
                }
                let mut out = Vec::new();
                self.reg.flush_globals(&mut out);
                for l in out {
                    self.emit(l);
                }
            }
            Instr::ArrayDecl { .. } => {
                // Array declarations are handled by finish_codegen
            }
        }
    }
}
