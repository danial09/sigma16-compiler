//! Greedy (basic) register allocator.
//!
//! Uses a simple LRU eviction strategy with no liveness information.
//! Suitable for unoptimised builds or as a baseline.

use super::{emit_c, AllocState, RegAllocator};
use crate::backend::abi::Register;
use crate::backend::instruction::{AnnotatedInstr, S16Instr};
use crate::ir::{Value, Var, VarKind};

pub struct GreedyRegAllocator {
    state: AllocState,
    /// LRU usage order — oldest first.
    usage_order: Vec<Register>,
}

impl GreedyRegAllocator {
    pub fn new() -> Self {
        Self {
            state: AllocState::new(),
            usage_order: Vec::new(),
        }
    }

    /// Mark a register as recently used (move to end of LRU list).
    fn touch(&mut self, reg: Register) {
        if let Some(pos) = self.usage_order.iter().position(|&r| r == reg) {
            self.usage_order.remove(pos);
        }
        self.usage_order.push(reg);
    }

    /// Pick a spill victim using LRU (oldest used register).
    fn get_spill_victim(&self) -> Register {
        for &r in &self.usage_order {
            if Register::CALLER_SAVED.contains(&r) && !self.state.temp_busy.contains(&r) {
                return r;
            }
        }
        for &r in &self.usage_order {
            if !self.state.temp_busy.contains(&r) {
                return r;
            }
        }
        panic!("No registers to spill (all are temp busy)");
    }
}

impl RegAllocator for GreedyRegAllocator {
    fn bind_var_to_reg(&mut self, var: Var, reg: Register) {
        self.state.bind(var, reg);
        self.touch(reg);
    }

    fn mark_dirty(&mut self, var: &Var) {
        self.state.mark_dirty(var);
    }

    fn clear_temp_busy(&mut self) {
        self.state.clear_temp_busy();
    }

    fn spill_reg(&mut self, reg: Register, out: &mut Vec<AnnotatedInstr>) {
        if let Some(var) = self.state.reg_to_var.remove(&reg) {
            self.state.var_to_reg.remove(&var);
            let is_dirty = self.state.dirty.remove(&var);

            if var.is_reg_allocated() {
                self.state.write_back(reg, &var, out);
            } else if is_dirty {
                emit_c(
                    out,
                    S16Instr::store_label(reg, &var.name),
                    format!("writeback {}", var.name),
                );
            }
        }
        if let Some(pos) = self.usage_order.iter().position(|&r| r == reg) {
            self.usage_order.remove(pos);
        }
    }

    fn allocate_reg(&mut self, out: &mut Vec<AnnotatedInstr>) -> Register {
        for &r in &Register::GP_REGS {
            if !self.state.reg_to_var.contains_key(&r)
                && !self.state.temp_busy.contains(&r)
            {
                self.touch(r);
                self.state.temp_busy.insert(r);
                return r;
            }
        }
        let victim = self.get_spill_victim();
        self.spill_reg(victim, out);
        self.touch(victim);
        self.state.temp_busy.insert(victim);
        victim
    }

    fn ensure_in_reg(
        &mut self,
        v: &Value,
        out: &mut Vec<AnnotatedInstr>,
        note_user: &mut dyn FnMut(&str),
    ) -> (Register, bool) {
        match v {
            Value::Imm(0) => (Register::ZERO_REG, true),
            Value::Imm(i) => {
                let r = self.allocate_reg(out);
                out.push((S16Instr::lea_imm(r, *i), None));
                (r, true)
            }
            Value::Var(var) => {
                if let Some(&r) = self.state.var_to_reg.get(var) {
                    self.touch(r);
                    self.state.temp_busy.insert(r);
                    return (r, false);
                }
                let r = self.allocate_reg(out);
                self.state.load_from_home(r, var, out, note_user);
                self.bind_var_to_reg(var.clone(), r);
                self.state.temp_busy.insert(r);
                (r, false)
            }
            Value::AddrOf(name) => {
                note_user(name);
                let r = self.allocate_reg(out);
                emit_c(out, S16Instr::lea_label(r, name.as_str()), format!("&{}", name));
                self.state.temp_busy.insert(r);
                (r, true)
            }
        }
    }

    fn ensure_var_in_reg(
        &mut self,
        var: &Var,
        out: &mut Vec<AnnotatedInstr>,
        note_user: &mut dyn FnMut(&str),
        prefer_reg: Option<Register>,
    ) -> Register {
        if let Some(&r) = self.state.var_to_reg.get(var) {
            self.touch(r);
            self.state.temp_busy.insert(r);
            return r;
        }

        let r = if let Some(pref) = prefer_reg {
            if !self.state.reg_to_var.contains_key(&pref)
                && !self.state.temp_busy.contains(&pref)
            {
                self.touch(pref);
                self.state.temp_busy.insert(pref);
                pref
            } else {
                self.allocate_reg(out)
            }
        } else {
            self.allocate_reg(out)
        };

        self.state.load_from_home(r, var, out, note_user);
        self.bind_var_to_reg(var.clone(), r);
        self.state.temp_busy.insert(r);
        r
    }

    fn get_var_reg(&self, var: &Var) -> Option<Register> {
        self.state.get_var_reg(var)
    }

    fn free_reg(&mut self, r: Register) {
        self.state.free_reg(r);
        if let Some(pos) = self.usage_order.iter().position(|&reg| reg == r) {
            self.usage_order.remove(pos);
        }
    }

    fn spill_caller_saved(&mut self, out: &mut Vec<AnnotatedInstr>) {
        let regs_to_spill: Vec<Register> = self
            .state
            .reg_to_var
            .keys()
            .filter(|&&r| Register::CALLER_SAVED.contains(&r))
            .copied()
            .collect();
        for r in regs_to_spill {
            self.spill_reg(r, out);
        }
    }

    fn flush_all(&mut self, out: &mut Vec<AnnotatedInstr>) {
        let regs: Vec<Register> = self.state.reg_to_var.keys().copied().collect();
        for r in regs {
            self.spill_reg(r, out);
        }
    }

    fn flush_globals(&mut self, out: &mut Vec<AnnotatedInstr>) {
        let regs: Vec<Register> = self
            .state
            .reg_to_var
            .iter()
            .filter(|(_, v)| v.kind == VarKind::Global)
            .map(|(&r, _)| r)
            .collect();
        for r in regs {
            self.spill_reg(r, out);
        }
    }

    fn begin_region(&mut self) {
        self.state.reset();
        self.usage_order.clear();
    }

    fn get_max_slots(&self) -> usize {
        self.state.max_slots
    }
}
