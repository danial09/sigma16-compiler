//! Liveness-aware (advanced) register allocator.
//!
//! Uses dataflow liveness information and Belady's optimal replacement
//! algorithm for spill-victim selection.  Supports register-resident mode
//! where variables are permanently assigned to registers.

use super::{AllocState, RegAllocator, emit_c};
use crate::backend::abi::Register;
use crate::backend::instruction::{AnnotatedInstr, S16Instr};
use crate::backend::liveness::LivenessInfo;
use crate::ir::{Value, Var, VarKind};
use std::collections::{HashMap, HashSet};

pub struct AdvancedRegAllocator {
    state: AllocState,
    /// Liveness information for the current region.
    liveness: Option<LivenessInfo>,
    /// Current global instruction index.
    current_idx: usize,
    /// Fixed variable → register assignments (register-resident mode).
    fixed_assignments: HashMap<Var, Register>,
    /// Whether register-resident mode is active.
    register_resident: bool,
    /// Registers reserved for fixed-assignment variables.
    reserved_regs: HashSet<Register>,
}

impl AdvancedRegAllocator {
    pub fn new() -> Self {
        Self {
            state: AllocState::new(),
            liveness: None,
            current_idx: 0,
            fixed_assignments: HashMap::new(),
            register_resident: false,
            reserved_regs: HashSet::new(),
        }
    }

    /// Check if a variable is dead (no future uses) at the current instruction.
    fn is_dead(&self, var: &Var) -> bool {
        // Globals are externally observable — never consider them dead.
        if var.is_global() {
            return false;
        }
        match &self.liveness {
            Some(info) => !info.is_live_after(self.current_idx, var),
            None => false, // conservative: assume live
        }
    }

    /// Find the best register to spill using Belady's optimal algorithm:
    /// 1. Dead variables (free eviction, no store needed).
    /// 2. Among live variables, pick the one with farthest next-use.
    fn find_spill_victim(&self) -> Register {
        let mut best_dead: Option<Register> = None;
        let mut best_live: Option<(Register, usize)> = None;

        for (&reg, var) in &self.state.reg_to_var {
            if self.state.temp_busy.contains(&reg) {
                continue;
            }
            // Never evict a register-resident variable.
            if self.register_resident && self.reserved_regs.contains(&reg) {
                continue;
            }

            if self.is_dead(var) {
                if best_dead.is_none() || !self.state.dirty.contains(var) {
                    best_dead = Some(reg);
                }
            } else {
                let dist = self
                    .liveness
                    .as_ref()
                    .map(|info| info.next_use_after(self.current_idx, var))
                    .unwrap_or(0);

                let is_better = match best_live {
                    None => true,
                    Some((_, best_dist)) => {
                        if dist != best_dist {
                            dist > best_dist
                        } else {
                            Register::CALLER_SAVED.contains(&reg)
                        }
                    }
                };

                if is_better {
                    best_live = Some((reg, dist));
                }
            }
        }

        best_dead
            .or(best_live.map(|(r, _)| r))
            .expect("No registers to spill (all are temp busy or reserved)")
    }
}

impl RegAllocator for AdvancedRegAllocator {
    fn bind_var_to_reg(&mut self, var: Var, reg: Register) {
        self.state.bind(var, reg);
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

            // Skip spilling dead variables entirely.
            if self.is_dead(&var) {
                return;
            }

            if var.is_reg_allocated() {
                // Only store if dirty (stack slot is up-to-date otherwise).
                if is_dirty || !self.state.spilled.contains_key(&var) {
                    self.state.write_back(reg, &var, out);
                }
            } else if is_dirty {
                emit_c(
                    out,
                    S16Instr::store_label(reg, &var.name),
                    format!("writeback {}", var.name),
                );
            }
        }
    }

    fn allocate_reg(&mut self, out: &mut Vec<AnnotatedInstr>) -> Register {
        // 1. Find a free register (skip reserved).
        for &r in &Register::GP_REGS {
            if !self.state.reg_to_var.contains_key(&r)
                && !self.state.temp_busy.contains(&r)
                && !self.reserved_regs.contains(&r)
            {
                self.state.temp_busy.insert(r);
                return r;
            }
        }

        // 2. Spill the best victim.
        let victim = self.find_spill_victim();
        self.spill_reg(victim, out);
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
                    self.state.temp_busy.insert(r);
                    return (r, false);
                }
                let r = self.allocate_reg(out);
                self.state.load_from_home(r, var, out, note_user);
                self.state.bind(var.clone(), r);
                self.state.temp_busy.insert(r);
                (r, false)
            }
            Value::AddrOf(name) => {
                note_user(name);
                let r = self.allocate_reg(out);
                emit_c(
                    out,
                    S16Instr::lea_label(r, name.as_str()),
                    format!("&{}", name),
                );
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
            self.state.temp_busy.insert(r);
            return r;
        }

        // In register-resident mode: use the fixed assignment register.
        let r = if let Some(&fixed_r) = self.fixed_assignments.get(var) {
            if let Some(occupant) = self.state.reg_to_var.get(&fixed_r).cloned() {
                if occupant != *var {
                    self.spill_reg(fixed_r, out);
                }
            }
            self.state.temp_busy.insert(fixed_r);
            fixed_r
        } else if let Some(pref) = prefer_reg {
            if !self.state.reg_to_var.contains_key(&pref)
                && !self.state.temp_busy.contains(&pref)
                && !self.reserved_regs.contains(&pref)
            {
                self.state.temp_busy.insert(pref);
                pref
            } else {
                self.allocate_reg(out)
            }
        } else {
            self.allocate_reg(out)
        };

        self.state.load_from_home(r, var, out, note_user);
        self.state.bind(var.clone(), r);
        self.state.temp_busy.insert(r);
        r
    }

    fn get_var_reg(&self, var: &Var) -> Option<Register> {
        self.state.get_var_reg(var)
    }

    fn free_reg(&mut self, r: Register) {
        self.state.free_reg(r);
    }

    fn spill_caller_saved(&mut self, out: &mut Vec<AnnotatedInstr>) {
        let regs_to_spill: Vec<Register> = self
            .state
            .reg_to_var
            .iter()
            .filter(|&(&r, var)| {
                if !Register::CALLER_SAVED.contains(&r) {
                    return false;
                }
                // Dead variables in temp_busy regs don't need saving.
                if self.state.temp_busy.contains(&r) && self.is_dead(var) {
                    return false;
                }
                true
            })
            .map(|(&r, _)| r)
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
        self.liveness = None;
        self.current_idx = 0;
        self.fixed_assignments.clear();
        self.register_resident = false;
        self.reserved_regs.clear();
    }

    fn get_max_slots(&self) -> usize {
        self.state.max_slots
    }

    fn is_var_dead(&self, var: &Var) -> bool {
        self.is_dead(var)
    }

    fn flush_dirty(&mut self, out: &mut Vec<AnnotatedInstr>) {
        if self.register_resident {
            // Register-resident: only flush dirty globals.
            let global_entries: Vec<(Register, Var)> = self
                .state
                .reg_to_var
                .iter()
                .filter(|(_, var)| var.is_global() && self.state.dirty.contains(*var))
                .map(|(&r, v)| (r, v.clone()))
                .collect();

            for (reg, var) in global_entries {
                self.state.write_back(reg, &var, out);
                self.state.dirty.remove(&var);
            }
            return;
        }

        // Non-resident: write all dirty or un-spilled variables.
        let entries: Vec<(Register, Var)> = self
            .state
            .reg_to_var
            .iter()
            .filter(|(_, var)| {
                self.state.dirty.contains(var)
                    || (var.is_reg_allocated() && !self.state.spilled.contains_key(var))
            })
            .map(|(&r, v)| (r, v.clone()))
            .collect();

        for (reg, var) in entries {
            if !self.is_dead(&var) {
                self.state.write_back(reg, &var, out);
            }
            self.state.dirty.remove(&var);
        }
    }

    fn clear_bindings(&mut self) {
        if self.register_resident {
            return;
        }
        self.state.var_to_reg.clear();
        self.state.reg_to_var.clear();
        self.state.dirty.clear();
    }

    fn prepare_def_reg(
        &mut self,
        var: &Var,
        out: &mut Vec<AnnotatedInstr>,
        _note_user: &mut dyn FnMut(&str),
    ) -> Register {
        if let Some(&r) = self.state.var_to_reg.get(var) {
            self.state.temp_busy.insert(r);
            return r;
        }
        if let Some(&fixed_r) = self.fixed_assignments.get(var) {
            if self.state.reg_to_var.contains_key(&fixed_r) {
                self.spill_reg(fixed_r, out);
            }
            self.state.temp_busy.insert(fixed_r);
            return fixed_r;
        }
        // Allocate a fresh register — do NOT load old value.
        self.allocate_reg(out)
    }

    fn set_liveness(&mut self, info: LivenessInfo) {
        self.liveness = Some(info);
    }

    fn set_current_instruction(&mut self, idx: usize) {
        self.current_idx = idx;
    }

    fn set_register_resident(&mut self, assignments: HashMap<Var, Register>) {
        self.register_resident = true;
        self.reserved_regs = assignments.values().copied().collect();
        self.fixed_assignments = assignments;
    }

    fn is_register_resident(&self) -> bool {
        self.register_resident
    }

    fn get_fixed_reg(&self, var: &Var) -> Option<Register> {
        self.fixed_assignments.get(var).copied()
    }
}
