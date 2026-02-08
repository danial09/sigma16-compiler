use super::abi::Register;
use super::liveness::LivenessInfo;
use crate::ir::{Value, Var};
use std::collections::{HashMap, HashSet};

pub trait RegAllocator {
    fn bind_var_to_reg(&mut self, var: Var, reg: Register);
    fn mark_dirty(&mut self, var: &Var);
    fn clear_temp_busy(&mut self);
    fn spill_reg(&mut self, reg: Register, out: &mut Vec<String>);
    fn allocate_reg(&mut self, out: &mut Vec<String>) -> Register;
    fn ensure_in_reg(
        &mut self,
        v: &Value,
        out: &mut Vec<String>,
        note_user: &mut dyn FnMut(&str),
    ) -> (Register, bool);
    fn ensure_var_in_reg(
        &mut self,
        var: &Var,
        out: &mut Vec<String>,
        note_user: &mut dyn FnMut(&str),
        prefer_reg: Option<Register>,
    ) -> Register;
    fn get_var_reg(&self, var: &Var) -> Option<Register>;
    fn free_reg(&mut self, r: Register);
    fn spill_caller_saved(&mut self, out: &mut Vec<String>);
    fn flush_all(&mut self, out: &mut Vec<String>);
    fn flush_globals(&mut self, out: &mut Vec<String>);
    fn begin_region(&mut self);
    fn get_max_slots(&self) -> usize;

    // --- Extended methods with defaults for backward compatibility ---

    /// Write only dirty variables to their home locations, keep register bindings.
    fn flush_dirty(&mut self, out: &mut Vec<String>) {
        // Default: fall back to flush_all (which also clears bindings)
        self.flush_all(out);
    }

    /// Drop all register-to-variable bindings without writing anything.
    fn clear_bindings(&mut self) {
        // Default: no-op (flush_all already clears in the basic allocator)
    }

    /// Allocate a register for a variable being defined (assigned to).
    /// Unlike ensure_var_in_reg, this does NOT load the old value.
    fn prepare_def_reg(
        &mut self,
        var: &Var,
        out: &mut Vec<String>,
        note_user: &mut dyn FnMut(&str),
    ) -> Register {
        // Default: fall back to ensure_var_in_reg (may emit unnecessary load)
        self.ensure_var_in_reg(var, out, note_user, None)
    }

    /// Check if a variable is dead (no future uses) at the current instruction.
    /// Returns false if liveness info is unavailable (conservative).
    fn is_var_dead(&self, _var: &Var) -> bool {
        false
    }

    /// Provide liveness information for the current region.
    fn set_liveness(&mut self, _info: LivenessInfo) {}

    /// Update the current instruction index (global).
    fn set_current_instruction(&mut self, _idx: usize) {}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocatorKind {
    Basic,
    Advanced,
}

// ============================================================================
// GreedyRegAllocator (Basic) - unchanged from original
// ============================================================================

pub struct GreedyRegAllocator {
    var_to_reg: HashMap<Var, Register>,
    reg_to_var: HashMap<Register, Var>,
    dirty: HashSet<Var>,
    temp_busy: HashSet<Register>,
    spilled: HashMap<Var, usize>,
    next_slot: usize,
    max_slots: usize,
    next_reg_idx: usize,
    usage_order: Vec<Register>,
}

impl GreedyRegAllocator {
    pub fn new() -> Self {
        Self {
            var_to_reg: HashMap::new(),
            reg_to_var: HashMap::new(),
            dirty: HashSet::new(),
            temp_busy: HashSet::new(),
            spilled: HashMap::new(),
            next_slot: 0,
            max_slots: 0,
            next_reg_idx: 0,
            usage_order: Vec::new(),
        }
    }

    fn touch(&mut self, reg: Register) {
        if let Some(pos) = self.usage_order.iter().position(|&r| r == reg) {
            self.usage_order.remove(pos);
        }
        self.usage_order.push(reg);
    }

    fn get_spill_victim(&self) -> Register {
        for &r in &self.usage_order {
            if Register::CALLER_SAVED.contains(&r) && !self.temp_busy.contains(&r) {
                return r;
            }
        }
        for &r in &self.usage_order {
            if !self.temp_busy.contains(&r) {
                return r;
            }
        }
        panic!("No registers to spill (all are temp busy)");
    }
}

impl RegAllocator for GreedyRegAllocator {
    fn bind_var_to_reg(&mut self, var: Var, reg: Register) {
        if let Some(old_reg) = self.var_to_reg.remove(&var) {
            self.reg_to_var.remove(&old_reg);
        }
        if let Some(old_var) = self.reg_to_var.remove(&reg) {
            self.var_to_reg.remove(&old_var);
            self.dirty.remove(&old_var);
        }
        self.var_to_reg.insert(var.clone(), reg);
        self.reg_to_var.insert(reg, var);
        self.touch(reg);
    }

    fn mark_dirty(&mut self, var: &Var) {
        if self.var_to_reg.contains_key(var) {
            self.dirty.insert(var.clone());
        }
    }

    fn clear_temp_busy(&mut self) {
        self.temp_busy.clear();
    }

    fn spill_reg(&mut self, reg: Register, out: &mut Vec<String>) {
        if let Some(var) = self.reg_to_var.remove(&reg) {
            self.var_to_reg.remove(&var);
            let is_dirty = self.dirty.remove(&var);

            if var.is_reg_allocated() {
                let slot = *self.spilled.entry(var.clone()).or_insert_with(|| {
                    self.next_slot += 1;
                    self.max_slots = self.max_slots.max(self.next_slot);
                    self.next_slot
                });
                let offset = -(self.max_slots as i32) + slot as i32 - 1;
                out.push(format!(
                    "  store {},{}[{}]",
                    reg,
                    offset,
                    Register::STACK_PTR
                ));
            } else if is_dirty {
                out.push(format!(
                    "  store {},{}[{}]",
                    reg,
                    var.name,
                    Register::ZERO_REG
                ));
            }
        }
        if let Some(pos) = self.usage_order.iter().position(|&r| r == reg) {
            self.usage_order.remove(pos);
        }
    }

    fn allocate_reg(&mut self, out: &mut Vec<String>) -> Register {
        for &r in &Register::GP_REGS {
            if !self.reg_to_var.contains_key(&r) && !self.temp_busy.contains(&r) {
                self.touch(r);
                self.temp_busy.insert(r);
                return r;
            }
        }
        let victim = self.get_spill_victim();
        self.spill_reg(victim, out);
        self.touch(victim);
        self.temp_busy.insert(victim);
        victim
    }

    fn ensure_in_reg(
        &mut self,
        v: &Value,
        out: &mut Vec<String>,
        note_user: &mut dyn FnMut(&str),
    ) -> (Register, bool) {
        match v {
            Value::Imm(i) => {
                let r = self.allocate_reg(out);
                out.push(format!("  lea {},{}[{}]", r, i, Register::ZERO_REG));
                (r, true)
            }
            Value::Var(var) => {
                if let Some(&r) = self.var_to_reg.get(var) {
                    self.touch(r);
                    self.temp_busy.insert(r);
                    (r, false)
                } else {
                    let r = self.allocate_reg(out);
                    if var.is_reg_allocated() {
                        if let Some(&slot) = self.spilled.get(var) {
                            let offset = -(self.max_slots as i32) + slot as i32 - 1;
                            out.push(format!("  load {},{}[{}]", r, offset, Register::STACK_PTR));
                        }
                    } else {
                        note_user(&var.name);
                        out.push(format!("  load {},{}[{}]", r, var.name, Register::ZERO_REG));
                    }
                    self.bind_var_to_reg(var.clone(), r);
                    self.temp_busy.insert(r);
                    (r, false)
                }
            }
            Value::AddrOf(name) => {
                note_user(name);
                let r = self.allocate_reg(out);
                out.push(format!("  lea {},{}[{}]", r, name, Register::ZERO_REG));
                self.temp_busy.insert(r);
                (r, true)
            }
        }
    }

    fn ensure_var_in_reg(
        &mut self,
        var: &Var,
        out: &mut Vec<String>,
        note_user: &mut dyn FnMut(&str),
        prefer_reg: Option<Register>,
    ) -> Register {
        if let Some(&r) = self.var_to_reg.get(var) {
            self.touch(r);
            self.temp_busy.insert(r);
            return r;
        }

        let r = if let Some(pref) = prefer_reg {
            if !self.reg_to_var.contains_key(&pref) && !self.temp_busy.contains(&pref) {
                self.touch(pref);
                self.temp_busy.insert(pref);
                pref
            } else {
                self.allocate_reg(out)
            }
        } else {
            self.allocate_reg(out)
        };

        if var.is_reg_allocated() {
            if let Some(&slot) = self.spilled.get(var) {
                let offset = -(self.max_slots as i32) + slot as i32 - 1;
                out.push(format!("  load {}, {}[{}]", r, offset, Register::STACK_PTR));
            }
        } else {
            note_user(&var.name);
            out.push(format!("  load {},{}[{}]", r, var.name, Register::ZERO_REG));
        }

        self.bind_var_to_reg(var.clone(), r);
        self.temp_busy.insert(r);
        r
    }

    fn get_var_reg(&self, var: &Var) -> Option<Register> {
        self.var_to_reg.get(var).copied()
    }

    fn free_reg(&mut self, r: Register) {
        if let Some(var) = self.reg_to_var.remove(&r) {
            self.var_to_reg.remove(&var);
            self.dirty.remove(&var);
        }
        if let Some(pos) = self.usage_order.iter().position(|&reg| reg == r) {
            self.usage_order.remove(pos);
        }
    }

    fn spill_caller_saved(&mut self, out: &mut Vec<String>) {
        let regs_to_spill: Vec<Register> = self
            .reg_to_var
            .keys()
            .filter(|&&r| Register::CALLER_SAVED.contains(&r))
            .copied()
            .collect();
        for r in regs_to_spill {
            self.spill_reg(r, out);
        }
    }

    fn flush_all(&mut self, out: &mut Vec<String>) {
        let regs: Vec<Register> = self.reg_to_var.keys().copied().collect();
        for r in regs {
            self.spill_reg(r, out);
        }
    }

    fn flush_globals(&mut self, out: &mut Vec<String>) {
        let regs: Vec<Register> = self
            .reg_to_var
            .iter()
            .filter(|(_, v)| v.kind == crate::ir::VarKind::Global)
            .map(|(&r, _)| r)
            .collect();
        for r in regs {
            self.spill_reg(r, out);
        }
    }

    fn begin_region(&mut self) {
        self.var_to_reg.clear();
        self.reg_to_var.clear();
        self.dirty.clear();
        self.spilled.clear();
        self.next_slot = 0;
        self.max_slots = 0;
        self.next_reg_idx = 0;
        self.usage_order.clear();
    }

    fn get_max_slots(&self) -> usize {
        self.max_slots
    }
}

// ============================================================================
// AdvancedRegAllocator - liveness-aware, smarter flushes
// ============================================================================

pub struct AdvancedRegAllocator {
    var_to_reg: HashMap<Var, Register>,
    reg_to_var: HashMap<Register, Var>,
    dirty: HashSet<Var>,
    temp_busy: HashSet<Register>,
    spilled: HashMap<Var, usize>,
    next_slot: usize,
    max_slots: usize,
    /// Liveness information for the current region
    liveness: Option<LivenessInfo>,
    /// Current global instruction index
    current_idx: usize,
}

impl AdvancedRegAllocator {
    pub fn new() -> Self {
        Self {
            var_to_reg: HashMap::new(),
            reg_to_var: HashMap::new(),
            dirty: HashSet::new(),
            temp_busy: HashSet::new(),
            spilled: HashMap::new(),
            next_slot: 0,
            max_slots: 0,
            liveness: None,
            current_idx: 0,
        }
    }

    /// Check if a variable is dead (no future uses) at the current instruction.
    fn is_dead(&self, var: &Var) -> bool {
        // Globals are externally observable - never consider them dead
        if var.is_global() {
            return false;
        }
        match &self.liveness {
            Some(info) => !info.is_live_after(self.current_idx, var),
            None => false, // conservative: assume live
        }
    }

    /// Find the best register to spill. Prefers:
    /// 1. Registers holding dead variables (no store needed)
    /// 2. Among live variables, pick the one with the farthest next-use
    ///    (approximated by preferring caller-saved, then LRU-like)
    fn find_spill_victim(&self) -> Register {
        let mut best_dead: Option<Register> = None;
        let mut best_live: Option<Register> = None;
        let mut best_live_is_caller = false;

        for (&reg, var) in &self.reg_to_var {
            if self.temp_busy.contains(&reg) {
                continue;
            }

            if self.is_dead(var) {
                // Dead variable: perfect victim (no spill cost)
                // Prefer one that's NOT dirty (truly free eviction)
                if best_dead.is_none() || !self.dirty.contains(var) {
                    best_dead = Some(reg);
                }
            } else {
                // Live variable: use heuristic
                let is_caller = Register::CALLER_SAVED.contains(&reg);
                if best_live.is_none()
                    || (is_caller && !best_live_is_caller)
                    || (is_caller == best_live_is_caller)
                {
                    best_live = Some(reg);
                    best_live_is_caller = is_caller;
                }
            }
        }

        // Prefer dead variables (cheapest to evict)
        best_dead
            .or(best_live)
            .expect("No registers to spill (all are temp busy)")
    }

    /// Write a single variable to its home location.
    fn write_back(&mut self, reg: Register, var: &Var, out: &mut Vec<String>) {
        if var.is_reg_allocated() {
            let slot = *self.spilled.entry(var.clone()).or_insert_with(|| {
                self.next_slot += 1;
                self.max_slots = self.max_slots.max(self.next_slot);
                self.next_slot
            });
            let offset = -(self.max_slots as i32) + slot as i32 - 1;
            out.push(format!(
                "  store {},{}[{}]",
                reg,
                offset,
                Register::STACK_PTR
            ));
        } else {
            // Global
            out.push(format!(
                "  store {},{}[{}]",
                reg,
                var.name,
                Register::ZERO_REG
            ));
        }
    }
}

impl RegAllocator for AdvancedRegAllocator {
    fn bind_var_to_reg(&mut self, var: Var, reg: Register) {
        if let Some(old_reg) = self.var_to_reg.remove(&var) {
            self.reg_to_var.remove(&old_reg);
        }
        if let Some(old_var) = self.reg_to_var.remove(&reg) {
            self.var_to_reg.remove(&old_var);
            self.dirty.remove(&old_var);
        }
        self.var_to_reg.insert(var.clone(), reg);
        self.reg_to_var.insert(reg, var);
    }

    fn mark_dirty(&mut self, var: &Var) {
        if self.var_to_reg.contains_key(var) {
            self.dirty.insert(var.clone());
        }
    }

    fn clear_temp_busy(&mut self) {
        self.temp_busy.clear();
    }

    fn spill_reg(&mut self, reg: Register, out: &mut Vec<String>) {
        if let Some(var) = self.reg_to_var.remove(&reg) {
            self.var_to_reg.remove(&var);
            let is_dirty = self.dirty.remove(&var);

            // Skip spilling dead variables entirely
            if self.is_dead(&var) {
                return;
            }

            if var.is_reg_allocated() {
                // For locals/temps: only store if dirty (stack slot is up-to-date otherwise)
                if is_dirty || !self.spilled.contains_key(&var) {
                    let slot = *self.spilled.entry(var.clone()).or_insert_with(|| {
                        self.next_slot += 1;
                        self.max_slots = self.max_slots.max(self.next_slot);
                        self.next_slot
                    });
                    let offset = -(self.max_slots as i32) + slot as i32 - 1;
                    out.push(format!(
                        "  store {},{}[{}]",
                        reg,
                        offset,
                        Register::STACK_PTR
                    ));
                }
            } else if is_dirty {
                // Global: only write if dirty
                out.push(format!(
                    "  store {},{}[{}]",
                    reg,
                    var.name,
                    Register::ZERO_REG
                ));
            }
        }
    }

    fn allocate_reg(&mut self, out: &mut Vec<String>) -> Register {
        // 1. Find a free register
        for &r in &Register::GP_REGS {
            if !self.reg_to_var.contains_key(&r) && !self.temp_busy.contains(&r) {
                self.temp_busy.insert(r);
                return r;
            }
        }

        // 2. Spill the best victim
        let victim = self.find_spill_victim();
        self.spill_reg(victim, out);
        self.temp_busy.insert(victim);
        victim
    }

    fn ensure_in_reg(
        &mut self,
        v: &Value,
        out: &mut Vec<String>,
        note_user: &mut dyn FnMut(&str),
    ) -> (Register, bool) {
        match v {
            Value::Imm(i) => {
                let r = self.allocate_reg(out);
                out.push(format!("  lea {},{}[{}]", r, i, Register::ZERO_REG));
                (r, true)
            }
            Value::Var(var) => {
                if let Some(&r) = self.var_to_reg.get(var) {
                    self.temp_busy.insert(r);
                    (r, false)
                } else {
                    let r = self.allocate_reg(out);
                    if var.is_reg_allocated() {
                        if let Some(&slot) = self.spilled.get(var) {
                            let offset = -(self.max_slots as i32) + slot as i32 - 1;
                            out.push(format!("  load {},{}[{}]", r, offset, Register::STACK_PTR));
                        }
                    } else {
                        note_user(&var.name);
                        out.push(format!("  load {},{}[{}]", r, var.name, Register::ZERO_REG));
                    }
                    self.bind_var_to_reg(var.clone(), r);
                    self.temp_busy.insert(r);
                    (r, false)
                }
            }
            Value::AddrOf(name) => {
                note_user(name);
                let r = self.allocate_reg(out);
                out.push(format!("  lea {},{}[{}]", r, name, Register::ZERO_REG));
                self.temp_busy.insert(r);
                (r, true)
            }
        }
    }

    fn ensure_var_in_reg(
        &mut self,
        var: &Var,
        out: &mut Vec<String>,
        note_user: &mut dyn FnMut(&str),
        prefer_reg: Option<Register>,
    ) -> Register {
        if let Some(&r) = self.var_to_reg.get(var) {
            self.temp_busy.insert(r);
            return r;
        }

        let r = if let Some(pref) = prefer_reg {
            if !self.reg_to_var.contains_key(&pref) && !self.temp_busy.contains(&pref) {
                self.temp_busy.insert(pref);
                pref
            } else {
                self.allocate_reg(out)
            }
        } else {
            self.allocate_reg(out)
        };

        if var.is_reg_allocated() {
            if let Some(&slot) = self.spilled.get(var) {
                let offset = -(self.max_slots as i32) + slot as i32 - 1;
                out.push(format!("  load {},{}[{}]", r, offset, Register::STACK_PTR));
            }
        } else {
            note_user(&var.name);
            out.push(format!("  load {},{}[{}]", r, var.name, Register::ZERO_REG));
        }

        self.bind_var_to_reg(var.clone(), r);
        self.temp_busy.insert(r);
        r
    }

    fn get_var_reg(&self, var: &Var) -> Option<Register> {
        self.var_to_reg.get(var).copied()
    }

    fn free_reg(&mut self, r: Register) {
        if let Some(var) = self.reg_to_var.remove(&r) {
            self.var_to_reg.remove(&var);
            self.dirty.remove(&var);
        }
    }

    fn spill_caller_saved(&mut self, out: &mut Vec<String>) {
        let regs_to_spill: Vec<Register> = self
            .reg_to_var
            .iter()
            .filter(|&(&r, ref var)| {
                if !Register::CALLER_SAVED.contains(&r) {
                    return false;
                }
                // Skip temp_busy registers only if the variable is dead.
                // Temp_busy regs hold values still needed by the current
                // instruction (e.g. evaluated call arguments). Dead variables
                // in temp_busy regs don't need saving (their value won't be
                // used after this point). Live variables in temp_busy regs
                // must still be spilled for correctness.
                if self.temp_busy.contains(&r) && self.is_dead(var) {
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

    fn flush_all(&mut self, out: &mut Vec<String>) {
        let regs: Vec<Register> = self.reg_to_var.keys().copied().collect();
        for r in regs {
            self.spill_reg(r, out);
        }
    }

    fn flush_globals(&mut self, out: &mut Vec<String>) {
        let regs: Vec<Register> = self
            .reg_to_var
            .iter()
            .filter(|(_, v)| v.kind == crate::ir::VarKind::Global)
            .map(|(&r, _)| r)
            .collect();
        for r in regs {
            self.spill_reg(r, out);
        }
    }

    fn begin_region(&mut self) {
        self.var_to_reg.clear();
        self.reg_to_var.clear();
        self.dirty.clear();
        self.spilled.clear();
        self.next_slot = 0;
        self.max_slots = 0;
        self.liveness = None;
        self.current_idx = 0;
    }

    fn get_max_slots(&self) -> usize {
        self.max_slots
    }

    fn is_var_dead(&self, var: &Var) -> bool {
        self.is_dead(var)
    }

    // --- Advanced methods ---

    fn flush_dirty(&mut self, out: &mut Vec<String>) {
        let dirty_entries: Vec<(Register, Var)> = self
            .reg_to_var
            .iter()
            .filter(|(_, var)| self.dirty.contains(var))
            .map(|(&r, v)| (r, v.clone()))
            .collect();

        for (reg, var) in dirty_entries {
            // Skip writing dead variables
            if !self.is_dead(&var) {
                self.write_back(reg, &var, out);
            }
            self.dirty.remove(&var);
        }
    }

    fn clear_bindings(&mut self) {
        self.var_to_reg.clear();
        self.reg_to_var.clear();
        self.dirty.clear();
    }

    fn prepare_def_reg(
        &mut self,
        var: &Var,
        out: &mut Vec<String>,
        _note_user: &mut dyn FnMut(&str),
    ) -> Register {
        // If var is already in a register, reuse it (no load needed)
        if let Some(&r) = self.var_to_reg.get(var) {
            self.temp_busy.insert(r);
            return r;
        }
        // Otherwise allocate a fresh register - do NOT load old value
        self.allocate_reg(out)
    }

    fn set_liveness(&mut self, info: LivenessInfo) {
        self.liveness = Some(info);
    }

    fn set_current_instruction(&mut self, idx: usize) {
        self.current_idx = idx;
    }
}
