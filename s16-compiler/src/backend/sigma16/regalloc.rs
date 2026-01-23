use crate::ir::{Var, Value};
use std::collections::{HashMap, HashSet};
use super::abi::Register;

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
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocatorKind {
    Basic,
    Advanced,
}

pub struct GreedyRegAllocator {
    // Current mapping of variables to registers
    var_to_reg: HashMap<Var,Register>,
    // Current mapping of registers to variables
    reg_to_var: HashMap<Register, Var>,
    
    // Variables that have been modified in register and need write-back
    dirty: HashSet<Var>,
    
    // Registers that are busy for the current instruction only
    temp_busy: HashSet<Register>,
    
    // Stack slots for spilled Local/Temp variables
    // Sigma16 stack grows upward, so slots are at negative offsets from R13
    // slot 1 = -total_frame+1[R13], slot 2 = -total_frame+2[R13], etc.
    spilled: HashMap<Var, usize>, // var -> slot (1-based)
    next_slot: usize,
    max_slots: usize,
    
    // Linear allocation state
    next_reg_idx: usize,
    
    // Used for LRU spilling
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
        // Prefer caller-saved registers for spilling if possible
        for &r in &self.usage_order {
            if Register::CALLER_SAVED.contains(&r) && !self.temp_busy.contains(&r) {
                return r;
            }
        }
        // Fallback to the first one used that is not temp busy
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
        // If var was elsewhere, unbind it
        if let Some(old_reg) = self.var_to_reg.remove(&var) {
            self.reg_to_var.remove(&old_reg);
        }
        // If reg was used by another var, unbind that var
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
                // Local or Temp: always spill to stack if we want to preserve it
                let slot = *self.spilled.entry(var.clone()).or_insert_with(|| {
                    self.next_slot += 1;
                    self.max_slots = self.max_slots.max(self.next_slot);
                    self.next_slot
                });
                // Stack grows upward: R13 points above frame, so use negative offset
                // slot 1 is at -(max_slots)[R13], slot 2 at -(max_slots-1)[R13], etc.
                let offset = -(self.max_slots as i32) + slot as i32 - 1;
                out.push(format!("  store {},{}[{}]",reg,offset,Register::STACK_PTR));
            } else if is_dirty {
                // Global: only spill back to memory if dirty
                out.push(format!("  store {},{}[{}]", reg, var.name,Register::ZERO_REG));
            }
        }
        // Remove from usage order
        if let Some(pos) = self.usage_order.iter().position(|&r| r == reg) {
            self.usage_order.remove(pos);
        }
    }

    fn allocate_reg(&mut self, out: &mut Vec<String>) -> Register {
        // 1. Try to find a register that is not bound to any variable and not temp busy
        for &r in &Register::GP_REGS {
            if !self.reg_to_var.contains_key(&r) && !self.temp_busy.contains(&r) {
                self.touch(r);
                self.temp_busy.insert(r);
                return r;
            }
        }
        
        // 2. No free registers, spill one
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
                out.push(format!("  lea {},{}[{}]", r, i,Register::ZERO_REG));
                (r, true) // Temp register, can be freed
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
                            // Stack grows upward: R13 points above frame, so use negative offset
                            let offset = -(self.max_slots as i32) + slot as i32 - 1;
                            out.push(format!("  load {},{}[{}]",r,offset,Register::STACK_PTR));
                        }
                    } else {
                        note_user(&var.name);
                        out.push(format!("  load {},{}[{}]", r, var.name,Register::ZERO_REG));
                    }
                    self.bind_var_to_reg(var.clone(), r);
                    self.temp_busy.insert(r);
                    (r, false)
                }
            }
            Value::AddrOf(name) => {
                note_user(name);
                let r = self.allocate_reg(out);
                out.push(format!("  lea {},{}[{}]", r, name,Register::ZERO_REG));
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
        // If variable is already in a register, return it
        if let Some(&r) = self.var_to_reg.get(var) {
            self.touch(r);
            self.temp_busy.insert(r);
            return r;
        }

        // If a preferred register is specified and available, use it
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

        // Load variable into register
        if var.is_reg_allocated() {
            if let Some(&slot) = self.spilled.get(var) {
                // Stack grows upward: R13 points above frame, so use negative offset
                let offset = -(self.max_slots as i32) + slot as i32 - 1;
                out.push(format!("  load {}, {}[{}]",r,offset,Register::STACK_PTR));
            }
        } else {
            note_user(&var.name);
            out.push(format!("  load {},{}[{}]", r, var.name,Register::ZERO_REG));
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
        let regs_to_spill: Vec<Register> = self.reg_to_var.keys()
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
        let regs: Vec<Register> = self.reg_to_var.iter()
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
