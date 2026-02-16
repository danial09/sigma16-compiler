//! Register allocation interfaces and shared infrastructure.
//!
//! This module defines:
//! - [`RegAllocator`] — the trait that all register allocators implement.
//! - [`AllocatorKind`] — enum for selecting an allocator at compile time.
//! - [`AllocState`] — shared bookkeeping state embedded by both allocators.

mod advanced;
mod greedy;

pub use advanced::AdvancedRegAllocator;
pub use greedy::GreedyRegAllocator;

use super::abi::Register;
use super::instruction::{AnnotatedInstr, S16Instr};
use super::liveness::LivenessInfo;
use crate::ir::{Value, Var};
use std::collections::{HashMap, HashSet};

// ============================================================================
// Allocator selection
// ============================================================================

/// Which register allocator implementation to use.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AllocatorKind {
    Basic,
    Advanced,
}

// ============================================================================
// Allocator trait
// ============================================================================

/// Abstract register allocation interface.
///
/// Both the basic (greedy) and advanced (liveness-aware) allocators implement
/// this trait.  The codegen layer interacts exclusively through this interface.
pub trait RegAllocator {
    // ── Core operations ─────────────────────────────────────────────────
    fn bind_var_to_reg(&mut self, var: Var, reg: Register);
    fn mark_dirty(&mut self, var: &Var);
    fn clear_temp_busy(&mut self);
    fn spill_reg(&mut self, reg: Register, out: &mut Vec<AnnotatedInstr>);
    fn allocate_reg(&mut self, out: &mut Vec<AnnotatedInstr>) -> Register;
    fn ensure_in_reg(
        &mut self,
        v: &Value,
        out: &mut Vec<AnnotatedInstr>,
        note_user: &mut dyn FnMut(&str),
    ) -> (Register, bool);
    fn ensure_var_in_reg(
        &mut self,
        var: &Var,
        out: &mut Vec<AnnotatedInstr>,
        note_user: &mut dyn FnMut(&str),
        prefer_reg: Option<Register>,
    ) -> Register;
    fn get_var_reg(&self, var: &Var) -> Option<Register>;
    fn free_reg(&mut self, r: Register);
    fn spill_caller_saved(&mut self, out: &mut Vec<AnnotatedInstr>);
    fn flush_all(&mut self, out: &mut Vec<AnnotatedInstr>);
    fn flush_globals(&mut self, out: &mut Vec<AnnotatedInstr>);
    fn begin_region(&mut self);
    fn get_max_slots(&self) -> usize;

    // ── Extended methods with defaults (for basic allocator compat) ──────

    /// Write only dirty variables to their home locations; keep register bindings.
    fn flush_dirty(&mut self, out: &mut Vec<AnnotatedInstr>) {
        self.flush_all(out);
    }

    /// Drop all register-to-variable bindings without writing anything.
    fn clear_bindings(&mut self) {}

    /// Allocate a register for a variable being *defined* (assigned to).
    /// Unlike `ensure_var_in_reg`, this does NOT load the old value.
    fn prepare_def_reg(
        &mut self,
        var: &Var,
        out: &mut Vec<AnnotatedInstr>,
        note_user: &mut dyn FnMut(&str),
    ) -> Register {
        self.ensure_var_in_reg(var, out, note_user, None)
    }

    /// Check if a variable is dead (no future uses) at the current instruction.
    fn is_var_dead(&self, _var: &Var) -> bool {
        false
    }

    /// Provide liveness information for the current region.
    fn set_liveness(&mut self, _info: LivenessInfo) {}

    /// Update the current instruction index (global).
    fn set_current_instruction(&mut self, _idx: usize) {}

    /// Enable register-resident mode with fixed variable-to-register assignments.
    fn set_register_resident(&mut self, _assignments: HashMap<Var, Register>) {}

    /// Whether the allocator is currently in register-resident mode.
    #[allow(dead_code)]
    fn is_register_resident(&self) -> bool {
        false
    }

    /// Return the fixed register assignment for a variable, if any.
    fn get_fixed_reg(&self, _var: &Var) -> Option<Register> {
        None
    }
}

// ============================================================================
// Shared allocator state
// ============================================================================

/// Common bookkeeping state shared by both allocator implementations.
///
/// Each allocator embeds an `AllocState` and delegates trivial operations
/// to it, keeping allocator-specific logic in its own file.
pub(crate) struct AllocState {
    pub var_to_reg: HashMap<Var, Register>,
    pub reg_to_var: HashMap<Register, Var>,
    pub dirty: HashSet<Var>,
    pub temp_busy: HashSet<Register>,
    pub spilled: HashMap<Var, usize>,
    pub next_slot: usize,
    pub max_slots: usize,
}

impl AllocState {
    pub fn new() -> Self {
        Self {
            var_to_reg: HashMap::new(),
            reg_to_var: HashMap::new(),
            dirty: HashSet::new(),
            temp_busy: HashSet::new(),
            spilled: HashMap::new(),
            next_slot: 0,
            max_slots: 0,
        }
    }

    /// Reset all state for a new allocation region.
    pub fn reset(&mut self) {
        self.var_to_reg.clear();
        self.reg_to_var.clear();
        self.dirty.clear();
        self.temp_busy.clear();
        self.spilled.clear();
        self.next_slot = 0;
        self.max_slots = 0;
    }

    /// Bind a variable to a register, evicting any prior occupant.
    pub fn bind(&mut self, var: Var, reg: Register) {
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

    pub fn mark_dirty(&mut self, var: &Var) {
        if self.var_to_reg.contains_key(var) {
            self.dirty.insert(var.clone());
        }
    }

    pub fn clear_temp_busy(&mut self) {
        self.temp_busy.clear();
    }

    pub fn get_var_reg(&self, var: &Var) -> Option<Register> {
        self.var_to_reg.get(var).copied()
    }

    /// Remove a variable-register binding entirely (no writeback).
    pub fn free_reg(&mut self, r: Register) {
        if let Some(var) = self.reg_to_var.remove(&r) {
            self.var_to_reg.remove(&var);
            self.dirty.remove(&var);
        }
    }

    /// Allocate (or reuse) a spill slot for a variable, returning the slot number.
    pub fn alloc_slot(&mut self, var: &Var) -> usize {
        *self.spilled.entry(var.clone()).or_insert_with(|| {
            self.next_slot += 1;
            self.max_slots = self.max_slots.max(self.next_slot);
            self.next_slot
        })
    }

    /// Store a variable from its register to its home location (stack slot or global label).
    pub fn write_back(&mut self, reg: Register, var: &Var, out: &mut Vec<AnnotatedInstr>) {
        if var.is_reg_allocated() {
            let slot = self.alloc_slot(var);
            let offset = -(slot as i64);
            emit_c(
                out,
                S16Instr::store_disp(reg, offset, Register::STACK_PTR),
                format!("spill {}", var.name),
            );
        } else {
            emit_c(
                out,
                S16Instr::store_label(reg, &var.name),
                format!("writeback {}", var.name),
            );
        }
    }

    /// Load a variable from its home location into a register.
    pub fn load_from_home(
        &self,
        reg: Register,
        var: &Var,
        out: &mut Vec<AnnotatedInstr>,
        note_user: &mut dyn FnMut(&str),
    ) {
        if var.is_reg_allocated() {
            if let Some(&slot) = self.spilled.get(var) {
                let offset = -(slot as i64);
                emit_c(
                    out,
                    S16Instr::load_disp(reg, offset, Register::STACK_PTR),
                    format!("restore {}", var.name),
                );
            }
        } else {
            note_user(&var.name);
            emit_c(
                out,
                S16Instr::load_label(reg, &var.name),
                format!("load {}", var.name),
            );
        }
    }

}

// ============================================================================
// Helpers
// ============================================================================

/// Push an instruction with a comment.
#[inline]
pub(crate) fn emit_c(out: &mut Vec<AnnotatedInstr>, instr: S16Instr, comment: impl Into<String>) {
    out.push((instr, Some(comment.into())));
}
