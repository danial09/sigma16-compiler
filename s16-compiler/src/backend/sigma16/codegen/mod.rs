//! Sigma16 Code Generation.
//!
//! This module lowers IR instructions into Sigma16 assembly code.
//! It uses a register allocator to manage the mapping of variables to registers.

pub mod emitter;
pub mod instr_lowering;
pub mod item;

use super::abi::Register;
use super::liveness;
use super::regalloc::{AdvancedRegAllocator, AllocatorKind, GreedyRegAllocator, RegAllocator};
use crate::ir::{Instr, ProgramIR, Rhs, Value, Var};
/// Codegen orchestrates the emission of assembly from IR.
pub use emitter::Codegen;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone)]
pub struct Sigma16Asm {
    pub lines: Vec<String>,
    pub asm_ir_mapping: Vec<Option<usize>>,
}

impl Sigma16Asm {
    pub fn join(&self) -> String {
        self.lines.join("\n")
    }
}

pub fn compile_ir_to_sigma16(ir: &ProgramIR) -> String {
    let asm = compile_ir_to_sigma16_mapped(ir);
    asm.join()
}

pub fn compile_ir_to_sigma16_mapped(ir: &ProgramIR) -> Sigma16Asm {
    compile_ir_to_sigma16_with_allocator_mapped(AllocatorKind::Advanced, ir)
}

pub fn compile_ir_to_sigma16_with_allocator(kind: AllocatorKind, ir: &ProgramIR) -> String {
    let asm = compile_ir_to_sigma16_with_allocator_mapped(kind, ir);
    asm.join()
}

pub fn compile_ir_to_sigma16_with_allocator_mapped(
    kind: AllocatorKind,
    ir: &ProgramIR,
) -> Sigma16Asm {
    let (reg, advanced): (Box<dyn RegAllocator>, bool) = match kind {
        AllocatorKind::Basic => (Box::new(GreedyRegAllocator::new()), false),
        AllocatorKind::Advanced => (Box::new(AdvancedRegAllocator::new()), true),
    };
    let mut cg = Codegen::with_regalloc(reg, advanced);
    cg.emit_program(ir);
    cg.finish_codegen()
}

/// Find the end of a function starting at `start` in the instruction list.
fn find_func_end(instrs: &[Instr], start: usize) -> usize {
    let mut j = start + 1;
    while j < instrs.len() {
        if matches!(&instrs[j], Instr::FuncEnd { .. }) {
            return j + 1;
        }
        j += 1;
    }
    instrs.len()
}

/// Find the extent of top-level code starting at `start`.
/// Stops at the next FuncStart or end of instructions.
fn find_toplevel_end(instrs: &[Instr], start: usize) -> usize {
    let mut j = start;
    while j < instrs.len() {
        if matches!(&instrs[j], Instr::FuncStart { .. }) {
            return j;
        }
        j += 1;
    }
    instrs.len()
}

// ============================================================================
// Function variable analysis for register-resident mode
// ============================================================================

/// Collect all distinct register-allocated (local/temp) variables used in a
/// function region `[start, end)`.
fn collect_function_vars(instrs: &[Instr], start: usize, end: usize) -> HashSet<Var> {
    let mut vars = HashSet::new();

    fn add_val(vars: &mut HashSet<Var>, v: &Value) {
        if let Value::Var(var) = v {
            if var.is_reg_allocated() {
                vars.insert(var.clone());
            }
        }
    }

    for instr in &instrs[start..end] {
        match instr {
            Instr::FuncStart { params, .. } => {
                for p in params {
                    vars.insert(Var::local(p.clone()));
                }
            }
            Instr::Assign { dst, src } => {
                if dst.is_reg_allocated() {
                    vars.insert(dst.clone());
                }
                match src {
                    Rhs::Value(v) => add_val(&mut vars, v),
                    Rhs::Binary { left, right, .. } => {
                        add_val(&mut vars, left);
                        add_val(&mut vars, right);
                    }
                }
            }
            Instr::IfCmpGoto { left, right, .. } => {
                add_val(&mut vars, left);
                add_val(&mut vars, right);
            }
            Instr::Call { args, ret, .. } => {
                for a in args {
                    add_val(&mut vars, a);
                }
                if let Some(r) = ret {
                    if r.is_reg_allocated() {
                        vars.insert(r.clone());
                    }
                }
            }
            Instr::Return { value: Some(v) } => add_val(&mut vars, v),
            Instr::Load { dst, addr } => {
                if dst.is_reg_allocated() {
                    vars.insert(dst.clone());
                }
                add_val(&mut vars, addr);
            }
            Instr::Store { addr, src } => {
                add_val(&mut vars, addr);
                add_val(&mut vars, src);
            }
            Instr::ArrayLoad { dst, index, .. } => {
                if dst.is_reg_allocated() {
                    vars.insert(dst.clone());
                }
                add_val(&mut vars, index);
            }
            Instr::ArrayStore { index, src, .. } => {
                add_val(&mut vars, index);
                add_val(&mut vars, src);
            }
            _ => {}
        }
    }

    vars
}

/// Check if a function region contains any call instructions (non-leaf).
fn is_leaf_function(instrs: &[Instr], start: usize, end: usize) -> bool {
    !instrs[start..end]
        .iter()
        .any(|i| matches!(i, Instr::Call { .. }))
}

/// Number of GP registers reserved for temporaries (immediates, addresses).
const TEMP_REGISTER_RESERVE: usize = 2;

/// Compute fixed variable-to-register assignments for register-resident mode.
///
/// Returns `None` if the variables don't fit in available registers.
fn compute_fixed_assignments(
    vars: &HashSet<Var>,
    params: &[String],
    is_leaf: bool,
) -> Option<HashMap<Var, Register>> {
    let budget = Register::GP_REGS.len() - TEMP_REGISTER_RESERVE; // 10

    if vars.len() > budget {
        return None; // Too many variables for register-resident mode
    }

    let mut assignments = HashMap::new();
    let mut used = HashSet::new();

    // Step 1: Parameters keep their natural registers (R1-R8).
    for (i, p) in params.iter().enumerate().take(8) {
        let var = Var::local(p.clone());
        if vars.contains(&var) {
            let reg = Register::PARAM_REGS[i];
            assignments.insert(var, reg);
            used.insert(reg);
        }
    }

    // Step 2: Build register order for remaining variables.
    // For leaf functions: use remaining GP registers in order (caller-saved
    // first naturally since GP_REGS starts with R1).
    // For non-leaf functions: prefer callee-saved (R9-R12) so variables
    // survive across calls without save/restore.
    let reg_order: Vec<Register> = if is_leaf {
        Register::GP_REGS
            .iter()
            .filter(|r| !used.contains(r))
            .copied()
            .collect()
    } else {
        let mut order: Vec<Register> = Register::CALLEE_SAVED
            .iter()
            .filter(|r| !used.contains(r))
            .copied()
            .collect();
        order.extend(
            Register::CALLER_SAVED
                .iter()
                .filter(|r| !used.contains(r))
                .copied(),
        );
        order
    };

    let mut reg_iter = reg_order.into_iter();
    for var in vars {
        if assignments.contains_key(var) {
            continue;
        }
        match reg_iter.next() {
            Some(reg) => {
                assignments.insert(var.clone(), reg);
            }
            None => return None, // Ran out of registers
        }
    }

    Some(assignments)
}

impl Codegen {
    pub fn emit_program(&mut self, ir: &ProgramIR) {
        self.arrays = ir.arrays.clone();
        self.emitted_header = true;

        // Initialize register allocation for top-level code
        self.reg.begin_region();

        // Set liveness for initial top-level region if in advanced mode
        if self.advanced_mode {
            let end = find_toplevel_end(&ir.instrs, 0);
            if end > 0 {
                let info = liveness::compute_liveness(&ir.instrs, 0, end);
                self.reg.set_liveness(info);
            }
        }

        for (ir_index, instr) in ir.instrs.iter().enumerate() {
            self.current_ir = Some(ir_index);
            self.reg.set_current_instruction(ir_index);

            // When entering a function, compute and set its liveness info
            if let Instr::FuncStart { name: _, params } = instr {
                if self.advanced_mode {
                    let end = find_func_end(&ir.instrs, ir_index);

                    // Analyse the function for register-resident eligibility:
                    // if all variables fit in registers, assign fixed registers
                    // so that no spills are needed at branch points.
                    let func_vars = collect_function_vars(&ir.instrs, ir_index, end);
                    let leaf = is_leaf_function(&ir.instrs, ir_index, end);
                    let fixed = compute_fixed_assignments(&func_vars, params, leaf);

                    let func_liveness = liveness::compute_liveness(&ir.instrs, ir_index, end);
                    // Note: start_function() calls begin_region() which clears liveness,
                    // so we set it AFTER emit_instr processes FuncStart.
                    self.emit_instr(instr);
                    self.reg.set_liveness(func_liveness);

                    // Enable register-resident mode if the function qualifies.
                    if let Some(assignments) = fixed {
                        self.reg.set_register_resident(assignments);
                    }

                    self.reg.clear_temp_busy();
                    continue;
                }
            }

            // When exiting a function, set liveness for the next top-level region
            if let Instr::FuncEnd { .. } = instr {
                if self.advanced_mode {
                    self.emit_instr(instr);
                    self.reg.clear_temp_busy();
                    // Set liveness for subsequent top-level code
                    let next = ir_index + 1;
                    if next < ir.instrs.len() {
                        let end = find_toplevel_end(&ir.instrs, next);
                        if end > next {
                            let info = liveness::compute_liveness(&ir.instrs, next, end);
                            self.reg.set_liveness(info);
                        }
                    }
                    continue;
                }
            }

            self.emit_instr(instr);
            self.reg.clear_temp_busy();
        }

        // Flush any remaining dirty globals while current_ir still points to
        // the last instruction, so the generated store instructions receive
        // the correct IR-to-source mapping.
        let mut out = Vec::new();
        self.reg.flush_all(&mut out);
        self.drain_regalloc(out);

        self.current_ir = None;
    }
}
