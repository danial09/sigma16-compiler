//! Sigma16 Code Generation.
//!
//! This module lowers IR instructions into Sigma16 assembly code.
//! It uses a register allocator to manage the mapping of variables to registers.

pub mod emitter;
pub mod instr_lowering;
pub mod item;

/// Codegen orchestrates the emission of assembly from IR.
pub use emitter::Codegen;
use crate::ir::{Instr, ProgramIR};
use super::regalloc::{AllocatorKind, AdvancedRegAllocator, GreedyRegAllocator, RegAllocator};
use super::liveness;
use super::abi::Register;

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

use item::AsmItem;

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

impl Codegen {
    pub fn emit_program(&mut self, ir: &ProgramIR) {
        self.arrays = ir.arrays.clone();
        if !self.emitted_header {
            self.out.push(AsmItem::Instruction {
                text: format!("  lea {},stack[{}]", Register::STACK_PTR, Register::ZERO_REG),
                ir_map: None,
            });
            self.out.push(AsmItem::Instruction {
                text: "  jump prog_start".to_string(),
                ir_map: None,
            });
            self.emitted_header = true;
        }

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
            if let Instr::FuncStart { .. } = instr {
                if self.advanced_mode {
                    let end = find_func_end(&ir.instrs, ir_index);
                    let func_liveness =
                        liveness::compute_liveness(&ir.instrs, ir_index, end);
                    // Note: start_function() calls begin_region() which clears liveness,
                    // so we set it AFTER emit_instr processes FuncStart.
                    self.emit_instr(instr);
                    self.reg.set_liveness(func_liveness);
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
        self.current_ir = None;
    }
}
