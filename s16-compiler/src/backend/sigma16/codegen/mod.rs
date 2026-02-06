//! Sigma16 Code Generation.
//!
//! This module lowers IR instructions into Sigma16 assembly code.
//! It uses a register allocator to manage the mapping of variables to registers.

pub mod emitter;
pub mod instr_lowering;
pub mod item;

/// Codegen orchestrates the emission of assembly from IR.
pub use emitter::Codegen;
use crate::ir::ProgramIR;
use super::regalloc::{AllocatorKind, GreedyRegAllocator, RegAllocator};
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
    let reg: Box<dyn RegAllocator> = match kind {
        AllocatorKind::Basic | AllocatorKind::Advanced => Box::new(GreedyRegAllocator::new()),
    };
    let mut cg = Codegen::with_regalloc(reg);
    cg.emit_program(ir);
    cg.finish_codegen()
}

use item::AsmItem;

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
        for (ir_index, instr) in ir.instrs.iter().enumerate() {
            self.current_ir = Some(ir_index);
            self.emit_instr(instr);
            self.reg.clear_temp_busy();
        }
        self.current_ir = None;
    }
}
