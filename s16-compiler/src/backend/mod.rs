//! Sigma16 backend — lowers IR to Sigma16 assembly.
//!
//! Module layout:
//! - `abi`         — register definitions and ABI constants
//! - `instruction` — typed machine instructions and assembly output items
//! - `liveness`    — dataflow-based liveness analysis
//! - `regalloc`    — register allocation trait and implementations
//! - `codegen`     — code generation driver (IR → assembly)
//! - `optimize`    — assembly-level optimization passes

pub mod abi;
pub mod instruction;
mod liveness;
mod optimize;
pub mod regalloc;
mod codegen;

// Re-export the public API at `backend::` level.
pub use codegen::Sigma16Asm;
pub use regalloc::AllocatorKind;

use crate::ir::ProgramIR;
use codegen::Codegen;
use regalloc::{AdvancedRegAllocator, AllocatorKind as AK, GreedyRegAllocator, RegAllocator};

/// Compile IR to Sigma16 assembly text (using the advanced allocator).
pub fn compile_ir_to_sigma16(ir: &ProgramIR) -> String {
    compile_ir_to_sigma16_mapped(ir).join()
}

/// Compile IR to Sigma16 assembly with source mapping (advanced allocator).
pub fn compile_ir_to_sigma16_mapped(ir: &ProgramIR) -> Sigma16Asm {
    compile_ir_to_sigma16_with_allocator_mapped(AK::Advanced, ir)
}

/// Compile IR to Sigma16 assembly text with a specific allocator.
pub fn compile_ir_to_sigma16_with_allocator(kind: AllocatorKind, ir: &ProgramIR) -> String {
    compile_ir_to_sigma16_with_allocator_mapped(kind, ir).join()
}

/// Compile IR to Sigma16 assembly with source mapping and a specific allocator.
pub fn compile_ir_to_sigma16_with_allocator_mapped(
    kind: AllocatorKind,
    ir: &ProgramIR,
) -> Sigma16Asm {
    let (reg, advanced): (Box<dyn RegAllocator>, bool) = match kind {
        AK::Basic => (Box::new(GreedyRegAllocator::new()), false),
        AK::Advanced => (Box::new(AdvancedRegAllocator::new()), true),
    };
    let mut cg = Codegen::with_regalloc(reg, advanced);
    cg.emit_program(ir);
    cg.finish_codegen()
}
