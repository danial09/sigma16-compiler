//! Sigma16 backend module.
//!
//! This module was split into submodules to keep responsibilities separated
//! and prepare the ground for a more sophisticated register allocator.
//!
//! Layout:
//! - regalloc.rs: register allocation interfaces and basic implementations
//! - codegen.rs: lowering from IR to Sigma16 assembly (uses a RegAllocator)

mod regalloc;
mod codegen;

pub use codegen::{compile_ir_to_sigma16, compile_ir_to_sigma16_with_allocator};
pub use regalloc::AllocatorKind;
