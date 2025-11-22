// compiler/src/ir/mod.rs

// Keep types reachable as `crate::ir::*`
pub mod ir;
pub use ir::*;

// The IR generator lives in this module as well
pub mod ir_generator;
