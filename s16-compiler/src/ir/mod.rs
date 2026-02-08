//! Intermediate Representation (IR) module.
//!
//! This module contains the IR definitions, the AST-to-IR generator,
//! source mapping logic, and IR optimization passes.

pub mod ir;
pub use ir::*;
pub mod source_map;
pub use source_map::*;
pub mod ir_generator;
pub mod opt;
pub mod symbol_table;
pub mod error_utils;
pub mod ast;
