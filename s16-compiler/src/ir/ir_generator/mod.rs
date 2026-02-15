//! AST to IR lowering module.
//!
//! This module handles the translation of the high-level AST into
//! a linear intermediate representation (IR).

pub mod context;
pub mod expr;
pub mod stmt;

use crate::CompileError;
use crate::ir::ProgramIR;
use crate::ir::ast::{AstSpanRecord, Program};

/// The main Gen struct that orchestrates the lowering process.
pub use context::Gen;

/// Entry point for lowering an AST Program to ProgramIR.
pub fn lower(
    program: &Program,
    spans: Vec<AstSpanRecord>,
    source: &str,
) -> Result<(ProgramIR, Vec<AstSpanRecord>), CompileError> {
    let mut g = Gen::new(spans, source);
    g.lower_program(program)?;
    Ok(g.finish())
}
