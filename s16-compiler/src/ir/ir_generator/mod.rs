//! AST to IR lowering module.
//!
//! This module handles the translation of the high-level AST into
//! a linear intermediate representation (IR).

pub mod context;
pub mod stmt;
pub mod expr;

use crate::ast::ast_ir::{Program, AstSpanRecord};
use crate::ir::ProgramIR;
use crate::CompileError;

/// The main Gen struct that orchestrates the lowering process.
pub use context::Gen;

/// Entry point for lowering an AST Program to ProgramIR.
pub fn lower(program: &Program, spans: Vec<AstSpanRecord>) -> Result<(ProgramIR, Vec<AstSpanRecord>), CompileError> {
    let mut g = Gen::new(spans);
    g.lower_program(program)?;
    Ok(g.finish())
}
