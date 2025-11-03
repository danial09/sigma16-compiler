pub mod lexer;
pub mod ast;
mod ir;
pub mod ir_generator;

use thiserror::Error;

lalrpop_util::lalrpop_mod!(pub grammar);

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Lexical error: {0}")]
    Lexical(#[from] lexer::LexicalError),

    #[error("Parse error at position {location}: {message}")]
    Parse {
        location: usize,
        message: String,
    },

    #[error("Parse error: {0}")]
    ParseGeneric(String),
}

pub fn compile_to_ir(source: &str) -> Result<ir::ProgramIR, CompileError> {
    let ast = ast::parse_to_ast(source)?;
    Ok(ir_generator::lower(&ast))
}
