pub mod frontend;
pub mod ast;
pub mod ir;
pub mod backend;

use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompileError {
    #[error("Lexical error: {0}")]
    Lexical(#[from] frontend::lexer::LexicalError),

    #[error("Parse error at position {location}: {message}")]
    Parse {
        location: usize,
        message: String,
    },

    #[error("Parse error: {0}")]
    ParseGeneric(String),
}

pub fn compile_to_ir(source: &str) -> Result<ir::ProgramIR, CompileError> {
    let parsed = ast::ast_ir::parse_to_ast(source)?;
    let mut ir = ir::ir_generator::lower(&parsed.program);

    // Run optimization passes
    ir::opt::optimize(&mut ir);

    // Initialize source index and register AST spans
    ir.source_map.init_source_index(source);
    for s in parsed.spans {
        ir.source_map.add_ast_span(s.id, s.start, s.end, s.kind);
    }
    // Populate mapping spans from the registered AST spans for robust lookups
    ir.source_map.finalize();
    Ok(ir)
}

/// Compile source text directly to Sigma16 assembly.
pub fn compile_to_sigma16(source: &str) -> Result<String, CompileError> {
    let ir = compile_to_ir(source)?;
    Ok(backend::sigma16::compile_ir_to_sigma16(&ir))
}
