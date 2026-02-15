pub mod frontend;
// pub mod ast;
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

    #[error("SemanticError:{kind} ({line}:{col}) - {message}")]
    Semantic {
        kind: SemanticErrorKind,
        location: SourceLocation,
        line: usize,
        col: usize,
        message: String,
    },
}

#[derive(Debug, Clone)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    pub offset: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SemanticErrorKind {
    VariableRedefinition,
    ArrayRedefinition,
    FunctionRedefinition,
    ParameterRedefinition,
    UndefinedVariable,
    UndefinedFunction,
    UndefinedArray,
    VariableUsedAsArray,
    ArrayUsedAsVariable,
    FunctionUsedAsVariable,
    ArrayUsedAsFunction,
    ReturnOutsideFunction,
    ArgumentCountMismatch,
}

impl std::fmt::Display for SemanticErrorKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SemanticErrorKind::VariableRedefinition => write!(f, "VariableRedefinition"),
            SemanticErrorKind::ArrayRedefinition => write!(f, "ArrayRedefinition"),
            SemanticErrorKind::FunctionRedefinition => write!(f, "FunctionRedefinition"),
            SemanticErrorKind::ParameterRedefinition => write!(f, "ParameterRedefinition"),
            SemanticErrorKind::UndefinedVariable => write!(f, "UndefinedVariable"),
            SemanticErrorKind::UndefinedFunction => write!(f, "UndefinedFunction"),
            SemanticErrorKind::UndefinedArray => write!(f, "UndefinedArray"),
            SemanticErrorKind::VariableUsedAsArray => write!(f, "VariableUsedAsArray"),
            SemanticErrorKind::ArrayUsedAsVariable => write!(f, "ArrayUsedAsVariable"),
            SemanticErrorKind::FunctionUsedAsVariable => write!(f, "FunctionUsedAsVariable"),
            SemanticErrorKind::ArrayUsedAsFunction => write!(f, "ArrayUsedAsFunction"),
            SemanticErrorKind::ReturnOutsideFunction => write!(f, "ReturnOutsideFunction"),
            SemanticErrorKind::ArgumentCountMismatch => write!(f, "ArgumentCountMismatch"),
        }
    }
}

pub fn compile_to_ir(source: &str) -> Result<ir::ProgramIR, CompileError> {
    let parsed = ir::ast::parse_to_ast(source)?;

    // Lower with semantic validation
    let (mut ir_prog, ast_spans) = ir::ir_generator::lower(&parsed.program, parsed.spans, source)?;

    // Run optimization passes
    ir::opt::optimize(&mut ir_prog);

    // Initialize source index and register AST spans
    ir_prog.source_map.init_source_index(source);
    for s in ast_spans {
        ir_prog.source_map.add_ast_span(s.id, s.start, s.end, s.kind);
    }
    // Populate mapping spans from the registered AST spans for robust lookups
    ir_prog.source_map.finalize();

    Ok(ir_prog)
}

/// Compile source text directly to Sigma16 assembly.
pub fn compile_to_sigma16(source: &str) -> Result<String, CompileError> {
    let ir = compile_to_ir(source)?;
    Ok(backend::sigma16::compile_ir_to_sigma16(&ir))
}
