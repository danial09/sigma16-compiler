use crate::{ast, grammar, lexer, CompileError};
use crate::ir::AstNodeId;

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Assign {
        id: AstNodeId,
        name: String,
        value: Expr,
    },
    If {
        id: AstNodeId,
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    While {
        id: AstNodeId,
        condition: Expr,
        body: Vec<Stmt>,
    },
    For {
        id: AstNodeId,
        var: String,
        from: Expr,
        to: Expr,
        body: Vec<Stmt>,
    }
}

impl Stmt {
    pub fn id(&self) -> AstNodeId {
        match self {
            Stmt::Assign { id, .. } => *id,
            Stmt::If { id, .. } => *id,
            Stmt::While { id, .. } => *id,
            Stmt::For { id, .. } => *id,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(AstNodeId, i64),
    Variable(AstNodeId, String),
    Binary {
        id: AstNodeId,
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        id: AstNodeId,
        op: UnOp,
        operand: Box<Expr>,
    },
}

impl Expr {
    pub fn id(&self) -> AstNodeId {
        match self {
            Expr::Number(id, _) => *id,
            Expr::Variable(id, _) => *id,
            Expr::Binary { id, .. } => *id,
            Expr::Unary { id, .. } => *id,
        }
    }
}

// ... existing BinOp, UnOp enums ...
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    Not,
}

/// Helper for generating unique AST node IDs during parsing
pub struct AstNodeIdGenerator {
    next_id: usize,
}

impl AstNodeIdGenerator {
    pub fn new() -> Self {
        Self { next_id: 0 }
    }

    pub fn next(&mut self) -> AstNodeId {
        let id = AstNodeId(self.next_id);
        self.next_id += 1;
        id
    }
}

pub fn parse_to_ast(source: &str) -> Result<ast::Program, CompileError> {
    use std::cell::RefCell;
    
    let lexer = lexer::lex_adapter(source);
    let id_gen = RefCell::new(AstNodeIdGenerator::new());
    
    grammar::ProgramParser::new()
        .parse(&id_gen, lexer)
        .map_err(|e| {
            use lalrpop_util::ParseError;

            match e {
                ParseError::InvalidToken { location } => {
                    CompileError::Parse {
                        location,
                        message: "Invalid token".to_string(),
                    }
                }
                ParseError::UnrecognizedEof { location, expected } => {
                    CompileError::Parse {
                        location,
                        message: format!(
                            "Unexpected end of file. Expected one of: {}",
                            expected.join(", ")
                        ),
                    }
                }
                ParseError::UnrecognizedToken { token: (start, tok, end), expected } => {
                    CompileError::Parse {
                        location: start,
                        message: format!(
                            "Unexpected token '{:?}' at position {}..{}. Expected one of: {}",
                            tok,
                            start,
                            end,
                            expected.join(", ")
                        ),
                    }
                }
                ParseError::ExtraToken { token: (start, tok, end) } => {
                    CompileError::Parse {
                        location: start,
                        message: format!("Extra token '{:?}' at position {}..{}", tok, start, end),
                    }
                }
                ParseError::User { error } => {
                    CompileError::ParseGeneric(format!("Lexical error: {}", error))
                }
            }
        })
}