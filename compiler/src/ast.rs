use crate::{ast, grammar, lexer, CompileError};

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Assign {
        name: String,
        value: Expr,
    },
    If {
        condition: Expr,
        then_branch: Vec<Stmt>,
        else_branch: Option<Vec<Stmt>>,
    },
    While {
        condition: Expr,
        body: Vec<Stmt>,
    },
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(i64),
    Variable(String),
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
    },
    Unary {
        op: UnOp,
        operand: Box<Expr>,
    },
}

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

pub fn parse_to_ast(source: &str) -> Result<ast::Program, CompileError> {
    let lexer = lexer::lex_adapter(source);
    grammar::ProgramParser::new()
        .parse(lexer)
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