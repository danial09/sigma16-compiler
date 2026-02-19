use crate::CompileError;
use crate::frontend::{grammar, lexer};
use crate::ir::{AstNodeId, AstNodeKind};

#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    /// Assignment to an lvalue
    Assign {
        id: AstNodeId,
        target: LValue,
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
    },
    /// Fixed-size global array declaration: array name [ N ]
    ArrayDecl {
        id: AstNodeId,
        name: String,
        size: usize,
        initial_values: Option<Vec<i64>>,
    },
    /// Function definition with untyped parameters
    Function {
        id: AstNodeId,
        name: String,
        params: Vec<String>,
        body: Vec<Stmt>,
    },
    /// Return from function (single return value)
    Return { id: AstNodeId, value: Option<Expr> },
    /// Expression used as a statement (e.g., a call with ignored result)
    ExprStmt { id: AstNodeId, expr: Expr },
    /// Global variable assignment from within a function: global x = expr;
    GlobalAssign {
        id: AstNodeId,
        name: String,
        value: Expr,
    },
    /// String declaration: name = "abc";
    StringDecl {
        id: AstNodeId,
        name: String,
        value: String,
    },
}

impl Stmt {
    pub fn id(&self) -> AstNodeId {
        match self {
            Stmt::Assign { id, .. } => *id,
            Stmt::If { id, .. } => *id,
            Stmt::While { id, .. } => *id,
            Stmt::For { id, .. } => *id,
            Stmt::ArrayDecl { id, .. } => *id,
            Stmt::Function { id, .. } => *id,
            Stmt::Return { id, .. } => *id,
            Stmt::ExprStmt { id, .. } => *id,
            Stmt::GlobalAssign { id, .. } => *id,
            Stmt::StringDecl { id, .. } => *id,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number(AstNodeId, i64),
    Variable(AstNodeId, String),
    /// Read a global variable explicitly: global x
    GlobalVar(AstNodeId, String),
    /// Function call: name(args)
    Call {
        id: AstNodeId,
        name: String,
        args: Vec<Expr>,
    },
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
    /// Address-of a variable: &x
    AddrOf(AstNodeId, String),
    /// Dereference of a pointer expression: *p
    Deref(AstNodeId, Box<Expr>),
    /// Array or pointer indexing: a[i]
    Index {
        id: AstNodeId,
        base: String,
        index: Box<Expr>,
    },
}

impl Expr {
    pub fn id(&self) -> AstNodeId {
        match self {
            Expr::Number(id, _) => *id,
            Expr::Variable(id, _) => *id,
            Expr::GlobalVar(id, _) => *id,
            Expr::Call { id, .. } => *id,
            Expr::Binary { id, .. } => *id,
            Expr::Unary { id, .. } => *id,
            Expr::AddrOf(id, _) => *id,
            Expr::Deref(id, _) => *id,
            Expr::Index { id, .. } => *id,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    And,
    Or,
    BitAnd,
    BitOr,
    BitXor,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum UnOp {
    Not,
    Neg,
    BitNot,
    // Note: address-of and deref are represented as dedicated Expr variants
}

/// Assignment target (lvalue)
#[derive(Debug, Clone)]
pub enum LValue {
    Var(String),
    Deref(Box<Expr>),
    Index { base: String, index: Expr },
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

/// A record of an AST node's span in the source
#[derive(Debug, Clone)]
pub struct AstSpanRecord {
    pub id: AstNodeId,
    pub start: usize,
    pub end: usize,
    pub kind: AstNodeKind,
}

#[derive(Default)]
pub struct SpanCollector {
    spans: Vec<AstSpanRecord>,
}

impl SpanCollector {
    pub fn new() -> Self {
        Self { spans: Vec::new() }
    }
    pub fn add(&mut self, id: AstNodeId, start: usize, end: usize, kind: AstNodeKind) {
        self.spans.push(AstSpanRecord {
            id,
            start,
            end,
            kind,
        });
    }
    pub fn into_vec(self) -> Vec<AstSpanRecord> {
        self.spans
    }
}

/// Parsed AST along with span information for each node
pub struct ParsedAst {
    pub program: Program,
    pub spans: Vec<AstSpanRecord>,
}

pub fn parse_to_ast(source: &str) -> Result<ParsedAst, CompileError> {
    use std::cell::RefCell;

    let lexer = lexer::lex_adapter(source);
    let id_gen = RefCell::new(AstNodeIdGenerator::new());
    let spans = RefCell::new(SpanCollector::new());

    let program = grammar::ProgramParser::new()
        .parse(&id_gen, &spans, lexer)
        .map_err(|e| {
            use lalrpop_util::ParseError;

            /// Build a friendly comma-separated "expected" list
            fn fmt_expected(expected: &[String]) -> String {
                expected
                    .iter()
                    .map(|s| lexer::friendly_token_name(s))
                    .collect::<Vec<_>>()
                    .join(", ")
            }

            /// Helper: create CompileError::Parse with line/col from byte position
            fn make_parse_error(source: &str, position: usize, message: String) -> CompileError {
                let (line, col) = lexer::position_to_line_col(source, position);
                let context = lexer::get_error_context(source, position);
                CompileError::Parse {
                    line,
                    col,
                    context,
                    message,
                }
            }

            match e {
                ParseError::InvalidToken { location } => {
                    make_parse_error(source, location, "Invalid token".to_string())
                }
                ParseError::UnrecognizedEof { location, expected } => make_parse_error(
                    source,
                    location,
                    format!(
                        "Unexpected end of input. Expected one of: {}",
                        fmt_expected(&expected)
                    ),
                ),
                ParseError::UnrecognizedToken {
                    token: (start, tok, _end),
                    expected,
                } => make_parse_error(
                    source,
                    start,
                    format!(
                        "Unexpected token '{}'. Expected one of: {}",
                        tok,
                        fmt_expected(&expected)
                    ),
                ),
                ParseError::ExtraToken {
                    token: (start, tok, _end),
                } => make_parse_error(source, start, format!("Extra token '{}'", tok)),
                ParseError::User { error } => {
                    // Lexical errors already carry line/col
                    CompileError::Parse {
                        line: error.line,
                        col: error.column,
                        context: error.context.clone(),
                        message: format!("Unexpected character '{}'", error.unexpected_char),
                    }
                }
            }
        })?;

    Ok(ParsedAst {
        program,
        spans: spans.into_inner().into_vec(),
    })
}
