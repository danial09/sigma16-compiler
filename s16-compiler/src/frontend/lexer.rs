use logos::Logos;
use std::fmt;

#[derive(Logos, Debug, PartialEq, Eq, Hash, Clone)]
#[logos(skip r"[ \t\r\n]+")] // Whitespace
#[logos(skip r"#[^\n]*")] // Shell-style comments starting with '#'
#[logos(skip r"//[^\n]*")] // C++-style line comments starting with '//'
pub enum Token {
    // --- Keywords ---
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("while")]
    While,
    #[token("for")]
    For,
    #[token("from")]
    From,
    #[token("to")]
    To,

    // functions, returns, arrays
    #[token("fn")]
    Fn,
    #[token("return")]
    Return,
    #[token("array")]
    Array,
    #[token("global")]
    Global,

    #[token("and")]
    And,
    #[token("or")]
    Or,
    #[token("not")]
    Not,

    // --- Identifiers and Numbers ---
    #[regex(r"[A-Za-z_][A-Za-z0-9_]*", |lex| lex.slice().to_string())]
    Ident(String),

    #[regex(r"-?[0-9]+", |lex| lex.slice().parse().ok())]
    Number(i64),

    // --- Operators ---
    #[token("==")]
    Eq,
    #[token("!=")]
    Neq,

    #[token("=")]
    Assign,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("%")]
    Mod,

    #[token(">")]
    Gt,
    #[token("<")]
    Lt,
    #[token(">=")]
    Ge,
    #[token("<=")]
    Le,

    // --- Punctuation
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("[")]
    LBracket,
    #[token("]")]
    RBracket,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    #[token(",")]
    Comma,
    #[token("&")]
    Amp,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token(";")]
    Semicolon,

    #[regex(r#""[^"]*""#, |lex| {
        let s = lex.slice();
        s[1..s.len()-1].to_string()
    })]
    String(String),
}

/// Custom error type for lexical errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LexicalError {
    pub location: usize,
    pub line: usize,
    pub column: usize,
    pub unexpected_char: char,
    pub context: String,
}

impl fmt::Display for LexicalError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "Unexpected character '{}' at line {}, column {} (position {})\n  Context: {}",
            self.unexpected_char, self.line, self.column, self.location, self.context
        )
    }
}

impl std::error::Error for LexicalError {}

/// Convert a byte position to line and column numbers
fn position_to_line_col(source: &str, position: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;

    for (i, ch) in source.char_indices() {
        if i >= position {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    (line, col)
}

/// Get context around an error position (the line containing the error)
fn get_error_context(source: &str, position: usize) -> String {
    let line_start = source[..position]
        .rfind('\n')
        .map(|pos| pos + 1)
        .unwrap_or(0);

    let line_end = source[position..]
        .find('\n')
        .map(|pos| position + pos)
        .unwrap_or(source.len());

    source[line_start..line_end].trim().to_string()
}

/// Create a detailed lexical error from a position and source
fn create_lexical_error(source: &str, position: usize) -> LexicalError {
    let (line, column) = position_to_line_col(source, position);
    let unexpected_char = source.chars().nth(position).unwrap_or('\0');
    let context = get_error_context(source, position);

    LexicalError {
        location: position,
        line,
        column,
        unexpected_char,
        context,
    }
}

// Adapter to convert Logos lexer output to LALRPOP's expected format
pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;

pub struct LexerAdapter<'source> {
    source: &'source str,
    lexer: logos::Lexer<'source, Token>,
}

impl<'source> LexerAdapter<'source> {
    pub fn new(source: &'source str) -> Self {
        Self {
            source,
            lexer: Token::lexer(source),
        }
    }
}

impl<'source> Iterator for LexerAdapter<'source> {
    type Item = Spanned<Token, usize, LexicalError>;

    fn next(&mut self) -> Option<Self::Item> {
        let token_result = self.lexer.next()?;
        let span = self.lexer.span();

        Some(match token_result {
            Ok(token) => Ok((span.start, token, span.end)),
            Err(_) => Err(create_lexical_error(self.source, span.start)),
        })
    }
}

pub fn lex_adapter(source: &str) -> LexerAdapter {
    LexerAdapter::new(source)
}
