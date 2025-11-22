// compiler/src/frontend/mod.rs

pub mod lexer;

// Expose the LALRPOP-generated parser module under frontend::grammar
lalrpop_util::lalrpop_mod!(pub grammar);
