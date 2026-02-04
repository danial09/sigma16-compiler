use crate::ir::AstNodeId;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Variable,
    Array,
    Function,
}

#[derive(Debug, Clone)]
pub struct SymbolInfo {
    pub kind: SymbolKind,
    pub ast_id: AstNodeId,
    pub param_count: Option<usize>, // For functions
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    /// Global scope symbols (variables, arrays, functions)
    globals: HashMap<String, SymbolInfo>,
    /// Local scope symbols (function parameters and local variables)
    /// Only active when inside a function
    locals: Option<HashMap<String, SymbolInfo>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            globals: HashMap::new(),
            locals: None,
        }
    }

    /// Enter function scope
    pub fn enter_function(&mut self) {
        self.locals = Some(HashMap::new());
    }

    /// Exit function scope
    pub fn exit_function(&mut self) {
        self.locals = None;
    }

    /// Check if currently in function scope
    pub fn in_function(&self) -> bool {
        self.locals.is_some()
    }

    /// Declare a symbol in the current scope
    pub fn declare(&mut self, name: String, info: SymbolInfo) -> Result<(), SymbolInfo> {
        let table = if let Some(ref mut locals) = self.locals {
            locals
        } else {
            &mut self.globals
        };

        if let Some(existing) = table.get(&name) {
            return Err(existing.clone());
        }

        table.insert(name, info);
        Ok(())
    }

    /// Declare a symbol in global scope (even if inside a function)
    pub fn declare_global(&mut self, name: String, info: SymbolInfo) -> Result<(), SymbolInfo> {
        if let Some(existing) = self.globals.get(&name) {
            return Err(existing.clone());
        }

        self.globals.insert(name, info);
        Ok(())
    }

    /// Lookup a symbol (checks locals first, then globals)
    pub fn lookup(&self, name: &str) -> Option<&SymbolInfo> {
        if let Some(ref locals) = self.locals {
            if let Some(info) = locals.get(name) {
                return Some(info);
            }
        }
        self.globals.get(name)
    }

    /// Lookup only in global scope
    pub fn lookup_global(&self, name: &str) -> Option<&SymbolInfo> {
        self.globals.get(name)
    }
}
