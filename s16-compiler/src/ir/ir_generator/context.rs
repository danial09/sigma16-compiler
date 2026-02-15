use crate::ir::ast::AstSpanRecord;
use crate::ir::symbol_table::SymbolTable;
use crate::ir::*;
use crate::{CompileError, SemanticErrorKind};

#[derive(Debug, Clone)]
pub struct FunctionCtx {
    pub had_return: bool,
}

pub struct Gen {
    pub out: ProgramIR,
    pub temp_count: usize,
    pub label_count: usize,
    pub current_ast_node: Option<(AstNodeId, Option<ControlFlowComponent>)>,
    pub parent_component: Option<ControlFlowComponent>,
    pub fn_ctx: Option<FunctionCtx>,
    pub symbols: SymbolTable,
    pub ast_spans: Vec<AstSpanRecord>,
}

impl Gen {
    pub fn new(ast_spans: Vec<AstSpanRecord>, source: &str) -> Self {
        let mut out = ProgramIR::new();
        out.source_map.init_source_index(source);
        Self {
            out,
            temp_count: 0,
            label_count: 0,
            current_ast_node: None,
            parent_component: None,
            fn_ctx: None,
            symbols: SymbolTable::new(),
            ast_spans,
        }
    }

    pub fn finish(self) -> (ProgramIR, Vec<AstSpanRecord>) {
        (self.out, self.ast_spans)
    }

    pub fn make_error(
        &self,
        kind: SemanticErrorKind,
        ast_id: AstNodeId,
        message: String,
    ) -> CompileError {
        // Get span info for the AST node
        let (line, col, offset) =
            if let Some(span_record) = self.ast_spans.iter().find(|s| s.id == ast_id) {
                // Convert byte offset to line:col if we have source index
                let (l, c) = self
                    .out
                    .source_map
                    .source_index_ref()
                    .map(|idx| idx.to_line_col(span_record.start))
                    .unwrap_or((0, 0));
                (l + 1, c + 1, span_record.start) // Convert to 1-based for display
            } else {
                (0, 0, 0)
            };

        CompileError::Semantic {
            kind,
            location: crate::SourceLocation {
                line,
                column: col,
                offset,
            },
            line,
            col,
            message,
        }
    }

    pub fn new_temp(&mut self) -> Var {
        let name = format!("__t{}", self.temp_count);
        self.temp_count += 1;
        Var::temp(name)
    }

    pub fn get_var(&self, name: String) -> Var {
        if self.fn_ctx.is_some() {
            Var::local(name)
        } else {
            Var::global(name)
        }
    }

    pub fn new_label(&mut self) -> String {
        let l = format!("L{}", self.label_count);
        self.label_count += 1;
        l
    }

    pub fn emit(&mut self, i: Instr) {
        let instr_index = self.out.instrs.len();
        self.out.instrs.push(i);

        if let Some((ast_id, component)) = self.current_ast_node {
            let description = self.generate_description(&self.out.instrs[instr_index]);
            let mut mapping = AstMapping::new(ast_id, description);
            if let Some(comp) = component.or(self.parent_component) {
                mapping = mapping.with_component(comp);
            }
            self.out.source_map.add_mapping(instr_index, mapping);
        }
    }

    pub fn generate_description(&self, instr: &Instr) -> String {
        match instr {
            Instr::Assign { dst, .. } => format!("Assign to {dst}"),
            Instr::IfCmpGoto { .. } => "Conditional branch".to_string(),
            Instr::Goto(_) => "Unconditional jump".to_string(),
            Instr::Label(l) => format!("Label: {l}"),
            Instr::Load { dst, .. } => format!("Load into {dst}"),
            Instr::Store { .. } => "Store through pointer".to_string(),
            Instr::ArrayLoad { dst, .. } => format!("Array load into {dst}"),
            Instr::ArrayStore { .. } => "Array store".to_string(),
            Instr::FuncStart { name, .. } => format!("Function start: {name}"),
            Instr::FuncEnd { name } => format!("Function end: {name}"),
            Instr::Call { func, .. } => format!("Call {func}"),
            Instr::Return { .. } => "Return".to_string(),
            Instr::ArrayDecl { name, .. } => format!("Array declaration: {name}"),
        }
    }

    pub fn with_ast_context<F, R>(
        &mut self,
        ast_id: AstNodeId,
        component: Option<ControlFlowComponent>,
        f: F,
    ) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let prev = self.current_ast_node;
        self.current_ast_node = Some((ast_id, component));
        let result = f(self);
        self.current_ast_node = prev;
        result
    }

    pub fn with_parent_component<F, R>(&mut self, component: ControlFlowComponent, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
    {
        let prev = self.parent_component;
        self.parent_component = Some(component);
        let result = f(self);
        self.parent_component = prev;
        result
    }
}
