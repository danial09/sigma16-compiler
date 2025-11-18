use wasm_bindgen::prelude::*;
use serde::{Deserialize, Serialize};

#[cfg(feature = "console_error_panic_hook")]
pub use console_error_panic_hook::set_once as set_panic_hook;

/// Initialize the WASM module
#[wasm_bindgen(start)]
pub fn init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Result of compilation
#[derive(Serialize, Deserialize)]
pub struct CompilationResult {
    pub success: bool,
    pub ir: Option<Vec<String>>,
    pub asm: Option<Vec<String>>,
    pub error: Option<String>,
    pub source_map: Option<SourceMapData>,
    pub ast_spans: Option<Vec<AstSpan>>,
}

/// Source map information for linking IR to AST
#[derive(Serialize, Deserialize)]
pub struct SourceMapData {
    pub mappings: Vec<InstructionMapping>,
}

/// Maps an IR instruction index to AST node information
#[derive(Serialize, Deserialize)]
pub struct InstructionMapping {
    pub instr_index: usize,
    pub ast_node_id: usize,
    pub component: Option<String>,
    pub description: String,
}

/// Represents a span of source code corresponding to an AST node
#[derive(Serialize, Deserialize)]
pub struct AstSpan {
    pub ast_node_id: usize,
    pub start_line: usize,
    pub end_line: usize,
    pub node_type: String,
}

/// Compile source code to IR (legacy helper – returns IR only)
#[wasm_bindgen]
pub fn compile(source: &str) -> JsValue {
    let result = compile_internal(source, true, false);
    serde_wasm_bindgen::to_value(&result).unwrap()
}

/// New unified entry: choose to emit IR, assembly, or both efficiently
#[wasm_bindgen]
pub fn compile_with_options(source: &str, emit_ir: bool, emit_asm: bool) -> JsValue {
    let result = compile_internal(source, emit_ir, emit_asm);
    serde_wasm_bindgen::to_value(&result).unwrap()
}

fn compile_internal(source: &str, emit_ir: bool, emit_asm: bool) -> CompilationResult {
    match compiler::compile_to_ir(source) {
        Ok(ir) => {
            // Extract source map data and AST spans from the IR
            let source_map_data = extract_source_map(&ir);
            let ast_spans = extract_ast_spans_from_ir(&ir);

            // Conditionally materialize IR and/or assembly
            let ir_lines = if emit_ir { Some(ir.to_lines()) } else { None };
            let asm_lines = if emit_asm {
                let asm = compiler::backend::sigma16::compile_ir_to_sigma16(&ir);
                Some(asm.lines().map(|s| s.to_string()).collect())
            } else { None };

            CompilationResult {
                success: true,
                ir: ir_lines,
                asm: asm_lines,
                error: None,
                source_map: Some(source_map_data),
                ast_spans: Some(ast_spans),
            }
        }
        Err(e) => CompilationResult {
            success: false,
            ir: None,
            asm: None,
            error: Some(e.to_string()),
            source_map: None,
            ast_spans: None,
        },
    }
}

fn extract_source_map(ir: &compiler::ir::ProgramIR) -> SourceMapData {
    let mut mappings = Vec::new();

    for (idx, _) in ir.instrs.iter().enumerate() {
        let ast_mappings = ir.source_map.get_mappings_for_instr(idx);
        for mapping in ast_mappings {
            mappings.push(InstructionMapping {
                instr_index: idx,
                ast_node_id: mapping.ast_node_id.0,
                component: mapping.component.map(|c| format!("{:?}", c)),
                description: mapping.description.clone(),
            });
        }
    }

    SourceMapData { mappings }
}

fn extract_ast_spans_from_ir(ir: &compiler::ir::ProgramIR) -> Vec<AstSpan> {
    use compiler::ir::AstNodeKind;

    let mut out = Vec::new();
    for (id, start_line, end_line, kind) in ir.source_map.list_ast_spans_by_line() {
        let node_type = match kind {
            AstNodeKind::Assign => "Assign",
            AstNodeKind::If => "If",
            AstNodeKind::While => "While",
            AstNodeKind::For => "For",
            AstNodeKind::Number => "Number",
            AstNodeKind::Variable => "Variable",
            AstNodeKind::Binary => "Binary",
            AstNodeKind::Unary => "Unary",
        };
        out.push(AstSpan {
            ast_node_id: id.0,
            start_line,
            end_line,
            node_type: node_type.to_string(),
        });
    }
    out
}

/// Get version information
#[wasm_bindgen]
pub fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_compile_simple() {
        let source = "x = 5\ny = 10";
        let result = compile(source);
        assert!(!result.is_null());
    }
}