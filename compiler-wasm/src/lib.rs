use std::collections::HashSet;
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

/// Compile source code to IR
#[wasm_bindgen]
pub fn compile(source: &str) -> JsValue {
    let result = match compiler::compile_to_ir(source) {
        Ok(ir) => {
            // Extract source map data
            let source_map_data = extract_source_map(&ir);

            // Extract AST spans from source code
            let ast_spans = extract_ast_spans_from_source(source, &source_map_data);

            CompilationResult {
                success: true,
                ir: Some(ir.to_lines()),
                error: None,
                source_map: Some(source_map_data),
                ast_spans: Some(ast_spans),
            }
        },
        Err(e) => CompilationResult {
            success: false,
            ir: None,
            error: Some(e.to_string()),
            source_map: None,
            ast_spans: None,
        },
    };

    serde_wasm_bindgen::to_value(&result).unwrap()
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

fn extract_ast_spans_from_source(source: &str, source_map: &SourceMapData) -> Vec<AstSpan> {
    use std::collections::{HashMap, HashSet};

    // Group mappings by AST node ID to understand which nodes exist
    let mut ast_nodes: HashMap<usize, Vec<&InstructionMapping>> = HashMap::new();
    for mapping in &source_map.mappings {
        ast_nodes.entry(mapping.ast_node_id)
            .or_insert_with(Vec::new)
            .push(mapping);
    }

    // Parse the source to identify statement boundaries
    let lines: Vec<&str> = source.lines().collect();
    let mut spans = Vec::new();

    // Track which AST nodes we've already processed (to avoid duplicates)
    let mut processed_nodes: HashSet<usize> = HashSet::new();

    let mut current_line = 0;

    while current_line < lines.len() {
        let line = lines[current_line].trim();

        // Skip empty lines
        if line.is_empty() {
            current_line += 1;
            continue;
        }

        // Determine statement type and span
        let (node_type, end_line) = if line.starts_with("if ") {
            ("If", find_block_end(&lines, current_line))
        } else if line.starts_with("while ") {
            ("While", find_block_end(&lines, current_line))
        } else if line.starts_with("for ") {
            ("For", find_block_end(&lines, current_line))
        } else {
            // Simple assignment
            ("Assign", current_line)
        };

        // Find the AST node ID that corresponds to this source line range
        // by looking at mappings
        if let Some(ast_node_id) = find_ast_node_for_line_range(
            current_line,
            end_line,
            &source_map.mappings,
            node_type,
            &processed_nodes
        ) {
            spans.push(AstSpan {
                ast_node_id,
                start_line: current_line,
                end_line,
                node_type: node_type.to_string(),
            });
            processed_nodes.insert(ast_node_id);
        }

        current_line = end_line + 1;
    }

    spans
}

fn find_block_end(lines: &[&str], start: usize) -> usize {
    let mut depth = 0;
    let mut i = start;

    while i < lines.len() {
        let line = lines[i].trim();

        for ch in line.chars() {
            if ch == '{' {
                depth += 1;
            } else if ch == '}' {
                depth -= 1;
                if depth == 0 {
                    return i;
                }
            }
        }

        // For simple statements without braces
        if depth == 0 && i > start {
            return i - 1;
        }

        i += 1;
    }

    lines.len() - 1
}

fn find_ast_node_for_line_range(
    start_line: usize,
    end_line: usize,
    mappings: &[InstructionMapping],
    node_type: &str,
    processed: &HashSet<usize>
) -> Option<usize> {
    // Strategy: Find an AST node that hasn't been processed yet and has
    // mappings that would logically correspond to this source range

    // First, try to find a node with a component matching the node type
    let target_component = match node_type {
        "If" => Some("Condition"),
        "While" => Some("Condition"),
        "For" => Some("Condition"),
        "Assign" => None,
        _ => None,
    };

    // Collect all AST node IDs that haven't been processed
    let mut candidate_nodes: Vec<usize> = mappings
        .iter()
        .map(|m| m.ast_node_id)
        .filter(|id| !processed.contains(id))
        .collect();

    // Remove duplicates
    candidate_nodes.sort();
    candidate_nodes.dedup();

    // If we have a target component, prefer nodes with that component
    if let Some(comp) = target_component {
        for node_id in &candidate_nodes {
            let has_component = mappings
                .iter()
                .any(|m| m.ast_node_id == *node_id &&
                    m.component.as_deref() == Some(comp));

            if has_component {
                return Some(*node_id);
            }
        }
    }

    // Fall back to the first unprocessed node
    candidate_nodes.first().copied()
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