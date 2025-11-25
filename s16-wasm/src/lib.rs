use s16_compiler::backend::sigma16::{
    compile_ir_to_sigma16_with_allocator, compile_ir_to_sigma16_with_allocator_mapped, AllocatorKind,
};
use serde::{Deserialize, Serialize};
use wasm_bindgen::prelude::*;

// ======================================================================================
// New WASM API (v2): snapshot-based, easy to consume, thoroughly documented.
//
// Design goals:
// - One-shot function `compile_snapshot` that returns everything a webapp needs to
//   present the same or richer insights than our TUI:
//     * IR lines with stable indices
//     * Sigma16 assembly (optional)
//     * Declared arrays
//     * AST spans with exact byte ranges and 0-based (line, col) for start/end
//     * Per-instruction mappings to AST with control-flow component and descriptions
//     * Convenience groupings by AST id and by control-flow component
// - 0-based coordinates consistently (line and column)
// - Stateless API: results are pure JSON (via serde_wasm_bindgen) – easy to cache
// - Backwards compatibility: legacy `compile` and `compile_with_options` remain,
//   but are deprecated in favor of `compile_snapshot`.
// ======================================================================================

#[cfg(feature = "console_error_panic_hook")]
pub use console_error_panic_hook::set_once as set_panic_hook;

/// Initialize the WASM module
#[wasm_bindgen(start)]
pub fn init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

// ===============================
// New public data model (v2)
// ===============================

/// Register allocator option for assembly generation.
#[derive(Serialize, Deserialize, Clone, Copy, Debug)]
#[serde(rename_all = "lowercase")]
pub enum AllocatorOpt {
    Basic,
    Advanced,
}

impl Default for AllocatorOpt {
    fn default() -> Self {
        AllocatorOpt::Advanced
    }
}

/// Control-flow component for fine-grained mapping.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum WasmControlFlowComponent {
    Condition,
    ThenBranch,
    ElseBranch,
    LoopBody,
    ControlFlowGlue,
}

/// AST node kind.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum WasmAstNodeKind {
    Assign,
    If,
    While,
    For,
    Number,
    Variable,
    Binary,
    Unary,
}

/// Span of an AST node in multiple coordinate systems.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmAstSpan {
    /// Stable ID of the AST node.
    pub ast_node_id: usize,
    /// Kind of AST node.
    pub kind: WasmAstNodeKind,
    /// Byte offsets [start, end) into the original source (UTF-8).
    pub start_byte: usize,
    pub end_byte: usize,
    /// 0-based line/column for the start of the span.
    pub start_line: usize,
    pub start_col: usize,
    /// 0-based line/column for the end of the span.
    pub end_line: usize,
    pub end_col: usize,
}

/// Mapping of a single IR instruction to an AST node.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmInstrMapping {
    /// IR instruction index.
    pub instr_index: usize,
    /// Target AST node id.
    pub ast_node_id: usize,
    /// Optional control-flow component.
    pub component: Option<WasmControlFlowComponent>,
    /// Human-friendly description of what the instruction does.
    pub description: String,
}

/// Group of IR instruction indices associated with an AST node.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmAstGroup {
    pub ast_node_id: usize,
    pub instr_indices: Vec<usize>,
}

/// Group of IR instruction indices associated with a control-flow component.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmComponentGroup {
    pub component: WasmControlFlowComponent,
    pub instr_indices: Vec<usize>,
}

/// Declared fixed-size arrays in the program.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmArrayDecl {
    pub name: String,
    pub size: usize,
}

/// Options for `compile_snapshot`. All fields are optional (sensible defaults).
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CompileOptions {
    /// Include textual IR lines in the snapshot (default: true).
    #[serde(default = "default_true")]
    pub emit_ir: bool,
    /// Include Sigma16 assembly in the snapshot (default: false).
    #[serde(default)]
    pub emit_asm: bool,
    /// Register allocator for assembly (default: "advanced").
    #[serde(default)]
    pub allocator: AllocatorOpt,
    /// Include per-instruction mappings (default: true).
    #[serde(default = "default_true")]
    pub include_mappings: bool,
    /// Include grouping by AST and component (default: true).
    #[serde(default = "default_true")]
    pub include_groups: bool,
}

fn default_true() -> bool {
    true
}

impl Default for CompileOptions {
    fn default() -> Self {
        Self {
            emit_ir: true,
            emit_asm: false,
            allocator: AllocatorOpt::Advanced,
            include_mappings: true,
            include_groups: true,
        }
    }
}

/// The complete, stateless snapshot returned by `compile_snapshot`.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ProgramSnapshot {
    /// Whether compilation succeeded.
    pub success: bool,
    /// Error message if compilation failed.
    pub error: Option<String>,

    /// IR lines (index-aligned), if requested.
    pub ir: Option<Vec<String>>,
    /// Sigma16 assembly, if requested.
    pub asm: Option<Vec<String>>,
    /// Mapping from each ASM line to the originating IR instruction index (if any).
    /// The i-th element corresponds to `asm[i]` when present.
    pub asm_ir_mapping: Option<Vec<Option<usize>>>,

    /// Declared arrays.
    pub arrays: Vec<WasmArrayDecl>,

    /// AST spans with precise positions.
    pub ast_spans: Vec<WasmAstSpan>,
    /// Flat list of per-instruction mappings.
    pub instr_mappings: Vec<WasmInstrMapping>,
    /// Convenience: groupings by AST id.
    pub by_ast: Vec<WasmAstGroup>,
    /// Convenience: groupings by control-flow component.
    pub by_component: Vec<WasmComponentGroup>,
}

/// Compile source to a rich, self-contained snapshot for the web UI.
///
/// Usage example (TypeScript):
/// ```ts
/// import init, { compile_snapshot } from 's16-wasm';
/// await init();
/// const snap = compile_snapshot(source, { emit_ir: true, emit_asm: false });
/// // snap.ir?.forEach((line, idx) => console.log(idx, line));
/// // Map an AST node to IR:
/// const node = snap.ast_spans[0];
/// const group = snap.by_ast.find(g => g.ast_node_id === node.ast_node_id);
/// console.log(group?.instr_indices);
/// ```
#[wasm_bindgen]
pub fn compile_snapshot(source: &str, options: JsValue) -> JsValue {
    let opts: CompileOptions = serde_wasm_bindgen::from_value(options).unwrap_or_default();
    let snapshot = compile_snapshot_internal(source, &opts);
    serde_wasm_bindgen::to_value(&snapshot).unwrap()
}

/// Compile with default options: IR included, ASM omitted, groupings + mappings included.
#[wasm_bindgen]
pub fn compile_snapshot_default(source: &str) -> JsValue {
    let snapshot = compile_snapshot_internal(source, &CompileOptions::default());
    serde_wasm_bindgen::to_value(&snapshot).unwrap()
}

fn compile_snapshot_internal(source: &str, opts: &CompileOptions) -> ProgramSnapshot {
    match s16_compiler::compile_to_ir(source) {
        Ok(ir) => {
            // Build line starts once for 0-based (line,col) conversion
            let line_starts = compute_line_starts(source);

            // IR + ASM choices
            let ir_lines = if opts.emit_ir {
                Some(ir.to_lines())
            } else {
                None
            };
            let (asm_lines, asm_ir_mapping) = if opts.emit_asm {
                let kind = match opts.allocator {
                    AllocatorOpt::Basic => AllocatorKind::Basic,
                    AllocatorOpt::Advanced => AllocatorKind::Advanced,
                };
                // Use mapped compilation to export per-line mapping to IR
                let asm = compile_ir_to_sigma16_with_allocator_mapped(kind, &ir);
                (
                    Some(asm.lines.clone()),
                    Some(asm.asm_ir_mapping.clone()),
                )
            } else {
                (None, None)
            };

            // Arrays
            let arrays: Vec<WasmArrayDecl> = ir
                .arrays
                .iter()
                .map(|(n, sz)| WasmArrayDecl {
                    name: n.clone(),
                    size: *sz,
                })
                .collect();

            // AST spans
            let mut ast_spans: Vec<WasmAstSpan> = Vec::new();
            for (id, start, end, kind) in ir.source_map.list_ast_spans_by_line() {
                let (sl, sc) = byte_to_line_col(&line_starts, start);
                let (el, ec) = byte_to_line_col(&line_starts, end);
                ast_spans.push(WasmAstSpan {
                    ast_node_id: id.0,
                    kind: map_kind(kind),
                    start_byte: start,
                    end_byte: end,
                    start_line: sl,
                    start_col: sc,
                    end_line: el,
                    end_col: ec,
                });
            }

            // Instruction mappings
            let mut instr_mappings: Vec<WasmInstrMapping> = Vec::new();
            if opts.include_mappings {
                for (idx, _) in ir.instrs.iter().enumerate() {
                    let maps = ir.source_map.get_mappings_for_instr(idx);
                    for m in maps {
                        instr_mappings.push(WasmInstrMapping {
                            instr_index: idx,
                            ast_node_id: m.ast_node_id.0,
                            component: m.component.map(map_component),
                            description: m.description.clone(),
                        });
                    }
                }
            }

            // Groupings
            let mut by_ast: Vec<WasmAstGroup> = Vec::new();
            let mut by_component: Vec<WasmComponentGroup> = Vec::new();
            if opts.include_groups {
                // by AST id: iterate ids we know from spans (stable set)
                for (id, _, _, _) in ir.source_map.list_ast_spans_by_line() {
                    let instr_indices = ir.source_map.get_instrs_for_ast(id);
                    by_ast.push(WasmAstGroup {
                        ast_node_id: id.0,
                        instr_indices,
                    });
                }
                // by component
                use s16_compiler::ir::ControlFlowComponent as C;
                for comp in [
                    C::Condition,
                    C::ThenBranch,
                    C::ElseBranch,
                    C::LoopBody,
                    C::ControlFlowGlue,
                ] {
                    let instr_indices = ir.source_map.get_instrs_for_component(comp);
                    by_component.push(WasmComponentGroup {
                        component: map_component(comp),
                        instr_indices,
                    });
                }
            }

            ProgramSnapshot {
                success: true,
                error: None,
                ir: ir_lines,
                asm: asm_lines,
                asm_ir_mapping,
                arrays,
                ast_spans,
                instr_mappings,
                by_ast,
                by_component,
            }
        }
        Err(e) => ProgramSnapshot {
            success: false,
            error: Some(e.to_string()),
            ir: None,
            asm: None,
            asm_ir_mapping: None,
            arrays: Vec::new(),
            ast_spans: Vec::new(),
            instr_mappings: Vec::new(),
            by_ast: Vec::new(),
            by_component: Vec::new(),
        },
    }
}

// ===============================
// Legacy API (deprecated)
// ===============================

/// Result of compilation (legacy). Prefer `ProgramSnapshot` returned by `compile_snapshot`.
#[derive(Serialize, Deserialize)]
pub struct CompilationResult {
    pub success: bool,
    pub ir: Option<Vec<String>>,
    pub asm: Option<Vec<String>>,
    pub error: Option<String>,
    pub source_map: Option<LegacySourceMapData>,
    pub ast_spans: Option<Vec<LegacyAstSpan>>, // Misnamed fields retained for back-compat
}

#[derive(Serialize, Deserialize)]
pub struct LegacySourceMapData {
    pub mappings: Vec<LegacyInstructionMapping>,
}

#[derive(Serialize, Deserialize)]
pub struct LegacyInstructionMapping {
    pub instr_index: usize,
    pub ast_node_id: usize,
    pub component: Option<String>,
    pub description: String,
}

#[derive(Serialize, Deserialize)]
pub struct LegacyAstSpan {
    pub ast_node_id: usize,
    /// WARNING: Historically these were actually byte offsets, not lines.
    /// They are kept as-is for compatibility with existing consumers.
    pub start_line: usize,
    pub end_line: usize,
    pub node_type: String,
}

#[deprecated(note = "Use compile_snapshot or compile_snapshot_default (v2 API)")]
#[wasm_bindgen]
pub fn compile(source: &str) -> JsValue {
    let legacy = compile_legacy_internal(source, true, false);
    serde_wasm_bindgen::to_value(&legacy).unwrap()
}

#[deprecated(note = "Use compile_snapshot (v2 API) with options")]
#[wasm_bindgen]
pub fn compile_with_options(source: &str, emit_ir: bool, emit_asm: bool) -> JsValue {
    let legacy = compile_legacy_internal(source, emit_ir, emit_asm);
    serde_wasm_bindgen::to_value(&legacy).unwrap()
}

fn compile_legacy_internal(source: &str, emit_ir: bool, emit_asm: bool) -> CompilationResult {
    match s16_compiler::compile_to_ir(source) {
        Ok(ir) => {
            let source_map_data = legacy_extract_source_map(&ir);
            let ast_spans = legacy_extract_ast_spans_from_ir(&ir);
            let ir_lines = if emit_ir { Some(ir.to_lines()) } else { None };
            let asm_lines = if emit_asm {
                // Legacy path used the simple allocator; preserve behavior
                let asm = s16_compiler::backend::sigma16::compile_ir_to_sigma16(&ir);
                Some(asm.lines().map(|s| s.to_string()).collect())
            } else {
                None
            };

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

fn legacy_extract_source_map(ir: &s16_compiler::ir::ProgramIR) -> LegacySourceMapData {
    let mut mappings = Vec::new();
    for (idx, _) in ir.instrs.iter().enumerate() {
        let ast_mappings = ir.source_map.get_mappings_for_instr(idx);
        for mapping in ast_mappings {
            mappings.push(LegacyInstructionMapping {
                instr_index: idx,
                ast_node_id: mapping.ast_node_id.0,
                component: mapping.component.map(|c| format!("{:?}", c)),
                description: mapping.description.clone(),
            });
        }
    }
    LegacySourceMapData { mappings }
}

fn legacy_extract_ast_spans_from_ir(ir: &s16_compiler::ir::ProgramIR) -> Vec<LegacyAstSpan> {
    use s16_compiler::ir::AstNodeKind;
    let mut out = Vec::new();
    for (id, start_byte, end_byte, kind) in ir.source_map.list_ast_spans_by_line() {
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
        out.push(LegacyAstSpan {
            ast_node_id: id.0,
            start_line: start_byte,
            end_line: end_byte,
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

// ===============================
// Internal helpers (positions, mapping conversions)
// ===============================

fn compute_line_starts(source: &str) -> Vec<usize> {
    let mut v = Vec::with_capacity(source.len() / 16 + 4);
    v.push(0);
    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            v.push(i + 1);
        }
    }
    v
}

fn byte_to_line_col(starts: &[usize], byte: usize) -> (usize, usize) {
    match starts.binary_search(&byte) {
        Ok(idx) => (idx, 0),
        Err(ins) => {
            let line_idx = ins.saturating_sub(1);
            let start = *starts.get(line_idx).unwrap_or(&0);
            (line_idx, byte.saturating_sub(start))
        }
    }
}

fn map_component(c: s16_compiler::ir::ControlFlowComponent) -> WasmControlFlowComponent {
    use s16_compiler::ir::ControlFlowComponent as C;
    match c {
        C::Condition => WasmControlFlowComponent::Condition,
        C::ThenBranch => WasmControlFlowComponent::ThenBranch,
        C::ElseBranch => WasmControlFlowComponent::ElseBranch,
        C::LoopBody => WasmControlFlowComponent::LoopBody,
        C::ControlFlowGlue => WasmControlFlowComponent::ControlFlowGlue,
    }
}

fn map_kind(k: s16_compiler::ir::AstNodeKind) -> WasmAstNodeKind {
    use s16_compiler::ir::AstNodeKind as K;
    match k {
        K::Assign => WasmAstNodeKind::Assign,
        K::If => WasmAstNodeKind::If,
        K::While => WasmAstNodeKind::While,
        K::For => WasmAstNodeKind::For,
        K::Number => WasmAstNodeKind::Number,
        K::Variable => WasmAstNodeKind::Variable,
        K::Binary => WasmAstNodeKind::Binary,
        K::Unary => WasmAstNodeKind::Unary,
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use wasm_bindgen_test::*;

    #[wasm_bindgen_test]
    fn test_compile_simple() {
        let source = "x = 5;\ny = 10;\nif x < y { z = x + y; }";
        let result = compile_snapshot_default(source);
        assert!(!result.is_null());
        let snap: ProgramSnapshot = serde_wasm_bindgen::from_value(result).unwrap();
        assert!(snap.success);
        assert!(snap.ir.as_ref().map(|v| !v.is_empty()).unwrap_or(false));
        assert!(!snap.ast_spans.is_empty());
        assert!(snap.instr_mappings.iter().any(|m| m.description.len() > 0));
    }
}
