use serde::{Deserialize, Serialize};

// ── Compilation options ──────────────────────────────────────────────

/// Register allocator selection for assembly generation.
#[derive(Serialize, Deserialize, Clone, Copy, Debug, Default)]
#[serde(rename_all = "lowercase")]
pub enum AllocatorOpt {
    Basic,
    #[default]
    Advanced,
}

fn default_true() -> bool {
    true
}

/// Options for `compile_snapshot`.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct CompileOptions {
    #[serde(default = "default_true")]
    pub emit_ir: bool,
    #[serde(default)]
    pub emit_asm: bool,
    #[serde(default)]
    pub allocator: AllocatorOpt,
    #[serde(default = "default_true")]
    pub include_mappings: bool,
    #[serde(default = "default_true")]
    pub include_groups: bool,
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

// ── Snapshot (return type) ───────────────────────────────────────────

/// Complete compilation result returned by `compile_snapshot`.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct ProgramSnapshot {
    pub success: bool,
    pub error: Option<String>,
    pub ir: Option<Vec<String>>,
    pub asm: Option<Vec<String>>,
    /// Per-ASM-line mapping to the originating IR instruction index.
    pub asm_ir_mapping: Option<Vec<Option<usize>>>,
    pub arrays: Vec<WasmArrayDecl>,
    pub ast_spans: Vec<WasmAstSpan>,
    pub instr_mappings: Vec<WasmInstrMapping>,
    pub by_ast: Vec<WasmAstGroup>,
    pub by_component: Vec<WasmComponentGroup>,
}

impl ProgramSnapshot {
    pub fn error(msg: String) -> Self {
        Self {
            success: false,
            error: Some(msg),
            ir: None,
            asm: None,
            asm_ir_mapping: None,
            arrays: Vec::new(),
            ast_spans: Vec::new(),
            instr_mappings: Vec::new(),
            by_ast: Vec::new(),
            by_component: Vec::new(),
        }
    }
}

// ── Sub-types ────────────────────────────────────────────────────────

#[derive(Serialize, Deserialize, Clone, Copy, Debug, PartialEq, Eq)]
#[serde(rename_all = "camelCase")]
pub enum WasmControlFlowComponent {
    Condition,
    ThenBranch,
    ElseBranch,
    LoopBody,
    ControlFlowGlue,
}

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

/// AST node span with byte offsets and 0-based line/column positions.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmAstSpan {
    pub ast_node_id: usize,
    pub kind: WasmAstNodeKind,
    pub start_byte: usize,
    pub end_byte: usize,
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

/// Mapping of a single IR instruction to an AST node.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmInstrMapping {
    pub instr_index: usize,
    pub ast_node_id: usize,
    pub component: Option<WasmControlFlowComponent>,
    pub description: String,
}

/// IR instruction indices grouped by AST node.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmAstGroup {
    pub ast_node_id: usize,
    pub instr_indices: Vec<usize>,
}

/// IR instruction indices grouped by control-flow component.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmComponentGroup {
    pub component: WasmControlFlowComponent,
    pub instr_indices: Vec<usize>,
}

/// Declared array in the source program.
#[derive(Serialize, Deserialize, Clone, Debug)]
pub struct WasmArrayDecl {
    pub name: String,
    pub size: usize,
    pub initial_values: Option<Vec<i64>>,
}
