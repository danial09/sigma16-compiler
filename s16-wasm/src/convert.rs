use s16_compiler::ir::{AstNodeKind, ControlFlowComponent};

use crate::types::{WasmAstNodeKind, WasmControlFlowComponent};

/// Compute byte offsets of each line start in `source`.
pub fn compute_line_starts(source: &str) -> Vec<usize> {
    let mut starts = vec![0];
    for (i, ch) in source.char_indices() {
        if ch == '\n' {
            starts.push(i + 1);
        }
    }
    starts
}

/// Convert a byte offset into a 0-based (line, column) pair.
pub fn byte_to_line_col(starts: &[usize], byte: usize) -> (usize, usize) {
    match starts.binary_search(&byte) {
        Ok(idx) => (idx, 0),
        Err(ins) => {
            let line = ins.saturating_sub(1);
            let col = byte.saturating_sub(*starts.get(line).unwrap_or(&0));
            (line, col)
        }
    }
}

pub fn map_component(c: ControlFlowComponent) -> WasmControlFlowComponent {
    match c {
        ControlFlowComponent::Condition => WasmControlFlowComponent::Condition,
        ControlFlowComponent::ThenBranch => WasmControlFlowComponent::ThenBranch,
        ControlFlowComponent::ElseBranch => WasmControlFlowComponent::ElseBranch,
        ControlFlowComponent::LoopBody => WasmControlFlowComponent::LoopBody,
        ControlFlowComponent::ControlFlowGlue => WasmControlFlowComponent::ControlFlowGlue,
    }
}

pub fn map_kind(k: AstNodeKind) -> WasmAstNodeKind {
    match k {
        AstNodeKind::Assign => WasmAstNodeKind::Assign,
        AstNodeKind::If => WasmAstNodeKind::If,
        AstNodeKind::While => WasmAstNodeKind::While,
        AstNodeKind::For => WasmAstNodeKind::For,
        AstNodeKind::Number => WasmAstNodeKind::Number,
        AstNodeKind::Variable => WasmAstNodeKind::Variable,
        AstNodeKind::Binary => WasmAstNodeKind::Binary,
        AstNodeKind::Unary => WasmAstNodeKind::Unary,
    }
}
