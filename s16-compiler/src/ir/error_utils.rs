use crate::{CompileError, SemanticErrorKind, SourceLocation};
use crate::ir::{AstNodeId, SourceMap};

/// Helper to create semantic errors with proper line/col conversion
pub fn make_semantic_error(
    kind: SemanticErrorKind,
    ast_id: AstNodeId,
    source_map: &SourceMap,
    message: String,
) -> CompileError {
    // Get span info for the AST node
    let (line, col, offset) = if let Some(info) = source_map.get_ast_info_by_id(ast_id) {
        let (l, c) = source_map.source_index_ref()
            .map(|idx| idx.to_line_col(info.span.start))
            .unwrap_or((0, 0));
        (l + 1, c + 1, info.span.start) // Convert to 1-based for display
    } else {
        (0, 0, 0)
    };

    CompileError::Semantic {
        kind,
        location: SourceLocation {
            line,
            column: col,
            offset,
        },
        line,
        col,
        message,
    }
}
