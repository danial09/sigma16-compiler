use s16_compiler::backend::{compile_ir_to_sigma16_with_allocator_mapped, AllocatorKind};
use s16_compiler::ir::ControlFlowComponent;

use crate::convert::{byte_to_line_col, compute_line_starts, map_component, map_kind};
use crate::types::*;

pub fn compile_snapshot_internal(source: &str, opts: &CompileOptions) -> ProgramSnapshot {
    let ir = match s16_compiler::compile_to_ir(source) {
        Ok(ir) => ir,
        Err(e) => return ProgramSnapshot::error(e.to_string()),
    };

    let line_starts = compute_line_starts(source);

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
        let asm = compile_ir_to_sigma16_with_allocator_mapped(kind, &ir);
        (Some(asm.lines.clone()), Some(asm.asm_ir_mapping.clone()))
    } else {
        (None, None)
    };

    let arrays: Vec<WasmArrayDecl> = ir
        .arrays
        .iter()
        .map(|(name, size, init, _)| WasmArrayDecl {
            name: name.clone(),
            size: *size,
            initial_values: init.clone(),
        })
        .collect();

    let ast_spans: Vec<WasmAstSpan> = ir
        .source_map
        .list_ast_spans_by_line()
        .into_iter()
        .map(|(id, start, end, kind)| {
            let (start_line, start_col) = byte_to_line_col(&line_starts, start);
            let (end_line, end_col) = byte_to_line_col(&line_starts, end);
            WasmAstSpan {
                ast_node_id: id.0,
                kind: map_kind(kind),
                start_byte: start,
                end_byte: end,
                start_line,
                start_col,
                end_line,
                end_col,
            }
        })
        .collect();

    let instr_mappings = if opts.include_mappings {
        ir.instrs
            .iter()
            .enumerate()
            .flat_map(|(idx, _)| {
                ir.source_map
                    .get_mappings_for_instr(idx)
                    .into_iter()
                    .map(move |m| WasmInstrMapping {
                        instr_index: idx,
                        ast_node_id: m.ast_node_id.0,
                        component: m.component.map(map_component),
                        description: m.description.clone(),
                    })
            })
            .collect()
    } else {
        Vec::new()
    };

    let (by_ast, by_component) = if opts.include_groups {
        let by_ast = ir
            .source_map
            .list_ast_spans_by_line()
            .into_iter()
            .map(|(id, _, _, _)| WasmAstGroup {
                ast_node_id: id.0,
                instr_indices: ir.source_map.get_instrs_for_ast(id),
            })
            .collect();

        let by_component = [
            ControlFlowComponent::Condition,
            ControlFlowComponent::ThenBranch,
            ControlFlowComponent::ElseBranch,
            ControlFlowComponent::LoopBody,
            ControlFlowComponent::ControlFlowGlue,
        ]
        .into_iter()
        .map(|comp| WasmComponentGroup {
            component: map_component(comp),
            instr_indices: ir.source_map.get_instrs_for_component(comp),
        })
        .collect();

        (by_ast, by_component)
    } else {
        (Vec::new(), Vec::new())
    };

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
