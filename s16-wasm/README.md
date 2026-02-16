### s16-wasm — WebAssembly interface for the Sigma16 compiler

Exposes the compiler as a WASM module for use in web applications.

#### API

**`compile_snapshot(source, options) → ProgramSnapshot`**

Compiles source code and returns a self-contained snapshot. Options (all optional):

| Field | Default | Description |
|---|---|---|
| `emit_ir` | `true` | Include IR lines |
| `emit_asm` | `false` | Include Sigma16 assembly |
| `allocator` | `"advanced"` | Register allocator (`"basic"` or `"advanced"`) |
| `include_mappings` | `true` | Include per-instruction AST mappings |
| `include_groups` | `true` | Include `by_ast` / `by_component` groupings |

**`compile_snapshot_default(source) → ProgramSnapshot`**

Shorthand for `compile_snapshot(source, {})` with all defaults.

**`version() → string`**

Returns the crate version.

#### ProgramSnapshot

```ts
{
  success: boolean;
  error?: string;
  ir?: string[];                    // IR lines (index-aligned with instr_mappings)
  asm?: string[];                   // Sigma16 assembly lines
  asm_ir_mapping?: (number|null)[]; // Per-ASM-line → IR instruction index
  arrays: { name, size, initial_values? }[];
  ast_spans: { ast_node_id, kind, start_byte, end_byte, start_line, start_col, end_line, end_col }[];
  instr_mappings: { instr_index, ast_node_id, component?, description }[];
  by_ast: { ast_node_id, instr_indices }[];
  by_component: { component, instr_indices }[];
}
```

All positions are 0-based. Byte offsets are half-open `[start, end)`.

#### Building

```sh
wasm-pack build --target web
```
