mod compiler;
mod convert;
pub mod types;

use types::CompileOptions;
use wasm_bindgen::prelude::*;

#[cfg(feature = "console_error_panic_hook")]
pub use console_error_panic_hook::set_once as set_panic_hook;

#[wasm_bindgen(start)]
pub fn init() {
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();
}

/// Compile source to a snapshot with the given options.
#[wasm_bindgen]
pub fn compile_snapshot(source: &str, options: JsValue) -> JsValue {
    let opts: CompileOptions = serde_wasm_bindgen::from_value(options).unwrap_or_default();
    let snapshot = compiler::compile_snapshot_internal(source, &opts);
    serde_wasm_bindgen::to_value(&snapshot).unwrap()
}

/// Compile with defaults: IR included, ASM omitted, mappings and groupings included.
#[wasm_bindgen]
pub fn compile_snapshot_default(source: &str) -> JsValue {
    let snapshot = compiler::compile_snapshot_internal(source, &CompileOptions::default());
    serde_wasm_bindgen::to_value(&snapshot).unwrap()
}

#[wasm_bindgen]
pub fn version() -> String {
    env!("CARGO_PKG_VERSION").to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use types::ProgramSnapshot;
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
        assert!(
            snap.instr_mappings
                .iter()
                .any(|m| !m.description.is_empty())
        );
    }
}
