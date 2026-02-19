use s16_compiler::compile_to_sigma16;

// ── Sample program compilation ───────────────────────────────────────────
// Verify that sample .sig files compile to Sigma16 assembly without errors.

#[test]
fn compile_paf_sig() {
    let source =
        std::fs::read_to_string("../samples/paf.sig").expect("Failed to read samples/paf.sig");
    let asm = compile_to_sigma16(&source).expect("paf.sig should compile");
    assert!(!asm.is_empty());
}

#[test]
fn compile_recursion_sig() {
    let source = std::fs::read_to_string("../samples/recursion.sig")
        .expect("Failed to read samples/recursion.sig");
    let asm = compile_to_sigma16(&source).expect("recursion.sig should compile");
    // Recursive function must use stack management
    assert!(
        asm.contains("store"),
        "Recursive function should save return address"
    );
    assert!(asm.contains("jal"), "Should have function call instruction");
}

#[test]
fn compile_bubble_sort_sig() {
    let source = std::fs::read_to_string("../samples/bubble_sort.sig")
        .expect("Failed to read samples/bubble_sort.sig");
    let asm = compile_to_sigma16(&source).expect("bubble_sort.sig should compile");
    assert!(!asm.is_empty());
}

#[test]
fn compile_gcd_sig() {
    let source =
        std::fs::read_to_string("../samples/gcd.sig").expect("Failed to read samples/gcd.sig");
    let asm = compile_to_sigma16(&source).expect("gcd.sig should compile");
    assert!(!asm.is_empty());
}

#[test]
fn compile_is_prime_sig() {
    let source = std::fs::read_to_string("../samples/is_prime.sig")
        .expect("Failed to read samples/is_prime.sig");
    let asm = compile_to_sigma16(&source).expect("is_prime.sig should compile");
    assert!(!asm.is_empty());
}

#[test]
fn compile_strlen_sig() {
    let source = std::fs::read_to_string("../samples/strlen.sig")
        .expect("Failed to read samples/strlen.sig");
    let asm = compile_to_sigma16(&source).expect("strlen.sig should compile");
    assert!(!asm.is_empty());
}

#[test]
fn compile_sum_digits_sig() {
    let source = std::fs::read_to_string("../samples/sum_digits.sig")
        .expect("Failed to read samples/sum_digits.sig");
    let asm = compile_to_sigma16(&source).expect("sum_digits.sig should compile");
    assert!(!asm.is_empty());
}

#[test]
fn compile_bitwise_sig() {
    let source = std::fs::read_to_string("../samples/bitwise.sig")
        .expect("Failed to read samples/bitwise.sig");
    let asm = compile_to_sigma16(&source).expect("bitwise.sig should compile");
    assert!(!asm.is_empty());
}

// ── Codegen structural properties ────────────────────────────────────────

#[test]
fn no_redundant_jumps() {
    // A jump to the immediately following label is redundant and should be optimized out.
    let source = "fn main() { if 1 == 1 { return 1; } return 0; }";
    let asm = compile_to_sigma16(source).expect("Compilation failed");

    let lines: Vec<&str> = asm.lines().map(|l| l.trim()).collect();
    for i in 0..lines.len().saturating_sub(1) {
        if lines[i].starts_with("jump ") {
            let target = &lines[i][5..];
            let next_label = format!("{}:", target);
            assert_ne!(
                lines[i + 1],
                next_label,
                "Redundant jump to next label '{}' found",
                target
            );
        }
    }
}

#[test]
fn function_codegen_has_return() {
    let source = "fn func() { return 1; }";
    let asm = compile_to_sigma16(source).expect("Compilation failed");
    assert!(asm.contains("jump"), "Function should have a return jump");
}

#[test]
fn pointer_param_codegen_has_load_store() {
    let source = r#"
        fn swap(arr, i, j) {
            temp = arr[i];
            arr[i] = arr[j];
            arr[j] = temp;
            return 0;
        }
    "#;
    let asm = compile_to_sigma16(source).expect("Compilation failed");
    assert!(asm.contains("add"), "Pointer arithmetic should use add");
    assert!(asm.contains("load"), "Should have load instructions");
    assert!(asm.contains("store"), "Should have store instructions");
}

#[test]
fn bubble_sort_end_to_end() {
    let source = r#"
        array data[5] = [5, 3, 1, 4, 2];
        fn bubble_sort(arr, n) {
            i = 0;
            while i < n - 1 {
                j = 0;
                while j < n - i - 1 {
                    if arr[j] > arr[j + 1] {
                        temp = arr[j];
                        arr[j] = arr[j + 1];
                        arr[j + 1] = temp;
                    }
                    j = j + 1;
                }
                i = i + 1;
            }
            return arr;
        }
        result = bubble_sort(data, 5);
    "#;
    let asm = compile_to_sigma16(source).expect("Compilation failed");
    assert!(
        asm.contains("bubble_sort"),
        "Should contain bubble_sort label"
    );
    assert!(
        asm.contains("data   5"),
        "Should contain initial array data"
    );
}
