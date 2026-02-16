use s16_compiler::backend::compile_ir_to_sigma16;
use s16_compiler::compile_to_ir;

// ── Arrays ───────────────────────────────────────────────────────────────

#[test]
fn array_initialized() {
    let source = "array a[4] = [1, 2, 3, 4];";
    let ir = compile_to_ir(source).unwrap();

    assert_eq!(ir.arrays.len(), 1);
    assert_eq!(ir.arrays[0].0, "a");
    assert_eq!(ir.arrays[0].1, 4);
    assert_eq!(ir.arrays[0].2, Some(vec![1, 2, 3, 4]));

    let asm = compile_ir_to_sigma16(&ir);
    for val in &["data   1", "data   2", "data   3", "data   4"] {
        assert!(asm.contains(val), "ASM should contain '{}'", val);
    }
}

#[test]
fn array_uninitialized() {
    let source = "array a[3];";
    let ir = compile_to_ir(source).unwrap();

    assert_eq!(ir.arrays.len(), 1);
    assert_eq!(ir.arrays[0].0, "a");
    assert_eq!(ir.arrays[0].1, 3);
    assert_eq!(ir.arrays[0].2, None);

    let asm = compile_ir_to_sigma16(&ir);
    let zero_count = asm.matches("data   0").count();
    assert!(
        zero_count >= 3,
        "Uninitialized array of size 3 should have >= 3 zero entries"
    );
}

#[test]
fn array_negative_initialization() {
    let source = "array a[3] = [-1, 0, -5];";
    let ir = compile_to_ir(source).unwrap();
    assert_eq!(ir.arrays[0].2, Some(vec![-1, 0, -5]));
}

#[test]
fn array_decay_to_pointer() {
    // Using an array name as a value should succeed (decays to address).
    let source = r#"
        array arr[5];
        ptr = arr;
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "Array decay to pointer should compile: {:?}",
        result.err()
    );
}

#[test]
fn array_passed_to_function() {
    let source = r#"
        array data[10];
        fn sum(arr, n) {
            total = 0;
            i = 0;
            while i < n {
                total = total + arr[i];
                i = i + 1;
            }
            return total;
        }
        result = sum(data, 10);
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "Passing array to function should compile: {:?}",
        result.err()
    );
}

// ── Strings ──────────────────────────────────────────────────────────────

#[test]
fn string_declaration() {
    let source = r#"s = "abc";"#;
    let ir = compile_to_ir(source).unwrap();

    // "abc" -> [97, 98, 99, 0] (null-terminated)
    assert_eq!(ir.arrays.len(), 1);
    assert_eq!(ir.arrays[0].0, "s");
    assert_eq!(ir.arrays[0].1, 4);
    assert_eq!(ir.arrays[0].2, Some(vec![97, 98, 99, 0]));
}

#[test]
fn empty_string() {
    let source = r#"a = "";"#;
    let ir = compile_to_ir(source).unwrap();
    assert_eq!(ir.arrays[0].1, 1);
    assert_eq!(ir.arrays[0].2, Some(vec![0]));
}

#[test]
fn multiple_strings() {
    let source = r#"
        a = "";
        b = "X";
    "#;
    let ir = compile_to_ir(source).unwrap();

    assert_eq!(ir.arrays[0].0, "a");
    assert_eq!(ir.arrays[0].2, Some(vec![0]));

    assert_eq!(ir.arrays[1].0, "b");
    assert_eq!(ir.arrays[1].2, Some(vec![88, 0])); // 'X' = 88
}

// ── Signed integers ─────────────────────────────────────────────────────

#[test]
fn negative_literals() {
    let source = "x = -5; y = -123;";
    let ir = compile_to_ir(source).expect("Should parse negative literals");
    let ir_text = ir.to_lines().join("\n");
    assert!(
        ir_text.contains("x = -5"),
        "IR should contain x = -5, got:\n{}",
        ir_text
    );
    assert!(
        ir_text.contains("y = -123"),
        "IR should contain y = -123, got:\n{}",
        ir_text
    );
}

#[test]
fn unary_minus() {
    let source = "a = 10; b = -a;";
    let ir = compile_to_ir(source).expect("Should parse unary minus");
    let ir_text = ir.to_lines().join("\n");
    // Unary minus is lowered to subtraction from 0
    assert!(
        ir_text.contains("0 - a"),
        "Unary minus should lower to '0 - a', got:\n{}",
        ir_text
    );
}

// ── Operators ────────────────────────────────────────────────────────────

#[test]
fn ge_le_operators_compile() {
    let source = r#"
        x = 10; y = 5;
        if x >= y { z = 1; } else { z = 0; }
        if y <= x { w = 1; } else { w = 0; }
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        ">=/<= operators should compile: {:?}",
        result.err()
    );
}

// ── Pointers ─────────────────────────────────────────────────────────────

#[test]
fn pointer_address_of_and_deref() {
    let source = r#"
        fn main() {
            x = 5;
            p = &x;
            *p = 10;
        }
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "Pointer operations should compile: {:?}",
        result.err()
    );
}

// ── Functions ────────────────────────────────────────────────────────────

#[test]
fn simple_function() {
    let source = "fn inc(x) { return x + 1; }";
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "Simple function should compile: {:?}",
        result.err()
    );
}

#[test]
fn function_with_array_param_read() {
    let source = r#"
        fn sum(data, n) {
            total = 0;
            i = 0;
            while i < n {
                total = total + data[i];
                i = i + 1;
            }
            return total;
        }
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "Array param read should compile: {:?}",
        result.err()
    );
}

#[test]
fn function_with_array_param_write() {
    let source = r#"
        fn fill(arr, n, val) {
            i = 0;
            while i < n {
                arr[i] = val;
                i = i + 1;
            }
            return 0;
        }
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "Array param write should compile: {:?}",
        result.err()
    );
}

#[test]
fn recursive_function() {
    let source = r#"
        fn fact(n) {
            if n > 1 {
                return n * fact(n - 1);
            } else {
                return 1;
            }
        }
        result = fact(5);
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "Recursive function should compile: {:?}",
        result.err()
    );
}

// ── Control flow ─────────────────────────────────────────────────────────

#[test]
fn if_else() {
    let source = r#"
        x = 5;
        if x > 3 { y = 1; } else { y = 0; }
    "#;
    let result = compile_to_ir(source);
    assert!(result.is_ok(), "If-else should compile: {:?}", result.err());
}

#[test]
fn while_loop() {
    let source = r#"
        z = 0;
        while z < 9 { z = z + 1; }
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "While loop should compile: {:?}",
        result.err()
    );
}

// ── Complete programs ────────────────────────────────────────────────────

#[test]
fn valid_program_with_array_and_function() {
    let source = r#"
        array arr[5];
        fn sum(n) {
            total = 0;
            i = 0;
            while i < n {
                total = total + arr[i];
                i = i + 1;
            }
            return total;
        }
        result = sum(5);
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "Valid program should compile: {:?}",
        result.err()
    );
}

#[test]
fn bubble_sort_compiles() {
    let source = r#"
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
    "#;
    let result = compile_to_ir(source);
    assert!(
        result.is_ok(),
        "Bubble sort should compile: {:?}",
        result.err()
    );
}
