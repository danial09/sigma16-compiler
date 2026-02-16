use s16_compiler::{CompileError, SemanticErrorKind, compile_to_ir};

// ── Semantic error detection ─────────────────────────────────────────────
// Each test verifies that an invalid program produces the correct error kind.

#[test]
fn array_redefinition() {
    let source = r#"
        array x[5];
        array x[10];
    "#;
    let result = compile_to_ir(source);
    assert_semantic_error(result, SemanticErrorKind::ArrayRedefinition);
}

#[test]
fn undefined_function() {
    let source = "x = foo();";
    let result = compile_to_ir(source);
    assert_semantic_error(result, SemanticErrorKind::UndefinedFunction);
}

#[test]
fn return_outside_function() {
    let source = r#"
        x = 5;
        return x;
    "#;
    let result = compile_to_ir(source);
    assert_semantic_error(result, SemanticErrorKind::ReturnOutsideFunction);
}

#[test]
fn argument_count_mismatch() {
    let source = r#"
        fn add(a, b) { return a + b; }
        x = add(1, 2, 3);
    "#;
    let result = compile_to_ir(source);
    assert_semantic_error(result, SemanticErrorKind::ArgumentCountMismatch);
}

#[test]
fn too_few_arguments() {
    let source = r#"
        fn add(a, b) { return a + b; }
        x = add(1);
    "#;
    let result = compile_to_ir(source);
    assert_semantic_error(result, SemanticErrorKind::ArgumentCountMismatch);
}

#[test]
fn undefined_array() {
    let source = "y = somearray[3];";
    let result = compile_to_ir(source);
    assert_semantic_error(result, SemanticErrorKind::UndefinedArray);
}

#[test]
fn function_used_as_array() {
    let source = r#"
        fn foo() { return 1; }
        x = foo[3];
    "#;
    let result = compile_to_ir(source);
    assert!(result.is_err(), "Function used as array should error");
}

// ── Error location accuracy ─────────────────────────────────────────────

#[test]
fn error_location_single_line() {
    let source = "x = unknown_func();";
    let result = compile_to_ir(source);
    assert!(result.is_err());
    if let Err(CompileError::Semantic { line, col, .. }) = result {
        assert_eq!(line, 1, "Error should be on line 1, got {}", line);
        assert!(col >= 1, "Error column should be >= 1, got {}", col);
    } else {
        panic!("Expected semantic error");
    }
}

#[test]
fn error_location_multiline() {
    let source = "x = 1;\ny = 2;\nz = not_a_func(1, 2);\n";
    let result = compile_to_ir(source);
    assert!(result.is_err());
    if let Err(CompileError::Semantic { line, col, .. }) = result {
        assert_eq!(line, 3, "Error should be on line 3, got {}", line);
        assert!(col > 1, "Error column should be > 1, got {}", col);
    } else {
        panic!("Expected semantic error");
    }
}

// ── Helper ───────────────────────────────────────────────────────────────

fn assert_semantic_error(
    result: Result<s16_compiler::ir::ProgramIR, CompileError>,
    expected: SemanticErrorKind,
) {
    match result {
        Ok(_) => panic!("Expected {:?} error, but compilation succeeded", expected),
        Err(CompileError::Semantic { kind, .. }) => {
            assert_eq!(kind, expected);
        }
        Err(other) => panic!("Expected {:?} semantic error, got: {:?}", expected, other),
    }
}
