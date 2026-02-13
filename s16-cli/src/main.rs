use clap::{Parser, ValueEnum};
use s16_compiler::backend::sigma16::{AllocatorKind, compile_ir_to_sigma16_with_allocator};
use s16_compiler::compile_to_ir;
use std::fs;
use std::path::PathBuf;

mod tui;

#[derive(Parser)]
#[command(name = "compiler")]
#[command(about = "A compiler the Sigma16 Architecture")]
struct Args {
    /// Path to the source file to compile
    file: Option<PathBuf>,

    /// Enable source map testing and display
    #[arg(long)]
    test_source_map: bool,

    /// Launch the interactive terminal UI
    #[arg(long)]
    tui: bool,

    /// Emit IR (Intermediate Representation). If none of --ir/--asm/--both is given, defaults to --asm.
    #[arg(long)]
    ir: bool,

    /// Emit Sigma16 assembly
    #[arg(long)]
    asm: bool,

    /// Emit both IR and Sigma16 assembly
    #[arg(long)]
    both: bool,

    /// Register allocation strategy for assembly generation
    /// Options: basic, advanced (default: advanced)
    #[arg(long, value_enum, default_value_t = AllocOpt::Advanced)]
    alloc: AllocOpt,
}

#[derive(Copy, Clone, Debug, ValueEnum)]
enum AllocOpt {
    Basic,
    Advanced,
}

fn main() {
    let args = Args::parse();

    if args.tui {
        // Launch TUI mode
        let initial_text = match &args.file {
            Some(path) => fs::read_to_string(path).unwrap_or_default(),
            None => DEFAULT_SAMPLE.trim().to_string(),
        };
        let _ = tui::run_tui(args.file, initial_text);
        return;
    }

    let src = if let Some(file_path) = args.file {
        fs::read_to_string(&file_path).unwrap_or_else(|e| {
            eprintln!("Error reading file '{}': {}", file_path.display(), e);
            std::process::exit(1);
        })
    } else {
        DEFAULT_SAMPLE.trim().to_string()
    };

    // Determine outputs
    let mut want_ir = args.ir;
    let mut want_asm = args.asm;
    if args.both {
        want_ir = true;
        want_asm = true;
    }
    if !want_ir && !want_asm {
        want_asm = true;
    } // default to assembly

    // Compile to IR once
    let ir = compile_to_ir(&src).unwrap_or_else(|e| {
        eprintln!("Compilation error: {}", e);
        std::process::exit(1);
    });

    // Conditionally print IR
    if want_ir {
        for line in ir.to_lines() {
            println!("{}", line);
        }
        println!();
        if args.test_source_map {
            test_source_map(&ir);
        }
    }

    // Conditionally build and print assembly
    if want_asm {
        let kind = match args.alloc {
            AllocOpt::Basic => AllocatorKind::Basic,
            AllocOpt::Advanced => AllocatorKind::Advanced,
        };
        let asm = compile_ir_to_sigma16_with_allocator(kind, &ir);
        println!("{}", asm);
    }
}

fn test_source_map(ir: &s16_compiler::ir::ProgramIR) {
    use s16_compiler::ir::{AstNodeId, ControlFlowComponent};

    println!("========== Source Map Tests ==========\n");

    // Test 1: Show all mappings
    println!("Test 1: All IR -> AST Mappings");
    println!("--------------------------------");
    for (idx, instr) in ir.instrs.iter().enumerate() {
        let mappings = ir.source_map.get_mappings_for_instr(idx);
        if !mappings.is_empty() {
            println!("Instruction {}: {:?}", idx, instr);
            for mapping in mappings {
                println!(
                    "  -> AST Node {:?}, Component: {:?}",
                    mapping.ast_node_id, mapping.component
                );
                println!("     Description: {}", mapping.description);
            }
        }
    }
    println!();

    // Test 2: Find all instructions for a specific AST node
    println!("Test 2: Find all IR instructions for AST Node 15");
    println!("------------------------------------------------");
    let ast_node_id = AstNodeId(15);
    let instrs = ir.source_map.get_instrs_for_ast(ast_node_id);
    if instrs.is_empty() {
        println!("No instructions found for AST Node {:?}", ast_node_id);
    } else {
        println!(
            "AST Node {:?} maps to {} instruction(s):",
            ast_node_id,
            instrs.len()
        );
        for idx in instrs {
            println!("  [{}] {:?}", idx, ir.instrs[idx]);
        }
    }
    println!();

    // Test 3: Find all condition-related instructions
    println!("Test 3: All Condition Instructions");
    println!("-----------------------------------");
    let condition_instrs = ir
        .source_map
        .get_instrs_for_component(ControlFlowComponent::Condition);
    println!("Found {} condition instruction(s):", condition_instrs.len());
    for idx in condition_instrs {
        let mappings = ir.source_map.get_mappings_for_instr(idx);
        println!("  [{}] {:?}", idx, ir.instrs[idx]);
        for mapping in mappings {
            println!("      AST Node: {:?}", mapping.ast_node_id);
        }
    }
    println!();

    // Test 4: Find all then-branch instructions
    println!("Test 4: All Then-Branch Instructions");
    println!("-------------------------------------");
    let then_instrs = ir
        .source_map
        .get_instrs_for_component(ControlFlowComponent::ThenBranch);
    println!("Found {} then-branch instruction(s):", then_instrs.len());
    for idx in then_instrs {
        println!("  [{}] {:?}", idx, ir.instrs[idx]);
    }
    println!();

    // Test 5: Find all else-branch instructions
    println!("Test 5: All Else-Branch Instructions");
    println!("-------------------------------------");
    let else_instrs = ir
        .source_map
        .get_instrs_for_component(ControlFlowComponent::ElseBranch);
    if else_instrs.is_empty() {
        println!("No else-branch instructions found");
    } else {
        println!("Found {} else-branch instruction(s):", else_instrs.len());
        for idx in else_instrs {
            println!("  [{}] {:?}", idx, ir.instrs[idx]);
        }
    }
    println!();

    // Test 6: Find all loop body instructions
    println!("Test 6: All Loop Body Instructions");
    println!("-----------------------------------");
    let loop_instrs = ir
        .source_map
        .get_instrs_for_component(ControlFlowComponent::LoopBody);
    if loop_instrs.is_empty() {
        println!("No loop body instructions found");
    } else {
        println!("Found {} loop body instruction(s):", loop_instrs.len());
        for idx in loop_instrs {
            println!("  [{}] {:?}", idx, ir.instrs[idx]);
        }
    }
    println!();

    // Test 7: Find all control flow glue instructions
    println!("Test 7: All Control Flow Glue Instructions");
    println!("-------------------------------------------");
    let glue_instrs = ir
        .source_map
        .get_instrs_for_component(ControlFlowComponent::ControlFlowGlue);
    println!(
        "Found {} control flow glue instruction(s):",
        glue_instrs.len()
    );
    for idx in glue_instrs {
        println!("  [{}] {:?}", idx, ir.instrs[idx]);
    }
    println!();

    // Test 8: Group instructions by AST node
    println!("Test 8: Instructions Grouped by AST Node");
    println!("-----------------------------------------");
    use std::collections::HashMap;
    let mut grouped: HashMap<AstNodeId, Vec<usize>> = HashMap::new();
    for (idx, _) in ir.instrs.iter().enumerate() {
        let mappings = ir.source_map.get_mappings_for_instr(idx);
        for mapping in mappings {
            grouped
                .entry(mapping.ast_node_id)
                .or_insert_with(Vec::new)
                .push(idx);
        }
    }

    let mut sorted_nodes: Vec<_> = grouped.keys().collect();
    sorted_nodes.sort_by_key(|k| k.0);

    for node_id in sorted_nodes {
        let instrs = &grouped[node_id];
        println!("AST Node {:?} -> {} instruction(s):", node_id, instrs.len());
        for &idx in instrs {
            let mappings = ir.source_map.get_mappings_for_instr(idx);
            let component = mappings.first().and_then(|m| m.component);
            println!(
                "  [{}] {:?} (Component: {:?})",
                idx, ir.instrs[idx], component
            );
        }
        println!();
    }

    // Test 9: Position-based lookup (0-based line, column)
    println!("Test 9: Position-based queries");
    println!("--------------------------------");
    let queries = [
        (0usize, 0usize), // start of file
        (3, 3),           // within an if condition, typically
        (8, 4),           // inside loop body if present
    ];
    for (line, col) in queries {
        if let Some(info) = ir.source_map.get_ast_info_at(line, col) {
            let instrs = ir.source_map.get_instrs_for_ast(info.id);
            println!(
                "At (line {}, col {}): AST {:?} span=({}-{}), kind={:?}, IR instrs={:?}",
                line, col, info.id, info.span.start, info.span.end, info.kind, instrs
            );
        } else {
            println!("At (line {}, col {}): no AST node found", line, col);
        }
    }
}

const DEFAULT_SAMPLE: &str = r#"
x = 5;
y = 10;
if x < y {
    z = x + y;
} else {
    z = x - y;
}

while z > 0 {
    z = z - 1;
    w = z * x + 2;
}
result = z + 100;

for i from 1 to 10 {
  result = result + i;
}
"#;
