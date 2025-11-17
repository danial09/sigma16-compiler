
use compiler::compile_to_ir;
use clap::Parser;
use std::fs;
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "compiler")]
#[command(about = "A compiler that generates IR from source code")]
struct Args {
    /// Path to the source file to compile
    file: Option<PathBuf>,

    /// Enable source map testing and display
    #[arg(long)]
    test_source_map: bool,
}

fn main() {
    let args = Args::parse();

    let src = if let Some(file_path) = args.file {
        fs::read_to_string(&file_path)
            .unwrap_or_else(|e| {
                eprintln!("Error reading file '{}': {}", file_path.display(), e);
                std::process::exit(1);
            })
    } else {
        r#"
x = 5
y = 10
if x < y {
    z = x + y
} else {
    z = x - y
}

while z > 0 {
    z = z - 1
    w = z * 2
}
result = z + 100

for i from 1 to 10 {
  result = result + i
}
"#.trim().to_string()
    };

    let ir = compile_to_ir(&src).unwrap_or_else(|e| {
        eprintln!("Compilation error: {}", e);
        std::process::exit(1);
    });

    println!("========== Generated IR ==========");
    for (idx, line) in ir.to_lines().iter().enumerate() {
        println!("{:3}: {}", idx, line);
    }
    println!();

    if args.test_source_map {
        test_source_map(&ir);
    }
}

fn test_source_map(ir: &compiler::ir::ProgramIR) {
    use compiler::ir::{AstNodeId, ControlFlowComponent};

    println!("========== Source Map Tests ==========\n");

    // Test 1: Show all mappings
    println!("Test 1: All IR -> AST Mappings");
    println!("--------------------------------");
    for (idx, instr) in ir.instrs.iter().enumerate() {
        let mappings = ir.source_map.get_mappings_for_instr(idx);
        if !mappings.is_empty() {
            println!("Instruction {}: {:?}", idx, instr);
            for mapping in mappings {
                println!("  -> AST Node {:?}, Component: {:?}",
                         mapping.ast_node_id,
                         mapping.component);
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
        println!("AST Node {:?} maps to {} instruction(s):", ast_node_id, instrs.len());
        for idx in instrs {
            println!("  [{}] {:?}", idx, ir.instrs[idx]);
        }
    }
    println!();

    // Test 3: Find all condition-related instructions
    println!("Test 3: All Condition Instructions");
    println!("-----------------------------------");
    let condition_instrs = ir.source_map.get_instrs_for_component(ControlFlowComponent::Condition);
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
    let then_instrs = ir.source_map.get_instrs_for_component(ControlFlowComponent::ThenBranch);
    println!("Found {} then-branch instruction(s):", then_instrs.len());
    for idx in then_instrs {
        println!("  [{}] {:?}", idx, ir.instrs[idx]);
    }
    println!();

    // Test 5: Find all else-branch instructions
    println!("Test 5: All Else-Branch Instructions");
    println!("-------------------------------------");
    let else_instrs = ir.source_map.get_instrs_for_component(ControlFlowComponent::ElseBranch);
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
    let loop_instrs = ir.source_map.get_instrs_for_component(ControlFlowComponent::LoopBody);
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
    let glue_instrs = ir.source_map.get_instrs_for_component(ControlFlowComponent::ControlFlowGlue);
    println!("Found {} control flow glue instruction(s):", glue_instrs.len());
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
            grouped.entry(mapping.ast_node_id)
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
            println!("  [{}] {:?} (Component: {:?})", idx, ir.instrs[idx], component);
        }
        println!();
    }
}
