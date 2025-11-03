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
            y = 6
            if x > 7 {
                y = 6 * x + y
            }
            z = x + y

            if z > 9 {
                z = z + 5
            }
        "#.to_string()
    };

    let ir = compile_to_ir(&src).unwrap_or_else(|e| {
        eprintln!("Compilation error: {}", e);
        std::process::exit(1);
    });

    for line in ir.to_lines() {
        println!("{line}");
    }
}
