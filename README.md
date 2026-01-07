# Sigma16 Compiler

This is a Rust-based compiler for [Sigma16](https://jtod.github.io/home/Sigma16/), a computer architecture developed by John T. O'Donnell.
This project was developed as part of my MSc Thesis at the University of Glasgow.

Sigma16 is a 16-bit RISC-based architecture designed for educational purposes, with a focus on simplicity and ease of understanding. It is particularly well-suited for teaching computer architecture concepts and is often used in introductory courses on the subject.

This compiler translates a simple imperative language into Sigma16 assembly code.
It is organised into three main Rust crates:
- `s16-compiler`: Core library.
- `s16-cli`: Command-line interface for the compiler.
- `s16-wasm`: WebAssembly module for the compiler.

# Usage

Install Rust and clone the repository.

To compile a program, navigate to the project directory and run:
```sh
cargo run --bin s16-cli -- path/to/program
```

Additionally, you can pass the `--tui` flag to run the compiler in a more interactive mode.

# Building for WebAssembly

To build the WebAssembly module, first install the [wasm-pack](https://drager.github.io/wasm-pack/installer/) tool.
Then, navigate to the `s16-wasm` directory and run:
```sh
wasm-pack build --target web
```

# License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.