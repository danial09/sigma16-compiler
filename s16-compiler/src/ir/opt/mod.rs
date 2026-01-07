//! IR Optimizations.
//!
//! This module provides a framework for running optimization passes on the ProgramIR.

use crate::ir::ProgramIR;

pub mod constant_folding;

/// A trait for an IR optimization pass.
pub trait IrPass {
    fn run(&mut self, program: &mut ProgramIR);
}

/// Manages and executes a sequence of IR passes.
pub struct PassManager {
    passes: Vec<Box<dyn IrPass>>,
}

impl PassManager {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn add_pass(&mut self, pass: Box<dyn IrPass>) {
        self.passes.push(pass);
    }

    pub fn run_passes(&mut self, program: &mut ProgramIR) {
        for pass in &mut self.passes {
            pass.run(program);
        }
    }
}

pub fn optimize(program: &mut ProgramIR) {
    let mut pm = PassManager::new();
    pm.add_pass(Box::new(constant_folding::ConstantFolder));
    pm.run_passes(program);
}
