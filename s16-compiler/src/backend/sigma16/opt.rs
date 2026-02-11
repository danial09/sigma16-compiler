use super::codegen::item::{AsmItem, Disp, S16Instr};
use super::abi::Register;

pub trait AsmPass {
    fn run(&self, items: &mut Vec<AsmItem>);
}

pub struct PassManager {
    passes: Vec<Box<dyn AsmPass>>,
}

impl PassManager {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn add_pass(&mut self, pass: Box<dyn AsmPass>) {
        self.passes.push(pass);
    }

    pub fn run(&self, items: &mut Vec<AsmItem>) {
        for pass in &self.passes {
            pass.run(items);
        }
    }
}

/// Removes jumps to the immediately following label.
pub struct JumpOptimizer;

impl AsmPass for JumpOptimizer {
    fn run(&self, items: &mut Vec<AsmItem>) {
        let mut i = 0;
        while i + 1 < items.len() {
            let mut remove = false;
            if let AsmItem::Instr {
                instr: S16Instr::Jump { disp: Disp::Label(target), .. },
                ..
            } = &items[i]
            {
                if let AsmItem::Label(label_name, _) = &items[i + 1] {
                    if target == label_name {
                        remove = true;
                    }
                }
            }
            if remove {
                items.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

/// Omits prologue and epilogue for leaf functions with no stack usage.
pub struct PrologueEpilogueOptimizer;

impl AsmPass for PrologueEpilogueOptimizer {
    fn run(&self, items: &mut Vec<AsmItem>) {
        for item in items.iter_mut() {
            if let AsmItem::Function {
                prologue,
                epilogue,
                frame_size,
                used_callee,
                is_leaf,
                ..
            } = item
            {
                // If it's a leaf function and doesn't use any stack space beyond R14
                // and doesn't use any callee-saved registers.
                // frame_size here is max_slots.
                if *is_leaf && *frame_size == 0 && used_callee.is_empty() {
                    prologue.clear();

                    // The epilogue normally contains:
                    // 1. restore callee-saved
                    // 2. lea R14, -frame[R14]
                    // 3. load R13, 0[R14]
                    // 4. jump 0[R13]

                    // We want to keep only the final jump 0[R13].
                    if let Some(jump_item) = epilogue.last().cloned() {
                        if let AsmItem::Instr {
                            instr: S16Instr::Jump { disp: Disp::Num(0), .. },
                            ..
                        } = &jump_item
                        {
                            epilogue.clear();
                            epilogue.push(jump_item);
                        }
                    }
                }
            }
        }
    }
}

/// Removes redundant instructions like `add Rx,R0,Rx` and store/load pairs
pub struct PeepholeOptimizer;

impl AsmPass for PeepholeOptimizer {
    fn run(&self, items: &mut Vec<AsmItem>) {
        Self::optimize_function_bodies(items);
    }
}

impl PeepholeOptimizer {
    fn optimize_function_bodies(items: &mut Vec<AsmItem>) {
        for item in items.iter_mut() {
            if let AsmItem::Function { body, .. } = item {
                Self::optimize_instructions(body);
            }
        }
        // Also optimize top-level instructions
        Self::optimize_instructions(items);
    }

    fn optimize_instructions(instrs: &mut Vec<AsmItem>) {
        let mut i = 0;
        while i < instrs.len() {
            let mut remove = false;

            if let AsmItem::Instr {
                instr: S16Instr::Add { d, a, b },
                ir_map,
            } = &instrs[i]
            {
                // Remove redundant `add Rx,R0,Rx` (identity move)
                if *a == Register::ZERO_REG && b == d {
                    // Preserve mapping-bearing instructions
                    if ir_map.is_none() {
                        remove = true;
                    }
                }
            }

            if remove {
                instrs.remove(i);
            } else {
                i += 1;
            }
        }
    }
}

pub fn optimize(items: &mut Vec<AsmItem>) {
    let mut pm = PassManager::new();
    pm.add_pass(Box::new(PeepholeOptimizer));
    pm.add_pass(Box::new(JumpOptimizer));
    pm.add_pass(Box::new(PrologueEpilogueOptimizer));
    pm.run(items);
}
