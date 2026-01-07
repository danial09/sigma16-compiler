use super::codegen::item::AsmItem;

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
            if let AsmItem::Instruction { text, .. } = &items[i] {
                let trimmed = text.trim();
                if trimmed.starts_with("jump ") {
                    let target = &trimmed[5..];
                    if let AsmItem::Label(label_name) = &items[i+1] {
                        if label_name == target {
                            remove = true;
                        }
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
            } = item {
                // If it's a leaf function and doesn't use any stack space beyond R14
                // and doesn't use any callee-saved registers.
                // frame_size here is max_slots.
                if *is_leaf && *frame_size == 0 && used_callee.is_empty() {
                    prologue.clear();
                    
                    // The epilogue normally contains:
                    // 1. restore callee-saved
                    // 2. lea R13, frame+1[R13]
                    // 3. load R14, -1[R13]
                    // 4. jump 0[R14]
                    
                    // We want to keep only the jump.
                    if let Some(jump_instr) = epilogue.last().cloned() {
                        if let AsmItem::Instruction { text, .. } = &jump_instr {
                            if text.contains("jump 0[") {
                                epilogue.clear();
                                epilogue.push(jump_instr);
                            }
                        }
                    }
                }
            }
        }
    }
}

pub fn optimize(items: &mut Vec<AsmItem>) {
    let mut pm = PassManager::new();
    pm.add_pass(Box::new(JumpOptimizer));
    pm.add_pass(Box::new(PrologueEpilogueOptimizer));
    pm.run(items);
}
