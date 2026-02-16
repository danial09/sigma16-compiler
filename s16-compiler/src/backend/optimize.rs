//! Assembly-level optimization passes.
//!
//! Each pass implements the `AsmPass` trait and operates on a `Vec<AsmItem>`.
//! Passes are composed via the `PassManager` and run after code generation
//! but before the final text emission.

use super::abi::Register;
use super::instruction::{AsmItem, Disp, S16Instr};

// ============================================================================
// Pass infrastructure
// ============================================================================

/// A single optimization pass over assembly output.
pub trait AsmPass {
    fn run(&self, items: &mut Vec<AsmItem>);
}

/// Runs a sequence of `AsmPass`es in order.
struct PassManager {
    passes: Vec<Box<dyn AsmPass>>,
}

impl PassManager {
    fn new() -> Self {
        Self { passes: Vec::new() }
    }

    fn add(&mut self, pass: Box<dyn AsmPass>) {
        self.passes.push(pass);
    }

    fn run_all(&self, items: &mut Vec<AsmItem>) {
        for pass in &self.passes {
            pass.run(items);
        }
    }
}

// ============================================================================
// Peephole optimizer — removes redundant instructions
// ============================================================================

/// Removes redundant instructions like `add Rx,R0,Rx` (identity moves).
struct PeepholeOptimizer;

impl AsmPass for PeepholeOptimizer {
    fn run(&self, items: &mut Vec<AsmItem>) {
        Self::optimize_function_bodies(items);
    }
}

impl PeepholeOptimizer {
    fn optimize_function_bodies(items: &mut Vec<AsmItem>) {
        for item in items.iter_mut() {
            if let AsmItem::Function { body, .. } = item {
                Self::remove_identity_moves(body);
            }
        }
        // Also optimize top-level instructions.
        Self::remove_identity_moves(items);
    }

    fn remove_identity_moves(instrs: &mut Vec<AsmItem>) {
        instrs.retain(|item| {
            if let AsmItem::Instr {
                instr: S16Instr::Add { d, a, b },
                ..
            } = item
            {
                // `add Rx,R0,Rx` is a no-op.
                !(*a == Register::ZERO_REG && b == d)
            } else {
                true
            }
        });
    }
}

// ============================================================================
// Jump optimizer — removes fallthrough jumps
// ============================================================================

/// Removes `jump X` instructions that are immediately followed by label `X`.
struct JumpOptimizer;

impl JumpOptimizer {
    /// Remove any `jump X` immediately followed by label `X` in a flat list.
    fn eliminate_fallthrough_jumps(items: &mut Vec<AsmItem>) {
        let mut i = 0;
        while i + 1 < items.len() {
            let mut remove = false;
            if let AsmItem::Instr {
                instr:
                    S16Instr::Jump {
                        disp: Disp::Label(target),
                        ..
                    },
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

impl AsmPass for JumpOptimizer {
    fn run(&self, items: &mut Vec<AsmItem>) {
        // Optimize inside function bodies and at body→epilogue boundary.
        for item in items.iter_mut() {
            if let AsmItem::Function { body, epilogue, .. } = item {
                Self::eliminate_fallthrough_jumps(body);
                Self::eliminate_fallthrough_jumps(epilogue);

                // Check boundary: last body instruction jumping to first epilogue label.
                if let Some(AsmItem::Instr {
                    instr:
                        S16Instr::Jump {
                            disp: Disp::Label(target),
                            ..
                        },
                    ..
                }) = body.last()
                {
                    if let Some(AsmItem::Label(label_name, _)) = epilogue.first() {
                        if target == label_name {
                            body.pop();
                        }
                    }
                }
            }
        }
        // Optimize top-level items.
        Self::eliminate_fallthrough_jumps(items);
    }
}

// ============================================================================
// Prologue/epilogue optimizer — omits frame setup for trivial leaf functions
// ============================================================================

/// Omits prologue and epilogue for leaf functions with no stack usage and no
/// callee-saved register pressure.
struct PrologueEpilogueOptimizer;

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
                if *is_leaf && *frame_size == 0 && used_callee.is_empty() {
                    prologue.clear();

                    // Keep the epilogue label (for early returns) and the
                    // final `jump 0[R13]`, discard everything else.
                    if let Some(jump_item) = epilogue.last().cloned() {
                        if let AsmItem::Instr {
                            instr:
                                S16Instr::Jump {
                                    disp: Disp::Num(0),
                                    idx: Register::LINK_REG,
                                },
                            ..
                        } = &jump_item
                        {
                            let labels: Vec<AsmItem> = epilogue
                                .iter()
                                .take_while(|it| matches!(it, AsmItem::Label(..)))
                                .cloned()
                                .collect();
                            epilogue.clear();
                            epilogue.extend(labels);
                            epilogue.push(jump_item);
                        }
                    }
                }
            }
        }
    }
}

// ============================================================================
// Return inliner — inlines trivial epilogues into function bodies
// ============================================================================

/// For functions where the epilogue is just `ret_<name>: jump 0[R13]`,
/// replaces each `jump ret_<name>` in the body with `jump 0[R13]` directly,
/// then removes the now-unreferenced label from the epilogue.
struct ReturnInliner;

impl ReturnInliner {
    /// Check if an item is `jump <label>[R0]` targeting `ret_label`.
    fn is_jump_to_ret(item: &AsmItem, ret_label: &str) -> bool {
        matches!(
            item,
            AsmItem::Instr {
                instr: S16Instr::Jump {
                    disp: Disp::Label(target),
                    idx,
                },
                ..
            } if target == ret_label && *idx == Register::ZERO_REG
        )
    }
}

impl AsmPass for ReturnInliner {
    fn run(&self, items: &mut Vec<AsmItem>) {
        for item in items.iter_mut() {
            if let AsmItem::Function {
                name,
                epilogue,
                body,
                ..
            } = item
            {
                // Check if epilogue is exactly [Label("ret_<name>"), jump 0[R13]]
                let ret_label = format!("ret_{}", name);
                let is_trivial_epilogue = epilogue.len() == 2
                    && matches!(&epilogue[0], AsmItem::Label(lbl, _) if lbl == &ret_label)
                    && matches!(
                        &epilogue[1],
                        AsmItem::Instr {
                            instr: S16Instr::Jump {
                                disp: Disp::Num(0),
                                idx,
                            },
                            ..
                        } if *idx == Register::LINK_REG
                    );

                if !is_trivial_epilogue {
                    continue;
                }

                // Replace each `jump ret_<name>` in the body with `jump 0[R13]`
                for body_item in body.iter_mut() {
                    if Self::is_jump_to_ret(body_item, &ret_label) {
                        // Preserve the ir_map from the original instruction
                        let ir_map = if let AsmItem::Instr { ir_map, .. } = body_item {
                            *ir_map
                        } else {
                            None
                        };
                        *body_item = AsmItem::Instr {
                            instr: S16Instr::Jump {
                                disp: Disp::Num(0),
                                idx: Register::LINK_REG,
                            },
                            comment: Some("return".to_string()),
                            ir_map,
                        };
                    }
                }

                // Remove the label from the epilogue (keep only the final jump)
                epilogue.remove(0);
            }
        }
    }
}

// ============================================================================
// Public entry point
// ============================================================================

/// Run all optimization passes on the assembly output.
pub fn optimize(items: &mut Vec<AsmItem>) {
    let mut pm = PassManager::new();
    pm.add(Box::new(PeepholeOptimizer));
    pm.add(Box::new(JumpOptimizer));
    pm.add(Box::new(PrologueEpilogueOptimizer));
    pm.add(Box::new(ReturnInliner));
    // Re-run jump optimizer to eliminate fallthrough jumps created by inlining.
    pm.add(Box::new(JumpOptimizer));
    pm.run_all(items);
}
