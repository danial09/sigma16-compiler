//! Code generation state machine and assembly output.
//!
//! Contains the [`Codegen`] struct (the central code-generation driver),
//! buffer management helpers, and final assembly emission.

mod lowering;
mod program;

use super::abi::Register;
use super::instruction::{AsmItem, AnnotatedInstr, Disp, S16Instr};
use super::regalloc::{GreedyRegAllocator, RegAllocator};
use crate::ir::{Value, Var};
use std::collections::HashSet;

// ============================================================================
// Output type
// ============================================================================

/// Final compiled Sigma16 assembly with optional IR mapping.
#[derive(Debug, Clone)]
pub struct Sigma16Asm {
    pub lines: Vec<String>,
    pub asm_ir_mapping: Vec<Option<usize>>,
}

impl Sigma16Asm {
    pub fn join(&self) -> String {
        self.lines.join("\n")
    }
}

// ============================================================================
// Codegen state
// ============================================================================

/// Central code-generation driver.
///
/// Accumulates assembly output from IR instructions, manages register
/// allocation interaction, and produces the final `Sigma16Asm`.
pub struct Codegen {
    pub(crate) out: Vec<AsmItem>,
    pub(crate) reg: Box<dyn RegAllocator>,
    pub(crate) user_vars: Vec<(String, i64, Option<usize>)>,
    pub(crate) user_vars_set: HashSet<String>,
    pub(crate) arrays: Vec<(String, usize, Option<Vec<i64>>, Option<usize>)>,
    pub(crate) emitted_header: bool,
    pub(crate) has_top_level_code: bool,
    pub(crate) top_level_buf: Option<Vec<AsmItem>>,
    pub(crate) toplevel_items: Vec<AsmItem>,
    pub(crate) func_buf: Option<Vec<AsmItem>>,
    pub(crate) current_func_name: Option<String>,
    pub(crate) current_func_ir: Option<usize>,
    pub(crate) current_func_end_ir: Option<usize>,
    pub(crate) current_ir: Option<usize>,
    pub(crate) advanced_mode: bool,
}

impl Codegen {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::with_regalloc(Box::new(GreedyRegAllocator::new()), false)
    }

    pub fn with_regalloc(reg: Box<dyn RegAllocator>, advanced_mode: bool) -> Self {
        Self {
            out: Vec::new(),
            reg,
            user_vars: Vec::new(),
            user_vars_set: HashSet::new(),
            arrays: Vec::new(),
            emitted_header: false,
            has_top_level_code: false,
            top_level_buf: None,
            toplevel_items: Vec::new(),
            func_buf: None,
            current_func_name: None,
            current_func_ir: None,
            current_func_end_ir: None,
            current_ir: None,
            advanced_mode,
        }
    }

    // ── User-variable tracking ──────────────────────────────────────────

    pub(crate) fn note_user_var(&mut self, name: &str) {
        if !self.user_vars_set.contains(name) {
            self.user_vars_set.insert(name.to_string());
            self.user_vars.push((name.to_string(), 0, None));
        }
    }

    pub(crate) fn note_user_var_init(&mut self, name: &str, init: i64) {
        if !self.user_vars_set.contains(name) {
            self.user_vars_set.insert(name.to_string());
            self.user_vars
                .push((name.to_string(), init, self.current_ir));
        }
    }

    // ── Item emission helpers ───────────────────────────────────────────

    /// Push a typed machine instruction into the current buffer.
    pub(crate) fn push_asm(&mut self, instr: S16Instr) {
        let item = AsmItem::Instr {
            instr,
            comment: None,
            ir_map: self.current_ir,
        };
        self.push_item(item);
    }

    /// Push a typed machine instruction with a comment.
    pub(crate) fn push_commented(&mut self, instr: S16Instr, comment: impl Into<String>) {
        let item = AsmItem::Instr {
            instr,
            comment: Some(comment.into()),
            ir_map: self.current_ir,
        };
        self.push_item(item);
    }

    /// Push a label into the current buffer.
    pub(crate) fn push_label(&mut self, name: String) {
        let item = AsmItem::Label(name, self.current_ir);
        self.push_item(item);
    }

    /// Push a raw `AsmItem` into the current buffer (func or top-level).
    fn push_item(&mut self, item: AsmItem) {
        if let Some(buf) = &mut self.func_buf {
            buf.push(item);
        } else if let Some(buf) = &mut self.top_level_buf {
            buf.push(item);
        } else {
            self.top_level_buf = Some(vec![item]);
        }
    }

    /// Number of items in the current output buffer.
    pub(crate) fn current_buf_len(&self) -> usize {
        if let Some(buf) = &self.func_buf {
            buf.len()
        } else if let Some(buf) = &self.top_level_buf {
            buf.len()
        } else {
            0
        }
    }

    /// Retroactively set the comment on the last instruction that writes to `reg`.
    pub(crate) fn annotate_last_write(&mut self, reg: Register, comment: String) {
        let buf = if let Some(ref mut buf) = self.func_buf {
            buf
        } else if let Some(ref mut buf) = self.top_level_buf {
            buf
        } else {
            return;
        };
        for item in buf.iter_mut().rev() {
            if let AsmItem::Instr {
                instr, comment: c, ..
            } = item
            {
                if instr.dest_reg() == Some(reg) {
                    *c = Some(comment);
                    return;
                }
            }
        }
    }

    /// Drain register-allocator-emitted instructions into the current buffer.
    pub(crate) fn drain_regalloc(&mut self, instrs: Vec<AnnotatedInstr>) {
        for (instr, comment) in instrs {
            let item = AsmItem::Instr {
                instr,
                comment,
                ir_map: self.current_ir,
            };
            self.push_item(item);
        }
    }

    // ── Register allocator convenience wrappers ─────────────────────────

    pub(crate) fn ensure_in_reg(&mut self, v: &Value) -> (Register, bool) {
        let mut tmp_out = Vec::new();
        let mut noted = Vec::new();
        let res = self
            .reg
            .ensure_in_reg(v, &mut tmp_out, &mut |name| noted.push(name.to_string()));
        self.drain_regalloc(tmp_out);
        for name in noted {
            self.note_user_var(&name);
        }
        res
    }

    pub(crate) fn allocate_temp_reg(&mut self) -> Register {
        let mut out = Vec::new();
        let r = self.reg.allocate_reg(&mut out);
        self.drain_regalloc(out);
        r
    }

    pub(crate) fn get_var_reg(&self, var: &Var) -> Option<Register> {
        self.reg.get_var_reg(var)
    }

    pub(crate) fn prepare_def_reg(&mut self, var: &Var) -> Register {
        let mut out = Vec::new();
        let mut noted = Vec::new();
        let r = self
            .reg
            .prepare_def_reg(var, &mut out, &mut |name| noted.push(name.to_string()));
        self.drain_regalloc(out);
        for name in noted {
            self.note_user_var(&name);
        }
        r
    }

    // ── Function / top-level region management ──────────────────────────

    pub(crate) fn start_function(&mut self, name: String) {
        let mut flush_out = Vec::new();
        self.reg.flush_dirty(&mut flush_out);
        self.drain_regalloc(flush_out);
        self.flush_top_level();
        self.func_buf = Some(Vec::new());
        self.current_func_name = Some(name);
        self.current_func_ir = self.current_ir;
        self.current_func_end_ir = None;
        self.reg.begin_region();
    }

    pub(crate) fn flush_top_level(&mut self) {
        if let Some(body) = self.top_level_buf.take() {
            let max_slots = self.reg.get_max_slots();
            self.has_top_level_code = true;

            if max_slots > 0 {
                self.toplevel_items.push(AsmItem::Instr {
                    instr: S16Instr::Lea {
                        d: Register::STACK_PTR,
                        disp: Disp::Num(max_slots as i64),
                        idx: Register::STACK_PTR,
                    },
                    comment: Some(format!("alloc toplevel frame ({})", max_slots)),
                    ir_map: None,
                });
            }
            self.toplevel_items.extend(body);
            if max_slots > 0 {
                self.toplevel_items.push(AsmItem::Instr {
                    instr: S16Instr::Lea {
                        d: Register::STACK_PTR,
                        disp: Disp::Num(-(max_slots as i64)),
                        idx: Register::STACK_PTR,
                    },
                    comment: Some("dealloc toplevel frame".into()),
                    ir_map: None,
                });
            }
        }
    }

    fn detect_used_callee_saved(body: &[AsmItem]) -> Vec<Register> {
        Register::CALLEE_SAVED
            .into_iter()
            .filter(|reg| {
                body.iter().any(|item| {
                    if let AsmItem::Instr { instr, .. } = item {
                        instr.uses_register(*reg)
                    } else {
                        false
                    }
                })
            })
            .collect()
    }

    fn is_leaf(body: &[AsmItem]) -> bool {
        !body.iter().any(|item| {
            if let AsmItem::Instr { instr, .. } = item {
                instr.is_call()
            } else {
                false
            }
        })
    }

    pub(crate) fn flush_function(&mut self) {
        let body = self.func_buf.take().unwrap_or_default();
        let name = self
            .current_func_name
            .take()
            .unwrap_or_else(|| "__anon".to_string());
        let max_slots = self.reg.get_max_slots();
        let used_callee = Self::detect_used_callee_saved(&body);
        let saved_count = used_callee.len();
        let frame = max_slots + saved_count;
        let is_leaf = Self::is_leaf(&body);
        let func_start_ir = self.current_func_ir;
        let func_end_ir = self.current_func_end_ir.or(func_start_ir);

        let mut prologue = Vec::new();
        let mut epilogue = Vec::new();

        // Sigma16 stack grows UPWARD.
        let total_frame = frame + 1;

        // Save return address at current stack pointer.
        prologue.push(AsmItem::Instr {
            instr: S16Instr::store_disp(Register::LINK_REG, 0, Register::STACK_PTR),
            comment: Some("save return addr".into()),
            ir_map: func_start_ir,
        });

        // Adjust stack pointer upward.
        prologue.push(AsmItem::Instr {
            instr: S16Instr::Lea {
                d: Register::STACK_PTR,
                disp: Disp::Num(total_frame as i64),
                idx: Register::STACK_PTR,
            },
            comment: Some(format!("allocate frame ({})", total_frame)),
            ir_map: func_start_ir,
        });

        // Save callee-saved registers.
        for (i, &reg) in used_callee.iter().enumerate() {
            let disp = -(max_slots as i64) - 1 - i as i64;
            prologue.push(AsmItem::Instr {
                instr: S16Instr::store_disp(reg, disp, Register::STACK_PTR),
                comment: Some(format!("save {}", reg)),
                ir_map: func_start_ir,
            });
        }

        // Epilogue label for early returns.
        epilogue.push(AsmItem::Label(format!("ret_{}", name), func_end_ir));

        // Restore callee-saved registers (reverse order).
        for (i, &reg) in used_callee.iter().enumerate().rev() {
            let disp = -(max_slots as i64) - 1 - i as i64;
            epilogue.push(AsmItem::Instr {
                instr: S16Instr::load_disp(reg, disp, Register::STACK_PTR),
                comment: Some(format!("restore {}", reg)),
                ir_map: func_end_ir,
            });
        }

        // Deallocate frame.
        epilogue.push(AsmItem::Instr {
            instr: S16Instr::Lea {
                d: Register::STACK_PTR,
                disp: Disp::Num(-(total_frame as i64)),
                idx: Register::STACK_PTR,
            },
            comment: Some("deallocate frame".into()),
            ir_map: func_end_ir,
        });

        // Restore return address.
        epilogue.push(AsmItem::Instr {
            instr: S16Instr::load_disp(Register::LINK_REG, 0, Register::STACK_PTR),
            comment: Some("restore return addr".into()),
            ir_map: func_end_ir,
        });

        // Return.
        epilogue.push(AsmItem::Instr {
            instr: S16Instr::Jump {
                disp: Disp::Num(0),
                idx: Register::LINK_REG,
            },
            comment: Some("return".into()),
            ir_map: func_end_ir,
        });

        self.out.push(AsmItem::Function {
            name,
            ir_map: self.current_func_ir,
            prologue,
            body,
            epilogue,
            frame_size: max_slots,
            used_callee,
            is_leaf,
        });

        self.reg.begin_region();
    }

    // ── Final output ────────────────────────────────────────────────────

    pub fn finish_codegen(mut self) -> Sigma16Asm {
        let mut out = Vec::new();
        self.reg.flush_all(&mut out);
        self.drain_regalloc(out);

        self.flush_top_level();
        if self.func_buf.is_some() {
            self.flush_function();
        }

        if self.has_top_level_code {
            let mut final_out = Vec::new();

            // Stack pointer setup.
            final_out.push(AsmItem::Instr {
                instr: S16Instr::lea_label(Register::STACK_PTR, "stack"),
                comment: Some("init stack ptr".into()),
                ir_map: None,
            });

            // All accumulated top-level code.
            final_out.append(&mut self.toplevel_items);

            // Halt.
            final_out.push(AsmItem::Instr {
                instr: S16Instr::trap_halt(),
                comment: Some("halt".into()),
                ir_map: None,
            });

            // Functions follow after the halt.
            final_out.append(&mut self.out);
            self.out = final_out;
        }

        // Data section.
        self.out.push(AsmItem::Blank);

        let array_names: HashSet<&str> = self
            .arrays
            .iter()
            .map(|(name, _, _, _)| name.as_str())
            .collect();

        for (name, init, ir_map) in &self.user_vars {
            if array_names.contains(name.as_str()) {
                continue;
            }
            self.out.push(AsmItem::Data {
                label: Some(name.clone()),
                value: *init,
                ir_map: *ir_map,
            });
        }
        for (name, len, initial_values, ir_map) in &self.arrays {
            self.out.push(AsmItem::Label(name.clone(), *ir_map));
            for i in 0..*len {
                let val = initial_values
                    .as_ref()
                    .and_then(|v| v.get(i))
                    .cloned()
                    .unwrap_or(0);
                self.out.push(AsmItem::Data {
                    label: None,
                    value: val,
                    ir_map: *ir_map,
                });
            }
        }
        if self.has_top_level_code {
            self.out.push(AsmItem::Data {
                label: Some("stack".to_string()),
                value: 0,
                ir_map: None,
            });
        }

        // Optimize.
        super::optimize::optimize(&mut self.out);

        // Flatten to text.
        let mut lines = Vec::new();
        let mut mapping = Vec::new();
        Self::flatten_items(&self.out, &mut lines, &mut mapping);

        Sigma16Asm {
            lines,
            asm_ir_mapping: mapping,
        }
    }

    fn flatten_items(
        items: &[AsmItem],
        lines: &mut Vec<String>,
        mapping: &mut Vec<Option<usize>>,
    ) {
        for item in items {
            match item {
                AsmItem::Label(name, ir_map) => {
                    lines.push(name.clone());
                    mapping.push(*ir_map);
                }
                AsmItem::Instr {
                    instr,
                    comment,
                    ir_map,
                } => {
                    let base = instr.to_string();
                    let line = if let Some(c) = comment {
                        const COMMENT_COL: usize = 30;
                        let pad = if base.len() < COMMENT_COL {
                            COMMENT_COL - base.len()
                        } else {
                            2
                        };
                        format!("{}{}; {}", base, " ".repeat(pad), c)
                    } else {
                        base
                    };
                    lines.push(line);
                    mapping.push(*ir_map);
                }
                AsmItem::Function {
                    name,
                    ir_map,
                    prologue,
                    body,
                    epilogue,
                    ..
                } => {
                    lines.push(name.clone());
                    mapping.push(*ir_map);
                    Self::flatten_items(prologue, lines, mapping);
                    Self::flatten_items(body, lines, mapping);
                    Self::flatten_items(epilogue, lines, mapping);
                }
                AsmItem::Data {
                    label,
                    value,
                    ir_map,
                } => {
                    let line = if let Some(lbl) = label {
                        format!("{:<8} data   {}", lbl, value)
                    } else {
                        format!("     data   {}", value)
                    };
                    lines.push(line);
                    mapping.push(*ir_map);
                }
                AsmItem::Blank => {
                    lines.push(String::new());
                    mapping.push(None);
                }
            }
        }
    }
}
