use super::super::abi::Register;
use super::super::regalloc::{GreedyRegAllocator, RegAllocator};
use super::item::{AsmItem, Disp, S16Instr};
use crate::ir::{Value, Var};
use std::collections::HashSet;

pub struct Codegen {
    pub out: Vec<AsmItem>,
    pub reg: Box<dyn RegAllocator>,
    pub user_vars: Vec<(String, i64, Option<usize>)>, // name, initial value, ir_map
    pub user_vars_set: HashSet<String>,
    pub arrays: Vec<(String, usize, Option<Vec<i64>>, Option<usize>)>, // name, size, values, ir_map
    pub emitted_header: bool,
    pub has_top_level_code: bool,
    pub top_level_buf: Option<Vec<AsmItem>>,
    /// Accumulated top-level code across all top-level regions.
    /// This is populated by `flush_top_level` and emitted as a single
    /// contiguous block under `prog_start` in `finish_codegen`.
    pub toplevel_items: Vec<AsmItem>,
    pub func_buf: Option<Vec<AsmItem>>,
    pub current_func_name: Option<String>,
    pub current_func_ir: Option<usize>,
    pub current_func_end_ir: Option<usize>,
    pub current_ir: Option<usize>,
    pub advanced_mode: bool,
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

    pub fn note_user_var(&mut self, name: &str) {
        if !self.user_vars_set.contains(name) {
            self.user_vars_set.insert(name.to_string());
            // No IR mapping for unspecified initial value; runtime stores carry the mapping.
            self.user_vars.push((name.to_string(), 0, None));
        }
    }

    pub fn note_user_var_init(&mut self, name: &str, init: i64) {
        if !self.user_vars_set.contains(name) {
            self.user_vars_set.insert(name.to_string());
            self.user_vars
                .push((name.to_string(), init, self.current_ir));
        }
    }

    // ── Item emission helpers ───────────────────────────────────────────

    /// Push a typed machine instruction into the current buffer.
    pub fn push_asm(&mut self, instr: S16Instr) {
        let item = AsmItem::Instr {
            instr,
            ir_map: self.current_ir,
        };
        self.push_item(item);
    }

    /// Push a label into the current buffer.
    pub fn push_label(&mut self, name: String) {
        let item = AsmItem::Label(name, self.current_ir);
        self.push_item(item);
    }

    /// Push a raw AsmItem into the current buffer (func_buf or top_level_buf).
    fn push_item(&mut self, item: AsmItem) {
        if let Some(buf) = &mut self.func_buf {
            buf.push(item);
        } else if let Some(buf) = &mut self.top_level_buf {
            buf.push(item);
        } else {
            self.top_level_buf = Some(Vec::new());
            self.top_level_buf.as_mut().unwrap().push(item);
        }
    }

    /// Drain regalloc-emitted instructions into the current buffer.
    pub fn drain_regalloc(&mut self, instrs: Vec<S16Instr>) {
        for instr in instrs {
            self.push_asm(instr);
        }
    }

    pub fn start_function(&mut self, name: String) {
        // Flush dirty globals to memory before the region ends,
        // otherwise begin_region() will discard them.
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

    pub fn flush_top_level(&mut self) {
        if let Some(body) = self.top_level_buf.take() {
            let max_slots = self.reg.get_max_slots();
            self.has_top_level_code = true;

            // Accumulate top-level code into toplevel_items.
            // Each region gets its own stack adjustments if needed.
            if max_slots > 0 {
                self.toplevel_items.push(AsmItem::Instr {
                    instr: S16Instr::Lea {
                        d: Register::STACK_PTR,
                        disp: Disp::Num(max_slots as i64),
                        idx: Register::STACK_PTR,
                    },
                    ir_map: None,
                });
            }
            for item in body {
                self.toplevel_items.push(item);
            }
            if max_slots > 0 {
                self.toplevel_items.push(AsmItem::Instr {
                    instr: S16Instr::Lea {
                        d: Register::STACK_PTR,
                        disp: Disp::Num(-(max_slots as i64)),
                        idx: Register::STACK_PTR,
                    },
                    ir_map: None,
                });
            }
        }
    }

    pub fn detect_used_callee_saved(&self, body: &[AsmItem]) -> Vec<Register> {
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

    pub fn is_leaf(&self, body: &[AsmItem]) -> bool {
        !body.iter().any(|item| {
            if let AsmItem::Instr { instr, .. } = item {
                instr.is_call()
            } else {
                false
            }
        })
    }

    pub fn flush_function(&mut self) {
        let body = self.func_buf.take().unwrap_or_default();
        let name = self
            .current_func_name
            .take()
            .unwrap_or("__anon".to_string());
        let max_slots = self.reg.get_max_slots();
        let used_callee = self.detect_used_callee_saved(&body);
        let saved_count = used_callee.len();
        let frame = max_slots + saved_count;
        let is_leaf = self.is_leaf(&body);
        let func_start_ir = self.current_func_ir;
        let func_end_ir = self.current_func_end_ir.or(func_start_ir);

        let mut prologue = Vec::new();
        let mut epilogue = Vec::new();

        // Sigma16 stack grows UPWARD
        // Stack layout when function is active:
        // [old_R14+0]: Return address (R13)
        // [old_R14+1]: First local/spill slot
        // ...
        // [old_R14+max_slots]: Last local/spill slot
        // [old_R14+max_slots+1]: First callee-saved register
        // ...
        // [old_R14+total_frame-1]: Last callee-saved register
        // [old_R14+total_frame]: <-- new R14 points here

        // Save return address at current stack pointer
        let total_frame = frame + 1;
        prologue.push(AsmItem::Instr {
            instr: S16Instr::store_disp(Register::LINK_REG, 0, Register::STACK_PTR),
            ir_map: func_start_ir,
        });

        // Adjust stack pointer upward
        prologue.push(AsmItem::Instr {
            instr: S16Instr::Lea {
                d: Register::STACK_PTR,
                disp: Disp::Num(total_frame as i64),
                idx: Register::STACK_PTR,
            },
            ir_map: func_start_ir,
        });

        // Save callee-saved registers
        for (i, &reg) in used_callee.iter().enumerate() {
            let disp = -(max_slots as i64) - 1 - i as i64;
            prologue.push(AsmItem::Instr {
                instr: S16Instr::store_disp(reg, disp, Register::STACK_PTR),
                ir_map: func_start_ir,
            });
        }

        // Epilogue label for early returns
        let epilogue_label = format!("ret_{}", name);
        epilogue.push(AsmItem::Label(epilogue_label, func_end_ir));

        // Restore callee-saved registers
        for (i, &reg) in used_callee.iter().enumerate().rev() {
            let disp = -(max_slots as i64) - 1 - i as i64;
            epilogue.push(AsmItem::Instr {
                instr: S16Instr::load_disp(reg, disp, Register::STACK_PTR),
                ir_map: func_end_ir,
            });
        }

        // Move stack pointer back down
        epilogue.push(AsmItem::Instr {
            instr: S16Instr::Lea {
                d: Register::STACK_PTR,
                disp: Disp::Num(-(total_frame as i64)),
                idx: Register::STACK_PTR,
            },
            ir_map: func_end_ir,
        });

        // Restore return address
        epilogue.push(AsmItem::Instr {
            instr: S16Instr::load_disp(Register::LINK_REG, 0, Register::STACK_PTR),
            ir_map: func_end_ir,
        });

        // Return: jump 0[R13]
        epilogue.push(AsmItem::Instr {
            instr: S16Instr::Jump {
                disp: Disp::Num(0),
                idx: Register::LINK_REG,
            },
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

    pub fn ensure_in_reg(&mut self, v: &Value) -> (Register, bool) {
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

    pub fn allocate_temp_reg(&mut self) -> Register {
        let mut out = Vec::new();
        let r = self.reg.allocate_reg(&mut out);
        self.drain_regalloc(out);
        r
    }

    pub fn get_var_reg(&self, var: &Var) -> Option<Register> {
        self.reg.get_var_reg(var)
    }

    pub fn prepare_def_reg(&mut self, var: &Var) -> Register {
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

    pub fn finish_codegen(mut self) -> super::Sigma16Asm {
        let mut out = Vec::new();
        self.reg.flush_all(&mut out);
        self.drain_regalloc(out);

        self.flush_top_level();
        if self.func_buf.is_some() {
            self.flush_function();
        }

        // Build final output with correct ordering:
        //   header (lea stack + jump prog_start)
        //   all functions (already in self.out)
        //   prog_start label + all top-level code + trap
        //   data section
        if self.has_top_level_code {
            // Build final output: top-level code first, then functions.
            let mut final_out = Vec::new();

            // Stack pointer setup
            final_out.push(AsmItem::Instr {
                instr: S16Instr::lea_label(Register::STACK_PTR, "stack"),
                ir_map: None,
            });

            // All accumulated top-level code
            final_out.append(&mut self.toplevel_items);

            // Halt
            final_out.push(AsmItem::Instr {
                instr: S16Instr::trap_halt(),
                ir_map: None,
            });

            // All functions follow after the trap
            final_out.append(&mut self.out);
            self.out = final_out;
        }

        // Blank separator
        self.out.push(AsmItem::Blank);

        // Collect array names to avoid emitting them twice (once as user_var, once as array)
        let array_names: std::collections::HashSet<&str> = self
            .arrays
            .iter()
            .map(|(name, _, _, _)| name.as_str())
            .collect();

        for (name, init, ir_map) in &self.user_vars {
            // Skip if this variable will be emitted as an array
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

        // Run optimisations
        super::super::opt::optimize(&mut self.out);

        let mut lines = Vec::new();
        let mut mapping = Vec::new();
        Self::flatten_items(&self.out, &mut lines, &mut mapping);

        super::Sigma16Asm {
            lines,
            asm_ir_mapping: mapping,
        }
    }

    fn flatten_items(items: &[AsmItem], lines: &mut Vec<String>, mapping: &mut Vec<Option<usize>>) {
        for item in items {
            match item {
                AsmItem::Label(name, ir_map) => {
                    lines.push(name.clone());
                    mapping.push(*ir_map);
                }
                AsmItem::Instr { instr, ir_map } => {
                    lines.push(instr.to_string());
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
