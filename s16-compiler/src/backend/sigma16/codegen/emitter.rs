use std::collections::HashSet;
use crate::ir::{Value, Var};
use super::super::regalloc::{RegAllocator, GreedyRegAllocator};
use super::super::abi::Register;
use super::item::AsmItem;

pub struct Codegen {
    pub out: Vec<AsmItem>,
    pub reg: Box<dyn RegAllocator>,
    pub user_vars: Vec<(String, i64, Option<usize>)>, // name, initial value, ir_map
    pub user_vars_set: HashSet<String>,
    pub arrays: Vec<(String, usize, Option<Vec<i64>>, Option<usize>)>, // name, size, values, ir_map
    pub emitted_header: bool,
    pub top_level_buf: Option<Vec<AsmItem>>,
    pub func_buf: Option<Vec<AsmItem>>,
    pub current_func_name: Option<String>,
    pub current_func_ir: Option<usize>,
    pub current_ir: Option<usize>,
}

impl Codegen {
    #[allow(dead_code)]
    pub fn new() -> Self {
        Self::with_regalloc(Box::new(GreedyRegAllocator::new()))
    }

    pub fn with_regalloc(reg: Box<dyn RegAllocator>) -> Self {
        Self {
            out: Vec::new(),
            reg,
            user_vars: Vec::new(),
            user_vars_set: HashSet::new(),
            arrays: Vec::new(),
            emitted_header: false,
            top_level_buf: None,
            func_buf: None,
            current_func_name: None,
            current_func_ir: None,
            current_ir: None,
        }
    }

    pub fn note_user_var(&mut self, name: &str) {
        if !self.user_vars_set.contains(name) {
            self.user_vars_set.insert(name.to_string());
            self.user_vars.push((name.to_string(), 0, self.current_ir));
        }
    }

    pub fn note_user_var_init(&mut self, name: &str, init: i64) {
        if !self.user_vars_set.contains(name) {
            self.user_vars_set.insert(name.to_string());
            self.user_vars.push((name.to_string(), init, self.current_ir));
        }
    }

    pub fn emit<S: Into<String>>(&mut self, s: S) {
        let line = s.into();
        let map = self.current_ir;
        let item = if line.ends_with(':') && !line.starts_with(' ') {
            AsmItem::Label(line[..line.len() - 1].to_string(), map)
        } else {
            AsmItem::Instruction {
                text: line.clone(),
                ir_map: map,
            }
        };

        if let Some(buf) = &mut self.func_buf {
            buf.push(item);
        } else if let Some(buf) = &mut self.top_level_buf {
            buf.push(item);
        } else {
            self.top_level_buf = Some(Vec::new());
            // Don't call begin_region() here - it clears variable bindings that were just made!
            // begin_region() should only be called at function/region boundaries.
            self.top_level_buf.as_mut().unwrap().push(item);
        }
    }

    pub fn start_function(&mut self, name: String) {
        self.flush_top_level();
        self.func_buf = Some(Vec::new());
        self.current_func_name = Some(name);
        self.current_func_ir = self.current_ir;
        self.reg.begin_region();
    }

    pub fn flush_top_level(&mut self) {
        if let Some(body) = self.top_level_buf.take() {
            let max_slots = self.reg.get_max_slots();
            if !self.out.iter().any(|item| item.as_label() == Some("prog_start")) {
                self.out.push(AsmItem::Label("prog_start".to_string(), None));
            }
            if max_slots > 0 {
                self.out.push(AsmItem::Instruction {
                    text: format!("  lea {},{}[{}]", Register::STACK_PTR, max_slots, Register::STACK_PTR),
                    ir_map: None,
                });
            }
            for item in body {
                self.out.push(item);
            }
            if max_slots > 0 {
                self.out.push(AsmItem::Instruction {
                    text: format!("  lea {},-{}[{}]", Register::STACK_PTR, max_slots, Register::STACK_PTR),
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
                    if let AsmItem::Instruction { text, .. } = item {
                        text.contains(reg.as_str())
                    } else {
                        false
                    }
                })
            })
            .collect()
    }

    pub fn is_leaf(&self, body: &[AsmItem]) -> bool {
        !body.iter().any(|item| {
            if let AsmItem::Instruction { text, .. } = item {
                text.contains("jal ")
            } else {
                false
            }
        })
    }

    pub fn flush_function(&mut self) {
        let body = self.func_buf.take().unwrap_or_default();
        let name = self.current_func_name.take().unwrap_or("__anon".to_string());
        let max_slots = self.reg.get_max_slots();
        let used_callee = self.detect_used_callee_saved(&body);
        let saved_count = used_callee.len();
        let frame = max_slots + saved_count;
        let is_leaf = self.is_leaf(&body);

        let mut prologue = Vec::new();
        let mut epilogue = Vec::new();

        // Sigma16 stack grows UPWARD
        // Stack layout when function is active:
        // [old_R14+0]: Return address (R13)
        // [old_R14+1]: First local/spill slot
        // [old_R14+2]: Second local/spill slot
        // ...
        // [old_R14+max_slots]: Last local/spill slot
        // [old_R14+max_slots+1]: First callee-saved register
        // ...
        // [old_R14+total_frame-1]: Last callee-saved register
        // [old_R14+total_frame]: <-- new R14 points here

        // Save return address at current stack pointer
        prologue.push(AsmItem::Instruction {
            text: format!("  store {},0[{}]", Register::LINK_REG, Register::STACK_PTR),
            ir_map: None,
        });

        // Adjust stack pointer upward for return address, local variables, and callee-saved regs
        let total_frame = frame + 1;
        prologue.push(AsmItem::Instruction {
            text: format!("  lea {},{}[{}]", Register::STACK_PTR, total_frame, Register::STACK_PTR),
            ir_map: None,
        });

        // Save callee-saved registers
        // They are stored relative to the NEW stack pointer (looking backward)
        for (i, &reg) in used_callee.iter().enumerate() {
            let disp = -(1 + saved_count as i32) + i as i32;
            prologue.push(AsmItem::Instruction {
                text: format!("  store {},{}[{}]", reg, disp, Register::STACK_PTR),
                ir_map: None,
            });
        }

        // Restore callee-saved registers
        for (i, &reg) in used_callee.iter().enumerate().rev() {
            let disp = -(1 + saved_count as i32) + i as i32;
            epilogue.push(AsmItem::Instruction {
                text: format!("  load {},{}[{}]", reg, disp, Register::STACK_PTR),
                ir_map: None,
            });
        }

        // Move stack pointer back down
        epilogue.push(AsmItem::Instruction {
            text: format!("  lea {},-{}[{}]", Register::STACK_PTR, total_frame, Register::STACK_PTR),
            ir_map: None,
        });

        // Restore return address
        epilogue.push(AsmItem::Instruction {
            text: format!("  load {},0[{}]", Register::LINK_REG, Register::STACK_PTR),
            ir_map: None,
        });

        epilogue.push(AsmItem::Instruction {
            text: format!("  jump 0[{}]", Register::LINK_REG),
            ir_map: None,
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
        let res = self.reg.ensure_in_reg(v, &mut tmp_out, &mut |name| noted.push(name.to_string()));
        for line in tmp_out {
            self.emit(line);
        }
        for name in noted {
            self.note_user_var(&name);
        }
        res
    }

    pub fn allocate_temp_reg(&mut self) -> Register {
        let mut out = Vec::new();
        let r = self.reg.allocate_reg(&mut out);
        for line in out {
            self.emit(line);
        }
        r
    }

    pub fn ensure_var_in_reg(&mut self, var: &Var, prefer_reg: Option<Register>) -> Register {
        let mut out = Vec::new();
        let mut noted = Vec::new();
        let r = self.reg.ensure_var_in_reg(var, &mut out, &mut |name| noted.push(name.to_string()), prefer_reg);
        for line in out {
            self.emit(line);
        }
        for name in noted {
            self.note_user_var(&name);
        }
        r
    }

    pub fn get_var_reg(&self, var: &Var) -> Option<Register> {
        self.reg.get_var_reg(var)
    }

    pub fn finish_codegen(mut self) -> super::Sigma16Asm {
        let mut out = Vec::new();
        self.reg.flush_all(&mut out);
        for l in out {
            self.emit(l);
        }

        self.flush_top_level();
        if self.func_buf.is_some() {
            self.flush_function();
        }

        // trap instruction to terminate program
        self.out.push(AsmItem::Instruction {
            text: "  trap R0,R0,R0".to_string(),
            ir_map: None,
        });

        self.out.push(AsmItem::Instruction {
            text: String::new(),
            ir_map: None,
        });

        for (name, init, ir_map) in &self.user_vars {
            self.out.push(AsmItem::Instruction {
                text: format!("{:<8} data   {}", name, init),
                ir_map: *ir_map,
            });
        }
        for (name, len, initial_values, ir_map) in &self.arrays {
            self.out.push(AsmItem::Label(name.clone(), *ir_map));
            for i in 0..*len {
                let val = initial_values.as_ref().and_then(|v| v.get(i)).cloned().unwrap_or(0);
                self.out.push(AsmItem::Instruction {
                    text: format!("     data   {}", val),
                    ir_map: *ir_map,
                });
            }
        }
        self.out.push(AsmItem::Instruction {
            text: "stack    data   0".to_string(),
            ir_map: None,
        });

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
                AsmItem::Instruction { text, ir_map } => {
                    lines.push(text.clone());
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
            }
        }
    }
}
