use crate::ir::{ArithOp, Instr, ProgramIR, RelOp, Rhs, Value};
use std::collections::HashSet;

use super::regalloc::{is_temp, AdvancedRegAllocator, AllocatorKind, BasicRegAllocator, RegAllocator};

/// Assembly output with a mapping from each ASM line to the originating IR instruction index (if any).
/// Lines like headers, synthesized prologues/epilogues, and data definitions map to `None`.
#[derive(Debug, Clone)]
pub struct Sigma16Asm {
    pub lines: Vec<String>,
    /// `asm_ir_mapping[i]` is `Some(ir_index)` if the asm `lines[i]` was emitted
    /// while lowering IR instruction `ir_index`; otherwise `None`.
    pub asm_ir_mapping: Vec<Option<usize>>,
}

impl Sigma16Asm {
    pub fn join(&self) -> String { self.lines.join("\n") }
}

/// Entry point: compile IR to Sigma16 assembly string.
pub fn compile_ir_to_sigma16(ir: &ProgramIR) -> String {
    let mut cg = Codegen::new(BasicRegAllocator::new());
    cg.emit_program(ir);
    let asm = cg.finish();
    asm.join()
}

/// Compile using a specific allocator kind. Advanced allocator is a scaffold for now.
pub fn compile_ir_to_sigma16_with_allocator(kind: AllocatorKind, ir: &ProgramIR) -> String {
    match kind {
        AllocatorKind::Basic => {
            let mut cg = Codegen::new(BasicRegAllocator::new());
            cg.emit_program(ir);
            let asm = cg.finish();
            asm.join()
        }
        AllocatorKind::Advanced => {
            let mut cg = Codegen::new(AdvancedRegAllocator::new());
            cg.emit_program(ir);
            let asm = cg.finish();
            asm.join()
        }
    }
}

/// Compile and also return an ASM-to-IR mapping for each emitted line.
pub fn compile_ir_to_sigma16_mapped(ir: &ProgramIR) -> Sigma16Asm {
    let mut cg = Codegen::new(BasicRegAllocator::new());
    cg.emit_program(ir);
    cg.finish()
}

/// Compile with a specific allocator and get ASM with mapping.
pub fn compile_ir_to_sigma16_with_allocator_mapped(kind: AllocatorKind, ir: &ProgramIR) -> Sigma16Asm {
    match kind {
        AllocatorKind::Basic => {
            let mut cg = Codegen::new(BasicRegAllocator::new());
            cg.emit_program(ir);
            cg.finish()
        }
        AllocatorKind::Advanced => {
            let mut cg = Codegen::new(AdvancedRegAllocator::new());
            cg.emit_program(ir);
            cg.finish()
        }
    }
}

struct Codegen<R: RegAllocator> {
    out: Vec<String>,
    /// Per-line mapping to the originating IR instruction index
    out_map: Vec<Option<usize>>,
    reg: R,
    /// Set and order of user variables discovered
    user_vars: Vec<String>,
    user_vars_set: HashSet<String>,
    /// Declared arrays (name, len)
    arrays: Vec<(String, usize)>,
    /// Whether we already emitted the startup header (SP init + jump)
    emitted_header: bool,
    /// Whether we emitted the program start label
    emitted_prog_start: bool,
    /// Per-function buffering to synthesize minimal prologue/epilogue
    func_buf: Option<Vec<(String, Option<usize>)>>, // buffered body of current function with mapping
    func_label: Option<String>,    // current function label
    func_makes_call: bool,         // whether current function performs any call
    /// Current IR instruction index whose lowering is emitting code (None for synthesized lines)
    current_ir: Option<usize>,
}

impl<R: RegAllocator> Codegen<R> {
    // Ensure the top-level allocation region is started BEFORE any allocator calls.
    // Without this, flushing allocator-emitted lines for the first operand of a
    // top-level instruction would trigger begin_region() mid-instruction via emit(),
    // resetting allocator state and potentially causing both operands to land in the
    // same register (e.g., cmp R1,R1). Starting the region here keeps allocator state
    // stable across the whole instruction.
    fn ensure_top_level_region_started(&mut self) {
        if self.func_buf.is_none() && !self.emitted_prog_start {
            // Insert program start label before any top-level code
            self.out.push("__prog_start".to_string());
            self.out_map.push(None);
            self.emitted_prog_start = true;
            // Begin a new allocation region for top-level code
            self.reg.begin_region();
        }
    }

    fn new(reg: R) -> Self {
        Self {
            out: Vec::new(),
            out_map: Vec::new(),
            reg,
            user_vars: Vec::new(),
            user_vars_set: HashSet::new(),
            arrays: Vec::new(),
            emitted_header: false,
            emitted_prog_start: false,
            func_buf: None,
            func_label: None,
            func_makes_call: false,
            current_ir: None,
        }
    }

    fn finish(mut self) -> Sigma16Asm {
        // If nobody caused us to place the program start label, emit it now to satisfy jump
        if !self.emitted_prog_start {
            self.out.push("__prog_start".to_string());
            self.out_map.push(None);
        }

        // Close any in-progress top-level region for allocators and fix up stack-based spills.
        // We translate pseudo SPILL_* markers into concrete load/store relative to R13
        // and reserve stack space for them for the top-level region.
        // Functions are handled separately when their buffers are flushed.
        let top_spills = {
            // End the current region and get spill count (0 for basic allocator)
            self.reg.end_region()
        };
        if top_spills > 0 {
            // Find program start position
            if let Some(pos) = self.out.iter().position(|s| s == "__prog_start") {
                // Insert prologue adjustment right after __prog_start
                let adj = format!("  lea R13,{}[R13]", top_spills);
                let insert_at = pos + 1;
                self.out.insert(insert_at, adj);
                self.out_map.insert(insert_at, None);
                // Translate SPILL markers in the remainder of the top-level code
                Codegen::<R>::translate_spill_markers(&mut self.out[insert_at + 1..], top_spills);
                // Emit epilogue adjustment at the end of top-level code (before data section)
                self.out.push(format!("  lea R13,-{}[R13]", top_spills));
                self.out_map.push(None);
            }
        } else {
            // Even if there are no spills, clean up any stray markers defensively
            if let Some(pos) = self.out.iter().position(|s| s == "__prog_start") {
                let slice_len = self.out.len().saturating_sub(pos + 1);
                if slice_len > 0 {
                    Codegen::<R>::translate_spill_markers(&mut self.out[pos + 1..], 0);
                }
            }
        }

        // If a function was started but not properly closed, flush it defensively
        if self.func_buf.is_some() {
            self.flush_current_function();
        }

        // Emit static data section for discovered user variables and arrays
        self.out.push(String::new());
        self.out_map.push(None);
        for v in &self.user_vars {
            // default-initialize to 0
            // Ensure there is always a space between the name and the directive
            self.out.push(format!("{:<8} data   0", v));
            self.out_map.push(None);
        }
        // Emit arrays with N zeros
        for (name, len) in &self.arrays {
            self.out.push(format!("{}", name));
            self.out_map.push(None);
            for _ in 0..*len {
                self.out.push("     data   0".to_string());
                self.out_map.push(None);
            }
        }
        // Append the stack base symbol always (even if there are no other symbols)
        self.out.push("stack   data   0".to_string());
        self.out_map.push(None);

        Sigma16Asm { lines: self.out, asm_ir_mapping: self.out_map }
    }

    fn emit_program(&mut self, ir: &ProgramIR) {
        // Copy declared arrays for data emission
        self.arrays = ir.arrays.clone();
        // Emit startup header once at the very top
        if !self.emitted_header {
            self.out.push("  lea R13,stack[R0]".to_string());
            self.out_map.push(None);
            self.out.push("  jump __prog_start".to_string());
            self.out_map.push(None);
            self.emitted_header = true;
            // Do NOT emit __prog_start here. We will lazily insert it the first
            // time we actually emit a top-level instruction (non-function code),
            // so that function definitions at the top don't get executed.
        }
        for (ir_index, instr) in ir.instrs.iter().enumerate() {
            self.current_ir = Some(ir_index);
            match instr {
                Instr::Label(lbl) => {
                    self.emit(lbl.clone());
                }
                Instr::Goto(t) => self.emit(format!("  jump {}", t)),
                Instr::IfCmpGoto { left, op, right, target } => {
                    let (rl, free_l) = self.ensure_in_reg(left);
                    let (rr, free_r) = self.ensure_in_reg(right);
                    self.emit(format!("  cmp {},{}", rl, rr));
                    let j = match op {
                        RelOp::Eq => "je",
                        RelOp::Neq => "jne",
                        RelOp::Lt => "jl",
                        RelOp::Gt => "jg",
                        RelOp::Le => "jle",
                        RelOp::Ge => "jge",
                    };
                    self.emit(format!("  {} {}", j, target));
                    if free_l { self.reg.free_reg(rl); }
                    if free_r { self.reg.free_reg(rr); }
                }
                Instr::IfFalseGoto { value, target } => {
                    let (rv, free_v) = self.ensure_in_reg(value);
                    self.emit(format!("  beq {},R0,{}", rv, target));
                    if free_v { self.reg.free_reg(rv); }
                }
                Instr::Assign { dst, src } => match src {
                    Rhs::Value(v) => {
                        let (rs, free_s) = self.ensure_in_reg(v);
                        if is_temp(dst) {
                            // Keep temporaries in registers; bind/move into dst's register.
                            let (rd, _free_d) = self.ensure_in_reg(&Value::Var(dst.clone()));
                            if rd != rs {
                                self.emit(format!("  add {},R0,{}", rd, rs));
                            }
                            // Free the source scratch if it was a temporary scratch register
                            if free_s && rs != rd { self.reg.free_reg(rs); }
                        } else {
                            // User variable destination: write back to memory
                            self.note_user_var(dst);
                            self.emit(format!("  store {},{}", rs, dst));
                            if free_s { self.reg.free_reg(rs); }
                        }
                    }
                    Rhs::Binary { op, left, right } => {
                        let (rl, free_l) = self.ensure_in_reg(left);
                        let (rr, free_r) = self.ensure_in_reg(right);
                        let (rd, free_d) = self.ensure_in_reg(&Value::Var(dst.clone()));
                        match op {
                            ArithOp::Add => self.emit(format!("  add {},{},{}", rd, rl, rr)),
                            ArithOp::Sub => self.emit(format!("  sub {},{},{}", rd, rl, rr)),
                            ArithOp::Mul => self.emit(format!("  mul {},{},{}", rd, rl, rr)),
                            ArithOp::Div => self.emit(format!("  div {},{},{}", rd, rl, rr)),
                        }
                        // If destination is a user variable (not a temp), write back the result
                        if !is_temp(dst) {
                            self.note_user_var(dst);
                            self.emit(format!("  store {},{}", rd, dst));
                            if free_d { self.reg.free_reg(rd); }
                        }
                        if free_l { self.reg.free_reg(rl); }
                        if free_r { self.reg.free_reg(rr); }
                    }
                },
                Instr::Load { dst, addr } => {
                    let (ra, free_a) = self.ensure_in_reg(addr);
                    let (rd, _) = self.ensure_in_reg(&Value::Var(dst.clone()));
                    self.emit(format!("  load {},0[{}]", rd, ra));
                    if free_a { self.reg.free_reg(ra); }
                }
                Instr::Store { addr, src } => {
                    let (ra, free_a) = self.ensure_in_reg(addr);
                    let (rs, free_s) = self.ensure_in_reg(src);
                    self.emit(format!("  store {},0[{}]", rs, ra));
                    if free_a { self.reg.free_reg(ra); }
                    if free_s { self.reg.free_reg(rs); }
                }
                Instr::FuncStart { name, params: _ } => {
                    // Start buffering this function to synthesize a minimal prologue/epilogue.
                    self.func_buf = Some(Vec::new());
                    self.func_label = Some(name.clone());
                    self.func_makes_call = false;
                    // Start a fresh allocation region for the function
                    self.reg.begin_region();
                }
                Instr::FuncEnd { name: _ } => {
                    // Flush buffered function with tailored prologue/epilogue
                    self.flush_current_function();
                }
                Instr::Call { func, args, ret } => {
                    // load args into R1..R8 as needed
                    if self.func_buf.is_some() { self.func_makes_call = true; }
                    for (i, a) in args.iter().take(8).enumerate() {
                        let (ra, free_a) = self.ensure_in_reg(a);
                        self.emit(format!("  add R{},R0,{}", i + 1, ra));
                        if free_a { self.reg.free_reg(ra); }
                    }
                    self.emit(format!("  jal {}", func));
                    if let Some(dst) = ret {
                        if is_temp(dst) {
                            // Bind the temp to a register and capture return value there
                            let (rd, _free_d) = self.ensure_in_reg(&Value::Var(dst.clone()));
                            if rd != "R1" {
                                self.emit(format!("  add {},R0,R1", rd));
                            }
                        } else {
                            // Returning directly into a user variable: store from R1
                            self.note_user_var(dst);
                            self.emit(format!("  store R1,{}", dst));
                        }
                    }
                }
                Instr::Return { value } => {
                    if let Some(v) = value {
                        let (rv, free_v) = self.ensure_in_reg(v);
                        self.emit(format!("  add R1,R0,{}", rv));
                        if free_v { self.reg.free_reg(rv); }
                    }
                    // fallthrough to epilogue in our convention
                }
            }
        }
        // Clear current ir after program emission
        self.current_ir = None;
    }

    fn translate_spill_markers(lines: &mut [String], spills: usize) {
        if spills == 0 {
            // Remove any pseudo markers if accidentally present
            for line in lines.iter_mut() {
                if line.trim_start().starts_with("SPILL_STORE") || line.trim_start().starts_with("SPILL_LOAD") {
                    // Replace with a harmless NOP-equivalent comment line
                    *line = String::from("  ; spill eliminated");
                }
            }
            return;
        }
        for line in lines.iter_mut() {
            let trimmed = line.trim_start();
            if let Some(rest) = trimmed.strip_prefix("SPILL_STORE ") {
                // rest is like "R9,idx"
                let parts: Vec<&str> = rest.split(',').collect();
                if parts.len() == 2 {
                    let reg = parts[0].trim();
                    if let Ok(idx) = parts[1].trim().parse::<usize>() {
                        let disp = spills.saturating_sub(idx);
                        *line = format!("  store {},-{}[R13]", reg, disp);
                        continue;
                    }
                }
            } else if let Some(rest) = trimmed.strip_prefix("SPILL_LOAD ") {
                let parts: Vec<&str> = rest.split(',').collect();
                if parts.len() == 2 {
                    let reg = parts[0].trim();
                    if let Ok(idx) = parts[1].trim().parse::<usize>() {
                        let disp = spills.saturating_sub(idx);
                        *line = format!("  load {},-{}[R13]", reg, disp);
                        continue;
                    }
                }
            }
            // leave other lines intact
        }
    }

    fn ensure_in_reg(&mut self, v: &Value) -> (&'static str, bool) {
        // For top-level code (outside any function), ensure we start the region
        // before consulting the allocator so its state doesn't reset mid-instruction.
        self.ensure_top_level_region_started();
        if let Value::Var(name) = v {
            if !is_temp(name) { self.note_user_var(name); }
        }
        // Buffer allocator-requested symbols and allocator-emitted lines
        // to avoid borrow conflicts between `self.reg` and output buffer.
        let mut requested: Vec<String> = Vec::new();
        let mut tmp_out: Vec<String> = Vec::new();
        let res = {
            let mut cb = |sym: &str| requested.push(sym.to_string());
            self.reg.ensure_in_reg(v, &mut tmp_out, &mut cb)
        };
        // Flush allocator-emitted lines into the correct output buffer
        for line in tmp_out { self.emit(line); }
        // After allocator returns, record requested symbols in data section
        for s in requested { self.note_user_var(&s); }
        res
    }

    fn note_user_var(&mut self, name: &str) {
        if !is_temp(name) && !self.user_vars_set.contains(name) {
            self.user_vars_set.insert(name.to_string());
            self.user_vars.push(name.to_string());
        }
    }

    fn emit<S: Into<String>>(&mut self, s: S) {
        let line = s.into();
        if let Some(ref mut buf) = self.func_buf {
            buf.push((line, self.current_ir));
        } else {
            // We are emitting at top-level (not inside a function). Ensure
            // the program start label is placed immediately before the first
            // top-level instruction so that execution begins here, not at
            // the first function definition.
            if !self.emitted_prog_start {
                self.out.push("__prog_start".to_string());
                self.out_map.push(None);
                self.emitted_prog_start = true;
                // Begin a new allocation region for top-level code
                self.reg.begin_region();
            }
            self.out.push(line);
            self.out_map.push(self.current_ir);
        }
    }

    fn flush_current_function(&mut self) {
        // Take the buffer and label out
        let mut body = self.func_buf.take().unwrap_or_default();
        let label = self.func_label.take().unwrap_or_else(|| "__anon_fn".to_string());
        let makes_call = self.func_makes_call;
        self.func_makes_call = false;

        // Close allocator region for this function and get number of spill slots
        let spills = self.reg.end_region();

        // Translate SPILL markers into stack-relative accesses
        Self::translate_spill_markers_in_pairs(&mut body[..], spills);

        // Detect which callee-saved registers are used in the translated body (R9..R12)
        let used_cs = Self::detect_used_callee_saved_pairs(&body);
        let mut saves: Vec<&'static str> = Vec::new();
        if makes_call { saves.push("R14"); }
        for rr in ["R9", "R10", "R11", "R12"].iter() {
            if used_cs.iter().any(|&u| u == *rr) { saves.push(*rr); }
        }

        let frame_slots = saves.len() + spills;

        // Emit label (no mapping)
        self.out.push(label);
        self.out_map.push(None);

        // Prologue
        if frame_slots > 0 {
            // Save callee-saved registers at offsets 0..saves-1
            for (i, reg) in saves.iter().enumerate() {
                self.out.push(format!("  store {},{}[R13]", reg, i));
                self.out_map.push(None);
            }
            // Reserve entire frame (saves + spills)
            self.out.push(format!("  lea R13,{}[R13]", frame_slots));
            self.out_map.push(None);
        }

        // Body
        for (line, map) in body.into_iter() {
            self.out.push(line);
            self.out_map.push(map);
        }

        // Epilogue
        if frame_slots > 0 {
            // Release frame
            self.out.push(format!("  lea R13,-{}[R13]", frame_slots));
            self.out_map.push(None);
            // Restore saved registers
            for (i, reg) in saves.iter().enumerate() {
                self.out.push(format!("  load {},{}[R13]", reg, i));
                self.out_map.push(None);
            }
        }
        self.out.push("  jr R14".to_string());
        self.out_map.push(None);
    }

    fn detect_used_callee_saved(lines: &[String]) -> Vec<&'static str> {
        let mut used = [false; 4]; // R9..R12
        for line in lines {
            // Tokenize by non-alphanumeric characters
            let mut token = String::new();
            for ch in line.chars() {
                if ch.is_alphanumeric() {
                    token.push(ch);
                } else {
                    match token.as_str() {
                        "R9" => used[0] = true,
                        "R10" => used[1] = true,
                        "R11" => used[2] = true,
                        "R12" => used[3] = true,
                        _ => {}
                    }
                    token.clear();
                }
            }
            // Flush last token
            match token.as_str() {
                "R9" => used[0] = true,
                "R10" => used[1] = true,
                "R11" => used[2] = true,
                "R12" => used[3] = true,
                _ => {}
            }
        }
        let mut out = Vec::new();
        if used[0] { out.push("R9"); }
        if used[1] { out.push("R10"); }
        if used[2] { out.push("R11"); }
        if used[3] { out.push("R12"); }
        out
    }

    fn translate_spill_markers_in_pairs(lines: &mut [(String, Option<usize>)], spills: usize) {
        if spills == 0 {
            for (line, _) in lines.iter_mut() {
                if line.trim_start().starts_with("SPILL_STORE") || line.trim_start().starts_with("SPILL_LOAD") {
                    *line = String::from("  ; spill eliminated");
                }
            }
            return;
        }
        for (line, _) in lines.iter_mut() {
            let current = line.clone();
            let mut replaced = false;
            let trimmed = current.trim_start();
            if let Some(rest) = trimmed.strip_prefix("SPILL_STORE ") {
                let parts: Vec<&str> = rest.split(',').collect();
                if parts.len() == 2 {
                    let reg = parts[0].trim();
                    if let Ok(idx) = parts[1].trim().parse::<usize>() {
                        let disp = spills.saturating_sub(idx);
                        *line = format!("  store {},-{}[R13]", reg, disp);
                        replaced = true;
                    }
                }
            }
            if !replaced {
                if let Some(rest) = trimmed.strip_prefix("SPILL_LOAD ") {
                    let parts: Vec<&str> = rest.split(',').collect();
                    if parts.len() == 2 {
                        let reg = parts[0].trim();
                        if let Ok(idx) = parts[1].trim().parse::<usize>() {
                            let disp = spills.saturating_sub(idx);
                            *line = format!("  load {},-{}[R13]", reg, disp);
                        }
                    }
                }
            }
        }
    }

    fn detect_used_callee_saved_pairs(lines: &[(String, Option<usize>)]) -> Vec<&'static str> {
        let only_lines: Vec<String> = lines.iter().map(|(s, _)| s.clone()).collect();
        Self::detect_used_callee_saved(&only_lines)
    }
}
