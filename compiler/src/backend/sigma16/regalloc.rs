use crate::ir::Value;
use std::collections::HashMap;

/// Abstraction over register allocation and operand materialization.
///
/// Implementations can emit instructions into `out` to materialize a `Value`
/// into a register and return the chosen register along with a boolean flag
/// indicating whether the caller should free that register after use.
pub trait RegAllocator {
    fn ensure_in_reg<F: FnMut(&str)>(
        &mut self,
        v: &Value,
        out: &mut Vec<String>,
        note_user_var: F,
    ) -> (&'static str, bool);

    fn free_reg(&mut self, r: &'static str);

    // Region management (top-level block or a function body). Default no-ops.
    fn begin_region(&mut self) {}
    fn end_region(&mut self) -> usize { 0 }
}

/// Which allocator to use. `Basic` mirrors the previous behavior; `Advanced`
/// is a placeholder for a future sophisticated allocator with spilling.
#[derive(Clone, Copy, Debug)]
pub enum AllocatorKind {
    Basic,
    Advanced,
}

/// A very simple linear-scan style allocator backed by a free-list.
/// This mirrors the old behavior and serves as a baseline implementation.
pub struct BasicRegAllocator {
    /// Free register pool (strings like "R1")
    free: Vec<&'static str>,
    /// Mapping of temporary names ("__t*") to a held register
    temp_regs: HashMap<String, &'static str>,
}

impl BasicRegAllocator {
    pub fn new() -> Self {
        // R1–R12 as general-purpose pool (keep R13/R14 reserved; R15 control)
        let mut pool: Vec<&'static str> = vec![
            "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10", "R11", "R12",
        ];
        // Allocate from the end for efficient pop()
        pool.reverse();
        Self { free: pool, temp_regs: HashMap::new() }
    }

    fn alloc_reg(&mut self) -> Option<&'static str> { self.free.pop() }

    #[allow(dead_code)]
    pub fn bind_temp(&mut self, name: &str, r: &'static str) { self.temp_regs.insert(name.to_string(), r); }
}

pub(crate) fn is_temp(name: &str) -> bool { name.starts_with("__t") }

/// An advanced allocator scaffold. For now, it delegates to `BasicRegAllocator`
/// but provides hooks/fields where future liveness/spilling logic will live.
pub struct AdvancedRegAllocator {
    // Preferred callee-saved pool to keep temps live across calls
    free: Vec<&'static str>, // e.g., [R12, R11, R10, R9]
    // temp name -> location
    temp_loc: HashMap<String, TempLoc>,
    // register -> temp name (only for registers currently bound to temps)
    reg_to_temp: HashMap<&'static str, String>,
    // simple LRU timestamp per temp
    last_use: HashMap<String, u64>,
    tick: u64,
    // Per-region spill slot mapping: temp -> slot index, and count
    spill_map: HashMap<String, usize>,
    spill_count: usize,
}

#[derive(Clone, Copy, Debug)]
enum TempLoc {
    Reg(&'static str),
    Spilled(usize), // index into spill_slots
}

impl AdvancedRegAllocator {
    pub fn new() -> Self {
        // Use callee-saved R9..R12 which our prologue/epilogue preserves
        let mut pool: Vec<&'static str> = vec!["R9", "R10", "R11", "R12"];
        pool.reverse(); // pop from end
        Self {
            free: pool,
            temp_loc: HashMap::new(),
            reg_to_temp: HashMap::new(),
            last_use: HashMap::new(),
            tick: 0,
            spill_map: HashMap::new(),
            spill_count: 0,
        }
    }

    #[allow(dead_code)]
    pub fn plan_allocation_for_function(&mut self) {
        // Placeholder: in the future compute liveness or intervals
    }

    fn bump_use(&mut self, t: &str) {
        self.tick = self.tick.wrapping_add(1);
        self.last_use.insert(t.to_string(), self.tick);
    }

    fn alloc_free_reg(&mut self) -> Option<&'static str> { self.free.pop() }

    fn ensure_spill_slot(&mut self, t: &str) -> usize {
        if let Some(&idx) = self.spill_map.get(t) { return idx; }
        let idx = self.spill_count;
        self.spill_count += 1;
        self.spill_map.insert(t.to_string(), idx);
        // Record as spilled if not currently in a register
        self.temp_loc.insert(t.to_string(), TempLoc::Spilled(idx));
        idx
    }

    fn spill_victim(
        &mut self,
        out: &mut Vec<String>,
    ) -> Option<&'static str> {
        // Pick a victim among currently bound temps using LRU
        if self.reg_to_temp.is_empty() { return None; }
        let mut victim_reg: Option<&'static str> = None;
        let mut oldest_tick: u64 = u64::MAX;
        for (reg, temp) in self.reg_to_temp.iter() {
            let t = *self.last_use.get(temp).unwrap_or(&0);
            if t < oldest_tick {
                oldest_tick = t;
                victim_reg = Some(*reg);
            }
        }
        if let Some(reg) = victim_reg {
            if let Some(temp) = self.reg_to_temp.remove(reg) {
                // Ensure we have a spill slot index (relative within region)
                let idx = self.ensure_spill_slot(&temp);
                // Emit pseudo store to spill slot (to be fixed up by codegen)
                out.push(format!("  SPILL_STORE {},{}", reg, idx));
                // Mark temp as spilled
                self.temp_loc.insert(temp, TempLoc::Spilled(idx));
                // The register becomes free to use
                return Some(reg);
            }
        }
        None
    }

    fn alloc_or_spill<F: FnMut(&str)>(
        &mut self,
        out: &mut Vec<String>,
        _note_user_var: F,
    ) -> &'static str {
        if let Some(r) = self.alloc_free_reg() { return r; }
        if let Some(r) = self.spill_victim(out) { return r; }
        // Fallback: in pathological cases, use R4 as an emergency scratch (volatile)
        // This should rarely happen unless expressions are extremely deep.
        "R4"
    }
}

impl RegAllocator for BasicRegAllocator {
    fn ensure_in_reg<F: FnMut(&str)>(
        &mut self,
        v: &Value,
        out: &mut Vec<String>,
        mut note_user_var: F,
    ) -> (&'static str, bool) {
        match v {
            Value::Imm(i) => {
                if let Some(r) = self.alloc_reg() {
                    out.push(format!("  lea {},{}[R0]", r, i));
                    (r, true)
                } else {
                    ("R1", false) // fallback, should not happen with small programs
                }
            }
            Value::Var(name) => {
                if is_temp(name) {
                    if let Some(&r) = self.temp_regs.get(name) { return (r, false); }
                    if let Some(r) = self.alloc_reg() {
                        self.temp_regs.insert(name.clone(), r);
                        (r, false)
                    } else {
                        ("R2", false)
                    }
                } else {
                    // user var from memory
                    if let Some(r) = self.alloc_reg() {
                        note_user_var(name);
                        out.push(format!("  load {},{}", r, name));
                        (r, true)
                    } else { ("R3", false) }
                }
            }
            Value::AddrOf(name) => {
                if let Some(r) = self.alloc_reg() {
                    out.push(format!("  lea {},{}[R0]", r, name));
                    (r, true)
                } else { ("R4", false) }
            }
            Value::Arg(i) => {
                let r = match *i { 1 => "R1", 2 => "R2", 3 => "R3", 4 => "R4", 5 => "R5", 6 => "R6", 7 => "R7", 8 => "R8", _ => "R1" };
                (r, false)
            }
        }
    }

    fn free_reg(&mut self, r: &'static str) { self.free.push(r); }
}

impl RegAllocator for AdvancedRegAllocator {
    fn ensure_in_reg<F: FnMut(&str)>(
        &mut self,
        v: &Value,
        out: &mut Vec<String>,
        mut note_user_var: F,
    ) -> (&'static str, bool) {
        match v {
            Value::Imm(i) => {
                let r = self.alloc_or_spill(out, &mut note_user_var);
                out.push(format!("  lea {},{}[R0]", r, i));
                (r, true)
            }
            Value::Var(name) => {
                if is_temp(name) {
                    self.bump_use(name);
                    match self.temp_loc.get(name).copied() {
                        Some(TempLoc::Reg(r)) => { return (r, false); }
                        Some(TempLoc::Spilled(idx)) => {
                            // Bring back from spill
                            let r = self.alloc_or_spill(out, &mut note_user_var);
                            // Pseudo load from spill slot; fixed in codegen
                            out.push(format!("  SPILL_LOAD {},{}", r, idx));
                            // Bind to register
                            self.temp_loc.insert(name.clone(), TempLoc::Reg(r));
                            self.reg_to_temp.insert(r, name.clone());
                            (r, false)
                        }
                        None => {
                            // First sighting of this temp: assign a register
                            let r = self.alloc_or_spill(out, &mut note_user_var);
                            self.temp_loc.insert(name.clone(), TempLoc::Reg(r));
                            self.reg_to_temp.insert(r, name.clone());
                            (r, false)
                        }
                    }
                } else {
                    // user var: load from memory into scratch reg
                    let r = self.alloc_or_spill(out, &mut note_user_var);
                    note_user_var(name); // make sure symbol exists
                    out.push(format!("  load {},{}", r, name));
                    (r, true)
                }
            }
            Value::AddrOf(name) => {
                let r = self.alloc_or_spill(out, &mut note_user_var);
                out.push(format!("  lea {},{}[R0]", r, name));
                (r, true)
            }
            Value::Arg(i) => {
                let r = match *i { 1 => "R1", 2 => "R2", 3 => "R3", 4 => "R4", 5 => "R5", 6 => "R6", 7 => "R7", 8 => "R8", _ => "R1" };
                (r, false)
            }
        }
    }

    fn free_reg(&mut self, r: &'static str) {
        // Only free if the register is unbound (scratch). If bound to a temp, ignore.
        if self.reg_to_temp.contains_key(r) { return; }
        // Only return callee-saved pool registers back to the free list
        match r {
            "R9" | "R10" | "R11" | "R12" => self.free.push(r),
            _ => {}
        }
    }

    fn begin_region(&mut self) {
        // Reset allocator state for a new region (function or top-level block)
        self.free = vec!["R12", "R11", "R10", "R9"]; // reversed pool
        self.temp_loc.clear();
        self.reg_to_temp.clear();
        self.last_use.clear();
        self.tick = 0;
        self.spill_map.clear();
        self.spill_count = 0;
    }

    fn end_region(&mut self) -> usize {
        let n = self.spill_count;
        // Clear bindings to prevent cross-region leakage
        self.temp_loc.clear();
        self.reg_to_temp.clear();
        self.spill_map.clear();
        self.spill_count = 0;
        n
    }
}
