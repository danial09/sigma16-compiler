use crate::ir::Value;
use std::collections::{HashMap, HashSet};

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
    /// Reverse map: register -> temporary name (only for temps currently bound)
    reg_to_temp: HashMap<&'static str, String>,
    /// Mapping of user variables to a resident register (to avoid reloads)
    user_regs: HashMap<String, &'static str>,
    /// Reverse map: register -> user variable (only for user vars currently bound)
    reg_to_user: HashMap<&'static str, String>,
    /// Reserved scratch registers used only as last-resort materialization
    /// when the free pool is exhausted. We do not bind temps or user vars to
    /// these registers, and they are never part of the free list.
    scratch_in_use: HashSet<&'static str>,
}

impl BasicRegAllocator {
    pub fn new() -> Self {
        // R1–R10 as general-purpose pool for Basic allocator.
        // Keep R11/R12 reserved as dedicated scratch registers.
        // R13/R14 are reserved by ABI; R15 is control.
        let mut pool: Vec<&'static str> = vec![
            "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
        ];
        // Allocate from the end for efficient pop()
        pool.reverse();
        Self {
            free: pool,
            temp_regs: HashMap::new(),
            reg_to_temp: HashMap::new(),
            user_regs: HashMap::new(),
            reg_to_user: HashMap::new(),
            scratch_in_use: HashSet::new(),
        }
    }

    fn alloc_reg(&mut self) -> Option<&'static str> { self.free.pop() }

    /// Allocate one of the reserved scratch registers (R11 or R12) if available.
    /// Returns None if both are already in-use for the current expression/sequence.
    fn alloc_scratch(&mut self) -> Option<&'static str> {
        for r in ["R11", "R12"].iter() {
            if !self.scratch_in_use.contains(r) {
                self.scratch_in_use.insert(r);
                return Some(*r);
            }
        }
        None
    }

    #[allow(dead_code)]
    pub fn bind_temp(&mut self, name: &str, r: &'static str) {
        self.temp_regs.insert(name.to_string(), r);
        self.reg_to_temp.insert(r, name.to_string());
    }
}

pub(crate) fn is_temp(name: &str) -> bool { name.starts_with("__t") }

/// An advanced allocator scaffold. For now, it delegates to `BasicRegAllocator`
/// but provides hooks/fields where future liveness/spilling logic will live.
/// AdvancedRegAllocator is currently a thin scaffold that delegates to
/// BasicRegAllocator. This keeps the API surface intact while we iterate
/// on the basic allocator. In the future, this type will implement real
/// liveness/spilling. For now it mirrors Basic behavior exactly.
pub struct AdvancedRegAllocator {
    inner: BasicRegAllocator,
}

impl AdvancedRegAllocator {
    pub fn new() -> Self { Self { inner: BasicRegAllocator::new() } }
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
                    return (r, true);
                }
                if let Some(r) = self.alloc_scratch() {
                    out.push(format!("  lea {},{}[R0]", r, i));
                    return (r, true);
                }
                // Ultimate fallback: reuse R11 (should be rare). Mark as not freeable to avoid double-use.
                out.push(format!("  lea {},{}[R0]", "R11", i));
                ("R11", true)
            }
            Value::Var(name) => {
                if is_temp(name) {
                    if let Some(&r) = self.temp_regs.get(name) { return (r, false); }
                    if let Some(r) = self.alloc_reg() {
                        self.temp_regs.insert(name.clone(), r);
                        self.reg_to_temp.insert(r, name.clone());
                        (r, false)
                    } else if let Some((&victim_r, _)) = self.reg_to_user.iter().next() {
                        // Evict a user-var binding to secure a real register for the temp.
                        if let Some(victim_name) = self.reg_to_user.remove(&victim_r) {
                            self.user_regs.remove(&victim_name);
                        }
                        self.temp_regs.insert(name.clone(), victim_r);
                        self.reg_to_temp.insert(victim_r, name.clone());
                        (victim_r, false)
                    } else {
                        // As a last resort only (should be extremely rare), use a scratch
                        // transiently. This risks losing the temp value after use, so we
                        // strongly prefer eviction above.
                        if let Some(r) = self.alloc_scratch() { return (r, true); }
                        ("R11", true)
                    }
                } else {
                    // User variable: keep a resident binding to avoid redundant loads.
                    if let Some(&r) = self.user_regs.get(name) {
                        return (r, false);
                    }
                    // Try to allocate a new register; if none, recycle one from another user var.
                    let r = if let Some(r) = self.alloc_reg() {
                        r
                    } else if let Some((&victim_r, _)) = self.reg_to_user.iter().next() {
                        // Evict the victim's binding (memory already reflects latest value after stores)
                        if let Some(victim_name) = self.reg_to_user.remove(&victim_r) {
                            self.user_regs.remove(&victim_name);
                        }
                        victim_r
                    } else if let Some(r) = self.alloc_scratch() {
                        // Use a reserved scratch register without caching
                        note_user_var(name);
                        out.push(format!("  load {},{}", r, name));
                        return (r, true);
                    } else {
                        // As a last resort, use a volatile general scratch (R3) without caching
                        note_user_var(name);
                        out.push(format!("  load {},{}", "R3", name));
                        return ("R3", true);
                    };
                    // First touch: ensure symbol and load once, then keep cached binding.
                    note_user_var(name);
                    out.push(format!("  load {},{}", r, name));
                    self.user_regs.insert(name.clone(), r);
                    self.reg_to_user.insert(r, name.clone());
                    (r, false)
                }
            }
            Value::AddrOf(name) => {
                if let Some(r) = self.alloc_reg() {
                    out.push(format!("  lea {},{}[R0]", r, name));
                    return (r, true);
                }
                if let Some(r) = self.alloc_scratch() {
                    out.push(format!("  lea {},{}[R0]", r, name));
                    return (r, true);
                }
                ("R11", true)
            }
            Value::Arg(i) => {
                let r = match *i { 1 => "R1", 2 => "R2", 3 => "R3", 4 => "R4", 5 => "R5", 6 => "R6", 7 => "R7", 8 => "R8", _ => "R1" };
                (r, false)
            }
        }
    }

    fn free_reg(&mut self, r: &'static str) {
        // Reserved scratch registers are not part of the general pool.
        if r == "R11" || r == "R12" {
            self.scratch_in_use.remove(&r);
            return;
        }
        // Do not free registers that are currently bound to temps or user vars
        if self.reg_to_temp.contains_key(&r) { return; }
        if self.reg_to_user.contains_key(&r) { return; }
        self.free.push(r);
    }

    fn begin_region(&mut self) {
        // Reset allocator state for a new region (function or top-level block)
        let mut pool: Vec<&'static str> = vec![
            "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8", "R9", "R10",
        ];
        pool.reverse();
        self.free = pool;
        self.temp_regs.clear();
        self.reg_to_temp.clear();
        self.user_regs.clear();
        self.reg_to_user.clear();
        self.scratch_in_use.clear();
    }

    fn end_region(&mut self) -> usize { 0 }
}

impl RegAllocator for AdvancedRegAllocator {
    fn ensure_in_reg<F: FnMut(&str)>(
        &mut self,
        v: &Value,
        out: &mut Vec<String>,
        note_user_var: F,
    ) -> (&'static str, bool) {
        self.inner.ensure_in_reg(v, out, note_user_var)
    }

    fn free_reg(&mut self, r: &'static str) { self.inner.free_reg(r); }

    fn begin_region(&mut self) { self.inner.begin_region(); }
    fn end_region(&mut self) -> usize { self.inner.end_region() }
}
