//! Liveness analysis for IR regions (functions or top-level code).
//!
//! Computes per-instruction live variable sets using iterative dataflow
//! analysis over basic blocks.  Used by the advanced register allocator
//! to avoid spilling dead variables and to choose optimal spill victims.

use crate::ir::{Instr, Rhs, Value, Var};
use std::collections::{HashMap, HashSet};

/// Liveness information for a region of IR instructions.
pub struct LivenessInfo {
    /// For each instruction index (relative to region start), the set of
    /// variables that are live AFTER the instruction executes.
    live_after: Vec<HashSet<Var>>,
    /// Offset of the region start in the global instruction list.
    offset: usize,
    /// For each instruction index (relative to region start), maps each live
    /// variable to the global index of its next use.  Variables not present
    /// have no subsequent use in the region.
    next_use: Vec<HashMap<Var, usize>>,
}

impl LivenessInfo {
    /// Check if a variable is live after instruction at global index `idx`.
    pub fn is_live_after(&self, idx: usize, var: &Var) -> bool {
        match idx.checked_sub(self.offset) {
            Some(i) => self.live_after.get(i).map_or(true, |s| s.contains(var)),
            None => true,
        }
    }

    /// Return the global index of the next use of `var` strictly after
    /// instruction `idx`.  Returns `usize::MAX` if there is no subsequent use.
    pub fn next_use_after(&self, idx: usize, var: &Var) -> usize {
        match idx.checked_sub(self.offset) {
            Some(i) => self
                .next_use
                .get(i)
                .and_then(|m| m.get(var))
                .copied()
                .unwrap_or(usize::MAX),
            None => 0, // conservative: assume immediate use
        }
    }
}

// ── Basic block representation ──────────────────────────────────────────

struct BasicBlock {
    start: usize,
    end: usize,             // inclusive
    successors: Vec<usize>, // block indices
}

// ── Instruction-level use/def extraction ────────────────────────────────

/// Extract variables used (read) by an instruction.
fn get_uses(instr: &Instr) -> Vec<Var> {
    let mut uses = Vec::new();
    let mut add_val = |v: &Value| {
        if let Value::Var(var) = v {
            uses.push(var.clone());
        }
    };
    match instr {
        Instr::Assign {
            src: Rhs::Value(v), ..
        } => add_val(v),
        Instr::Assign {
            src: Rhs::Binary { left, right, .. },
            ..
        } => {
            add_val(left);
            add_val(right);
        }
        Instr::Assign {
            src: Rhs::Unary { operand, .. },
            ..
        } => {
            add_val(operand);
        }
        Instr::IfCmpGoto { left, right, .. } => {
            add_val(left);
            add_val(right);
        }
        Instr::Call { args, .. } => {
            for a in args {
                add_val(a);
            }
        }
        Instr::Return { value: Some(v) } => add_val(v),
        Instr::Load { addr, .. } => add_val(addr),
        Instr::Store { addr, src } => {
            add_val(addr);
            add_val(src);
        }
        Instr::ArrayLoad { index, .. } => add_val(index),
        Instr::ArrayStore { index, src, .. } => {
            add_val(index);
            add_val(src);
        }
        _ => {}
    }
    uses
}

/// Extract the variable defined (written) by an instruction, if any.
fn get_def(instr: &Instr) -> Option<Var> {
    match instr {
        Instr::Assign { dst, .. } => Some(dst.clone()),
        Instr::Load { dst, .. } => Some(dst.clone()),
        Instr::ArrayLoad { dst, .. } => Some(dst.clone()),
        Instr::Call { ret: Some(dst), .. } => Some(dst.clone()),
        _ => None,
    }
}

// ── Core dataflow analysis ──────────────────────────────────────────────

/// Compute liveness information for a region of IR instructions.
///
/// `instrs` is the full instruction list.  The region spans `[start, end)`.
pub fn compute_liveness(instrs: &[Instr], start: usize, end: usize) -> LivenessInfo {
    let region = &instrs[start..end];
    let n = region.len();

    if n == 0 {
        return LivenessInfo {
            live_after: Vec::new(),
            offset: start,
            next_use: Vec::new(),
        };
    }

    // Step 1: Identify basic block boundaries.
    let mut block_starts: HashSet<usize> = HashSet::new();
    block_starts.insert(0);

    let mut label_to_idx: HashMap<&str, usize> = HashMap::new();
    for (i, instr) in region.iter().enumerate() {
        match instr {
            Instr::Label(lbl) => {
                block_starts.insert(i);
                label_to_idx.insert(lbl.as_str(), i);
            }
            Instr::Goto(_) | Instr::Return { .. } => {
                if i + 1 < n {
                    block_starts.insert(i + 1);
                }
            }
            Instr::IfCmpGoto { .. } => {
                if i + 1 < n {
                    block_starts.insert(i + 1);
                }
            }
            _ => {}
        }
    }

    let mut sorted_starts: Vec<usize> = block_starts.into_iter().collect();
    sorted_starts.sort();

    // Step 2: Build basic blocks with successor edges.
    let start_to_block: HashMap<usize, usize> = sorted_starts
        .iter()
        .enumerate()
        .map(|(bi, &si)| (si, bi))
        .collect();

    let mut blocks: Vec<BasicBlock> = Vec::new();
    for (bi, &block_start) in sorted_starts.iter().enumerate() {
        let block_end = if bi + 1 < sorted_starts.len() {
            sorted_starts[bi + 1] - 1
        } else {
            n - 1
        };

        let mut successors = Vec::new();
        let last_instr = &region[block_end];

        match last_instr {
            Instr::Goto(lbl) => {
                if let Some(&target_idx) = label_to_idx.get(lbl.as_str()) {
                    if let Some(&target_block) = start_to_block.get(&target_idx) {
                        successors.push(target_block);
                    }
                }
            }
            Instr::IfCmpGoto { target, .. } => {
                if let Some(&target_idx) = label_to_idx.get(target.as_str()) {
                    if let Some(&target_block) = start_to_block.get(&target_idx) {
                        successors.push(target_block);
                    }
                }
                if bi + 1 < sorted_starts.len() {
                    successors.push(bi + 1);
                }
            }
            Instr::Return { .. } | Instr::FuncEnd { .. } => {
                // No successors.
            }
            _ => {
                if bi + 1 < sorted_starts.len() {
                    successors.push(bi + 1);
                }
            }
        }

        blocks.push(BasicBlock {
            start: block_start,
            end: block_end,
            successors,
        });
    }

    // Step 3: Compute gen/kill sets per block.
    let num_blocks = blocks.len();
    let mut gen_sets: Vec<HashSet<Var>> = vec![HashSet::new(); num_blocks];
    let mut kill: Vec<HashSet<Var>> = vec![HashSet::new(); num_blocks];

    for (bi, block) in blocks.iter().enumerate() {
        for i in block.start..=block.end {
            for var in get_uses(&region[i]) {
                if !kill[bi].contains(&var) {
                    gen_sets[bi].insert(var);
                }
            }
            if let Some(def) = get_def(&region[i]) {
                kill[bi].insert(def);
            }
        }
    }

    // Step 4: Iterative backward dataflow.
    let mut live_in: Vec<HashSet<Var>> = vec![HashSet::new(); num_blocks];
    let mut live_out: Vec<HashSet<Var>> = vec![HashSet::new(); num_blocks];
    let mut changed = true;

    while changed {
        changed = false;
        for bi in (0..num_blocks).rev() {
            // live_out[B] = ∪ live_in[S] for all successors S
            let mut new_out = HashSet::new();
            for &succ in &blocks[bi].successors {
                for var in &live_in[succ] {
                    new_out.insert(var.clone());
                }
            }

            // live_in[B] = gen[B] ∪ (live_out[B] − kill[B])
            let mut new_in = gen_sets[bi].clone();
            for var in &new_out {
                if !kill[bi].contains(var) {
                    new_in.insert(var.clone());
                }
            }

            if new_in != live_in[bi] || new_out != live_out[bi] {
                changed = true;
                live_in[bi] = new_in;
                live_out[bi] = new_out;
            }
        }
    }

    // Step 5: Expand to per-instruction liveness.
    let mut live_after_vec: Vec<HashSet<Var>> = vec![HashSet::new(); n];

    for (bi, block) in blocks.iter().enumerate() {
        let mut live = live_out[bi].clone();
        for i in (block.start..=block.end).rev() {
            live_after_vec[i] = live.clone();
            if let Some(def) = get_def(&region[i]) {
                live.remove(&def);
            }
            for var in get_uses(&region[i]) {
                live.insert(var);
            }
        }
    }

    // Step 6: Compute per-instruction next-use distances.
    //
    // next_use_vec[i][var] = global index of the first instruction > i that
    // reads `var`.  We scan backwards: when we encounter a use of `var` at
    // position i, we record i (global) as the upcoming use for all earlier
    // positions.
    let mut next_use_vec: Vec<HashMap<Var, usize>> = vec![HashMap::new(); n];
    let mut upcoming: HashMap<Var, usize> = HashMap::new();

    for i in (0..n).rev() {
        // Snapshot: for position i, next-use is whatever we've accumulated
        // from positions > i.
        next_use_vec[i] = upcoming.clone();

        // Now record that position i uses these variables, so any position
        // < i will see this as the nearest upcoming use.
        for var in get_uses(&region[i]) {
            upcoming.insert(var, i + start);
        }
    }

    LivenessInfo {
        live_after: live_after_vec,
        offset: start,
        next_use: next_use_vec,
    }
}
