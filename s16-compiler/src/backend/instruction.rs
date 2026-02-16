//! Sigma16 machine-instruction types and assembly output items.
//!
//! This module defines the typed instruction set (`S16Instr`), displacement
//! operands (`Disp`), condition codes (`Cond`), and the structured assembly
//! output type (`AsmItem`) used throughout the backend.

use super::abi::Register;
use std::fmt;

// ============================================================================
// Displacement (address operand)
// ============================================================================

/// A displacement value used in RX-format instructions.
///
/// Can be a numeric constant (`0`, `-3`, `42`) or a symbolic label (`x`,
/// `loop_start`, `stack`).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Disp {
    /// Numeric constant displacement.
    Num(i64),
    /// Symbolic label reference.
    Label(String),
}

impl fmt::Display for Disp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Disp::Num(n) => write!(f, "{n}"),
            Disp::Label(s) => write!(f, "{s}"),
        }
    }
}

// ============================================================================
// Condition codes (for conditional jumps)
// ============================================================================

/// Condition code for conditional branch pseudo-instructions.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Cond {
    Eq,
    Ne,
    Lt,
    Gt,
    Le,
    Ge,
}

impl Cond {
    /// Returns the assembly mnemonic for this condition.
    pub fn mnemonic(&self) -> &'static str {
        match self {
            Cond::Eq => "jumpeq",
            Cond::Ne => "jumpne",
            Cond::Lt => "jumplt",
            Cond::Gt => "jumpgt",
            Cond::Le => "jumple",
            Cond::Ge => "jumpge",
        }
    }
}

// ============================================================================
// Typed Sigma16 instruction
// ============================================================================

/// A typed Sigma16 machine instruction.
///
/// Instructions are organised by format:
///
/// - **RRR** (`op Rd,Ra,Rb`): register-to-register operations.
/// - **RX** (`op Rd,disp[Rb]`): memory / immediate / branch operations.
/// - **Conditional jump** (`jumpCC disp[Rb]`): pseudo-instructions that
///   expand to an EXP-format instruction; modelled separately because
///   the condition is encoded in the mnemonic rather than a register field.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum S16Instr {
    // ── RRR format ──────────────────────────────────────────────────────
    /// `add Rd,Ra,Rb`
    Add {
        d: Register,
        a: Register,
        b: Register,
    },
    /// `sub Rd,Ra,Rb`
    Sub {
        d: Register,
        a: Register,
        b: Register,
    },
    /// `mul Rd,Ra,Rb`
    Mul {
        d: Register,
        a: Register,
        b: Register,
    },
    /// `div Rd,Ra,Rb` — quotient in Rd, remainder in R15
    Div {
        d: Register,
        a: Register,
        b: Register,
    },
    /// `cmp Ra,Rb` — result in R15 (Rd is conventionally R0)
    Cmp { a: Register, b: Register },
    /// `trap Rd,Ra,Rb` — system call / halt
    Trap {
        d: Register,
        a: Register,
        b: Register,
    },

    // ── RX format ───────────────────────────────────────────────────────
    /// `lea Rd,disp[Rb]` — load effective address
    Lea {
        d: Register,
        disp: Disp,
        idx: Register,
    },
    /// `load Rd,disp[Rb]` — load word from memory
    Load {
        d: Register,
        disp: Disp,
        idx: Register,
    },
    /// `store Rd,disp[Rb]` — store word to memory
    Store {
        d: Register,
        disp: Disp,
        idx: Register,
    },
    /// `jump disp[Rb]` — unconditional jump
    Jump { disp: Disp, idx: Register },
    /// `jal Rd,disp[Rb]` — jump and link (call)
    Jal {
        d: Register,
        disp: Disp,
        idx: Register,
    },

    // ── Conditional jump pseudo-instructions ─────────────────────────────
    /// `jumpCC disp[Rb]` — conditional branch (reads condition from R15)
    JumpCond {
        cond: Cond,
        disp: Disp,
        idx: Register,
    },
}

// ── Convenience constructors ────────────────────────────────────────────

impl S16Instr {
    /// `add Rd,R0,Rs` — idiomatic register-to-register move.
    pub fn mov(d: Register, s: Register) -> Self {
        S16Instr::Add {
            d,
            a: Register::ZERO_REG,
            b: s,
        }
    }

    /// `lea Rd,imm[R0]` — load a numeric immediate into Rd.
    pub fn lea_imm(d: Register, imm: i64) -> Self {
        S16Instr::Lea {
            d,
            disp: Disp::Num(imm),
            idx: Register::ZERO_REG,
        }
    }

    /// `lea Rd,label[R0]` — load a label address into Rd.
    pub fn lea_label(d: Register, label: impl Into<String>) -> Self {
        S16Instr::Lea {
            d,
            disp: Disp::Label(label.into()),
            idx: Register::ZERO_REG,
        }
    }

    /// `load Rd,disp[Rb]` with a numeric displacement.
    pub fn load_disp(d: Register, disp: i64, base: Register) -> Self {
        S16Instr::Load {
            d,
            disp: Disp::Num(disp),
            idx: base,
        }
    }

    /// `load Rd,label[R0]` — load from a labelled global.
    pub fn load_label(d: Register, label: impl Into<String>) -> Self {
        S16Instr::Load {
            d,
            disp: Disp::Label(label.into()),
            idx: Register::ZERO_REG,
        }
    }

    /// `store Rd,disp[Rb]` with a numeric displacement.
    pub fn store_disp(d: Register, disp: i64, base: Register) -> Self {
        S16Instr::Store {
            d,
            disp: Disp::Num(disp),
            idx: base,
        }
    }

    /// `store Rd,label[R0]` — store to a labelled global.
    pub fn store_label(d: Register, label: impl Into<String>) -> Self {
        S16Instr::Store {
            d,
            disp: Disp::Label(label.into()),
            idx: Register::ZERO_REG,
        }
    }

    /// `jump label[R0]` — unconditional jump to a label.
    pub fn jump_label(label: impl Into<String>) -> Self {
        S16Instr::Jump {
            disp: Disp::Label(label.into()),
            idx: Register::ZERO_REG,
        }
    }

    /// `jal Rd,label[R0]` — call a named function.
    pub fn jal_label(d: Register, label: impl Into<String>) -> Self {
        S16Instr::Jal {
            d,
            disp: Disp::Label(label.into()),
            idx: Register::ZERO_REG,
        }
    }

    /// `trap R0,R0,R0` — halt the machine.
    pub fn trap_halt() -> Self {
        S16Instr::Trap {
            d: Register::ZERO_REG,
            a: Register::ZERO_REG,
            b: Register::ZERO_REG,
        }
    }

    /// Returns `true` if the instruction references the given register in
    /// any operand position.
    pub fn uses_register(&self, reg: Register) -> bool {
        match self {
            S16Instr::Add { d, a, b }
            | S16Instr::Sub { d, a, b }
            | S16Instr::Mul { d, a, b }
            | S16Instr::Div { d, a, b }
            | S16Instr::Trap { d, a, b } => *d == reg || *a == reg || *b == reg,

            S16Instr::Cmp { a, b } => *a == reg || *b == reg,

            S16Instr::Lea { d, idx, .. }
            | S16Instr::Load { d, idx, .. }
            | S16Instr::Store { d, idx, .. }
            | S16Instr::Jal { d, idx, .. } => *d == reg || *idx == reg,

            S16Instr::Jump { idx, .. } | S16Instr::JumpCond { idx, .. } => *idx == reg,
        }
    }

    /// Returns `true` if this is a `jal` (call) instruction.
    pub fn is_call(&self) -> bool {
        matches!(self, S16Instr::Jal { .. })
    }

    /// Returns the destination register that this instruction writes to, if any.
    pub fn dest_reg(&self) -> Option<Register> {
        match self {
            S16Instr::Add { d, .. }
            | S16Instr::Sub { d, .. }
            | S16Instr::Mul { d, .. }
            | S16Instr::Div { d, .. }
            | S16Instr::Trap { d, .. }
            | S16Instr::Lea { d, .. }
            | S16Instr::Load { d, .. }
            | S16Instr::Jal { d, .. } => Some(*d),
            S16Instr::Store { .. }
            | S16Instr::Jump { .. }
            | S16Instr::JumpCond { .. }
            | S16Instr::Cmp { .. } => None,
        }
    }

    /// If this is an unconditional `jump`, return the target label (if symbolic).
    #[allow(dead_code)]
    pub fn jump_target(&self) -> Option<&str> {
        match self {
            S16Instr::Jump {
                disp: Disp::Label(t),
                ..
            } => Some(t.as_str()),
            _ => None,
        }
    }
}

// ── Display — lower to assembly text ────────────────────────────────────

impl fmt::Display for S16Instr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            // RRR
            S16Instr::Add { d, a, b } => write!(f, "  add {d},{a},{b}"),
            S16Instr::Sub { d, a, b } => write!(f, "  sub {d},{a},{b}"),
            S16Instr::Mul { d, a, b } => write!(f, "  mul {d},{a},{b}"),
            S16Instr::Div { d, a, b } => write!(f, "  div {d},{a},{b}"),
            S16Instr::Cmp { a, b } => write!(f, "  cmp {a},{b}"),
            S16Instr::Trap { d, a, b } => write!(f, "  trap {d},{a},{b}"),

            // RX
            S16Instr::Lea { d, disp, idx } => write!(f, "  lea {d},{disp}[{idx}]"),
            S16Instr::Load { d, disp, idx } => write!(f, "  load {d},{disp}[{idx}]"),
            S16Instr::Store { d, disp, idx } => write!(f, "  store {d},{disp}[{idx}]"),
            S16Instr::Jump { disp, idx } => write!(f, "  jump {disp}[{idx}]"),
            S16Instr::Jal { d, disp, idx } => write!(f, "  jal {d},{disp}[{idx}]"),

            // Conditional jumps
            S16Instr::JumpCond { cond, disp, idx } => {
                write!(f, "  {} {disp}[{idx}]", cond.mnemonic())
            }
        }
    }
}

// ============================================================================
// Annotated instruction (regalloc output)
// ============================================================================

/// An instruction paired with an optional assembly comment.
/// Used as the output type for register allocator operations.
pub type AnnotatedInstr = (S16Instr, Option<String>);

// ============================================================================
// AsmItem — top-level assembly output element
// ============================================================================

/// A structured assembly output element.
///
/// The backend emits a tree of `AsmItem`s that is later flattened to text.
/// The optimizer (`optimize.rs`) works on this structured representation
/// before final emission.
#[derive(Debug, Clone)]
pub enum AsmItem {
    /// A label on its own line.
    Label(String, Option<usize>),
    /// A typed machine instruction.
    Instr {
        instr: S16Instr,
        comment: Option<String>,
        ir_map: Option<usize>,
    },
    /// A structured function (optimiser can inspect prologue/body/epilogue).
    Function {
        name: String,
        ir_map: Option<usize>,
        prologue: Vec<AsmItem>,
        body: Vec<AsmItem>,
        epilogue: Vec<AsmItem>,
        frame_size: usize,
        used_callee: Vec<Register>,
        is_leaf: bool,
    },
    /// Assembler data directive, with an optional inline label.
    ///
    /// `label  data  value` (labelled) or `  data  value` (unlabelled).
    Data {
        label: Option<String>,
        value: i64,
        ir_map: Option<usize>,
    },
    /// Empty line separator.
    Blank,
}
