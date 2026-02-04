use std::fmt;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Register {
    R0, R1, R2, R3, R4, R5, R6, R7,
    R8, R9, R10, R11, R12, R13, R14, R15,
}

impl Register {
    /// All general purpose registers.
    pub const GP_REGS: [Register; 12] = [
        Register::R1, Register::R2, Register::R3, Register::R4,
        Register::R5, Register::R6, Register::R7, Register::R8,
        Register::R9, Register::R10, Register::R11, Register::R12,
    ];
    
    pub const CALLER_SAVED: [Register; 8] = [
        Register::R1, Register::R2, Register::R3, Register::R4,
        Register::R5, Register::R6, Register::R7, Register::R8,
    ];

    pub const CALLEE_SAVED: [Register; 4] = [
        Register::R9, Register::R10, Register::R11, Register::R12,
    ];

    pub const PARAM_REGS: [Register; 8] = [
        Register::R1, Register::R2, Register::R3, Register::R4,
        Register::R5, Register::R6, Register::R7, Register::R8,
    ];

    pub const STACK_PTR: Register = Register::R14;
    pub const LINK_REG: Register = Register::R13;
    pub const ZERO_REG: Register = Register::R0;

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "R0" => Some(Register::R0),
            "R1" => Some(Register::R1),
            "R2" => Some(Register::R2),
            "R3" => Some(Register::R3),
            "R4" => Some(Register::R4),
            "R5" => Some(Register::R5),
            "R6" => Some(Register::R6),
            "R7" => Some(Register::R7),
            "R8" => Some(Register::R8),
            "R9" => Some(Register::R9),
            "R10" => Some(Register::R10),
            "R11" => Some(Register::R11),
            "R12" => Some(Register::R12),
            "R13" => Some(Register::R13),
            "R14" => Some(Register::R14),
            "R15" => Some(Register::R15),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Register::R0 => "R0",
            Register::R1 => "R1",
            Register::R2 => "R2",
            Register::R3 => "R3",
            Register::R4 => "R4",
            Register::R5 => "R5",
            Register::R6 => "R6",
            Register::R7 => "R7",
            Register::R8 => "R8",
            Register::R9 => "R9",
            Register::R10 => "R10",
            Register::R11 => "R11",
            Register::R12 => "R12",
            Register::R13 => "R13",
            Register::R14 => "R14",
            Register::R15 => "R15",
        }
    }
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.as_str())
    }
}
