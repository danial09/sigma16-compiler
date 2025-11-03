// A tiny, linear IR with labels, gotos, and simple assignments.

#[derive(Debug, Clone)]
pub struct ProgramIR {
    pub instrs: Vec<Instr>,
}

impl ProgramIR {
    pub fn new() -> Self { Self { instrs: Vec::new() } }

    pub fn to_lines(&self) -> Vec<String> {
        let mut out = Vec::with_capacity(self.instrs.len());
        for ins in &self.instrs {
            match ins {
                Instr::Label(lbl) => out.push(format!("{lbl}:")),
                Instr::Goto(lbl) => out.push(format!("  GOTO {lbl}")),
                Instr::IfCmpGoto { left, op, right, target } => {
                    out.push(format!("  if {} {} {}: GOTO {target}", left, op, right));
                }
                Instr::IfFalseGoto { value, target } => {
                    out.push(format!("  if_false {} GOTO {target}", value));
                }
                Instr::Assign { dst, src } => {
                    out.push(format!("  {dst} = {src}"));
                }
            }
        }
        out
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    /// `dst = src`
    Assign { dst: String, src: Rhs },

    /// `if <left> <op> <right>: GOTO <target>`
    IfCmpGoto { left: Value, op: RelOp, right: Value, target: String },

    /// `if_false <value>: GOTO <target>` (used for non-relational truthiness)
    IfFalseGoto { value: Value, target: String },

    /// `GOTO <label>`
    Goto(String),

    /// `<label>:`
    Label(String),
}

#[derive(Debug, Clone)]
pub enum Rhs {
    /// A plain value (number or variable)
    Value(Value),

    /// `left <op> right` (arithmetic only)
    Binary { op: ArithOp, left: Value, right: Value },
}

impl std::fmt::Display for Rhs {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Rhs::Value(v) => write!(f, "{v}"),
            Rhs::Binary { op, left, right } => write!(f, "{} {} {}", left, op, right),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Value {
    Imm(i64),
    Var(String),
}

impl From<&str> for Value {
    fn from(s: &str) -> Self { Value::Var(s.to_string()) }
}
impl From<String> for Value {
    fn from(s: String) -> Self { Value::Var(s) }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Imm(n) => write!(f, "{n}"),
            Value::Var(v) => write!(f, "{v}"),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ArithOp { Add, Sub, Mul, Div }

impl std::fmt::Display for ArithOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self { ArithOp::Add => "+", ArithOp::Sub => "-", ArithOp::Mul => "*", ArithOp::Div => "/" };
        write!(f, "{s}")
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RelOp { Eq, Neq, Lt, Gt, Le, Ge }

impl std::fmt::Display for RelOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self {
            RelOp::Eq => "==",
            RelOp::Neq => "!=",
            RelOp::Lt => "<",
            RelOp::Gt => ">",
            RelOp::Le => "<=",
            RelOp::Ge => ">=",
        };
        write!(f, "{s}")
    }
}
