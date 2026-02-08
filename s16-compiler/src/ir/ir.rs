// A tiny, linear IR with labels, gotos, and simple assignments.
use super::source_map::*;

/// `ProgramIR` represents the entire program in linear intermediate form.
#[derive(Debug, Clone)]
pub struct ProgramIR {
    /// Sequential list of instructions.
    pub instrs: Vec<Instr>,
    /// Mappings to original source code.
    pub source_map: SourceMap,
    /// Declared fixed-size arrays (name, length, optional initial values, ir_mapping).
    pub arrays: Vec<(String, usize, Option<Vec<i64>>, Option<usize>)>,
}
impl ProgramIR {
    pub fn new() -> Self {
        Self {
            instrs: Vec::new(),
            source_map: SourceMap::new(),
            arrays: Vec::new(),
        }
    }
    pub fn to_lines(&self) -> Vec<String> {
        let mut out = Vec::with_capacity(self.instrs.len() + self.arrays.len());
        for ins in &self.instrs {
            match ins {
                Instr::Label(lbl) => out.push(format!("{lbl}:")),
                Instr::Goto(lbl) => out.push(format!("  GOTO {lbl}")),
                Instr::IfCmpGoto {
                    left,
                    op,
                    right,
                    target,
                } => {
                    out.push(format!("  if {} {} {} GOTO {target}", left, op, right));
                }
                Instr::Assign { dst, src } => {
                    let prefix = match dst.kind {
                        VarKind::Local => "local ",
                        VarKind::Temp => "temp ",
                        VarKind::Global => "",
                    };
                    out.push(format!("  {prefix}{dst} = {src}"));
                }
                Instr::Load { dst, addr } => {
                    let prefix = match dst.kind {
                        VarKind::Local => "local ",
                        VarKind::Temp => "temp ",
                        VarKind::Global => "",
                    };
                    out.push(format!("  {prefix}{dst} = *({addr})"));
                }
                Instr::Store { addr, src } => {
                    out.push(format!("  *({addr}) = {src}"));
                }
                Instr::ArrayLoad { dst, base, index } => {
                    let prefix = match dst.kind {
                        VarKind::Local => "local ",
                        VarKind::Temp => "temp ",
                        VarKind::Global => "",
                    };
                    out.push(format!("  {prefix}{dst} = {base}[{index}]"));
                }
                Instr::ArrayStore { base, index, src } => {
                    out.push(format!("  {base}[{index}] = {src}"));
                }
                Instr::FuncStart { name, params } => {
                    out.push(format!("FUNC {name}({})", params.join(", ")));
                }
                Instr::FuncEnd { name } => {
                    out.push(format!("END {name}"));
                }
                Instr::Call { func, args, ret } => {
                    if let Some(r) = ret {
                        let prefix = match r.kind {
                            VarKind::Local => "local ",
                            VarKind::Temp => "temp ",
                            VarKind::Global => "",
                        };
                        out.push(format!("  {prefix}{r} = CALL {func}({})", Values(args)));
                    } else {
                        out.push(format!("  CALL {func}({})", Values(args)));
                    }
                }
                Instr::Return { value } => {
                    if let Some(v) = value {
                        out.push(format!("  RETURN {v}"));
                    } else {
                        out.push("  RETURN".to_string());
                    }
                }
                Instr::ArrayDecl {
                    name,
                    size,
                    initial_values,
                } => {
                    let init_str = if let Some(vals) = initial_values {
                        format!(
                            " = [{}]",
                            vals.iter()
                                .map(|v| v.to_string())
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                    } else {
                        "".to_string()
                    };
                    out.push(format!("  ARRAY {name}[{size}]{init_str}"));
                }
            }
        }
        out
    }
}
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VarKind {
    Global,
    Local,
    Temp,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Var {
    pub name: String,
    pub kind: VarKind,
}

impl Var {
    pub fn temp(name: String) -> Self {
        Self {
            name,
            kind: VarKind::Temp,
        }
    }
    pub fn local(name: String) -> Self {
        Self {
            name,
            kind: VarKind::Local,
        }
    }
    pub fn global(name: String) -> Self {
        Self {
            name,
            kind: VarKind::Global,
        }
    }

    pub fn is_temp(&self) -> bool {
        matches!(self.kind, VarKind::Temp)
    }

    pub fn is_local(&self) -> bool {
        matches!(self.kind, VarKind::Local)
    }

    pub fn is_global(&self) -> bool {
        matches!(self.kind, VarKind::Global)
    }

    /// Whether the variable is register-allocated (Local or Temp)
    pub fn is_reg_allocated(&self) -> bool {
        !self.is_global()
    }
}

impl std::fmt::Display for Var {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug, Clone)]
pub enum Instr {
    Assign {
        dst: Var,
        src: Rhs,
    },
    Load {
        dst: Var,
        addr: Value,
    },
    Store {
        addr: Value,
        src: Value,
    },
    ArrayLoad {
        dst: Var,
        base: String,
        index: Value,
    },
    ArrayStore {
        base: String,
        index: Value,
        src: Value,
    },
    IfCmpGoto {
        left: Value,
        op: RelOp,
        right: Value,
        target: String,
    },
    Goto(String),
    Label(String),
    FuncStart {
        name: String,
        params: Vec<String>,
    },
    FuncEnd {
        name: String,
    },
    Call {
        func: String,
        args: Vec<Value>,
        ret: Option<Var>,
    },
    Return {
        value: Option<Value>,
    },
    ArrayDecl {
        name: String,
        size: usize,
        initial_values: Option<Vec<i64>>,
    },
}
#[derive(Debug, Clone)]
pub enum Rhs {
    Value(Value),
    Binary {
        op: ArithOp,
        left: Value,
        right: Value,
    },
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
    Var(Var),
    AddrOf(String),
}
impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Imm(i) => write!(f, "{i}"),
            Value::Var(v) => write!(f, "{v}"),
            Value::AddrOf(v) => write!(f, "&{v}"),
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}
impl std::fmt::Display for ArithOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ArithOp::Add => "+",
                ArithOp::Sub => "-",
                ArithOp::Mul => "*",
                ArithOp::Div => "/",
                ArithOp::Mod => "%",
            }
        )
    }
}
struct Values<'a>(&'a [Value]);
impl<'a> std::fmt::Display for Values<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, v) in self.0.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}", v)?;
        }
        Ok(())
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelOp {
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
}
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
