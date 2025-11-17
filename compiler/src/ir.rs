// A tiny, linear IR with labels, gotos, and simple assignments.

#[derive(Debug, Clone)]
pub struct ProgramIR {
    pub instrs: Vec<Instr>,
    pub source_map: SourceMap,
}

impl ProgramIR {
    pub fn new() -> Self { 
        Self { 
            instrs: Vec::new(),
            source_map: SourceMap::new(),
        } 
    }

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

/// Maps IR instruction indices to their corresponding AST nodes
#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    /// Maps instruction index -> AST mapping information
    mappings: Vec<(usize, AstMapping)>,
}

impl SourceMap {
    pub fn new() -> Self {
        Self {
            mappings: Vec::new(),
        }
    }

    /// Record a mapping from an IR instruction index to an AST node
    pub fn add_mapping(&mut self, instr_index: usize, mapping: AstMapping) {
        self.mappings.push((instr_index, mapping));
    }

    /// Get all mappings for a specific instruction index
    pub fn get_mappings_for_instr(&self, instr_index: usize) -> Vec<&AstMapping> {
        self.mappings
            .iter()
            .filter(|(idx, _)| *idx == instr_index)
            .map(|(_, mapping)| mapping)
            .collect()
    }

    /// Get all instruction indices that correspond to a specific AST node
    pub fn get_instrs_for_ast(&self, ast_id: AstNodeId) -> Vec<usize> {
        self.mappings
            .iter()
            .filter(|(_, mapping)| mapping.ast_node_id == ast_id)
            .map(|(idx, _)| *idx)
            .collect()
    }

    /// Get all instructions for a specific control flow component
    pub fn get_instrs_for_component(&self, component: ControlFlowComponent) -> Vec<usize> {
        self.mappings
            .iter()
            .filter(|(_, mapping)| mapping.component == Some(component))
            .map(|(idx, _)| *idx)
            .collect()
    }
}

/// Uniquely identifies an AST node
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstNodeId(pub usize);

/// Describes which part of a control flow statement an IR instruction belongs to
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ControlFlowComponent {
    /// The condition expression of an if/while statement
    Condition,
    /// The "then" branch of an if statement
    ThenBranch,
    /// The "else" branch of an if statement
    ElseBranch,
    /// The body of a while loop
    LoopBody,
    /// Control flow glue (labels, gotos for loop/branch management)
    ControlFlowGlue,
}

/// Maps an IR instruction to its source AST node and component
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AstMapping {
    /// The AST node this instruction originated from
    pub ast_node_id: AstNodeId,
    /// Which component of the AST node (for control flow)
    pub component: Option<ControlFlowComponent>,
    /// Human-readable description of what this instruction does
    pub description: String,
}

impl AstMapping {
    pub fn new(ast_node_id: AstNodeId, description: String) -> Self {
        Self {
            ast_node_id,
            component: None,
            description,
        }
    }

    pub fn with_component(mut self, component: ControlFlowComponent) -> Self {
        self.component = Some(component);
        self
    }
}
