// A tiny, linear IR with labels, gotos, and simple assignments.

#[derive(Debug, Clone)]
pub struct ProgramIR {
    pub instrs: Vec<Instr>,
    pub source_map: SourceMap,
    /// Declared fixed-size arrays (name, length)
    pub arrays: Vec<(String, usize)>,
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
                Instr::Load { dst, addr } => {
                    out.push(format!("  {dst} = *({})", addr));
                }
                Instr::Store { addr, src } => {
                    out.push(format!("  *({}) = {}", addr, src));
                }
                Instr::FuncStart { name, params } => {
                    out.push(format!("FUNC {name}({})", params.join(", ")));
                }
                Instr::FuncEnd { name } => {
                    out.push(format!("END {name}"));
                }
                Instr::Call { func, args, ret } => {
                    if let Some(r) = ret { out.push(format!("  {r} = CALL {}({})", func, Values(args))); }
                    else { out.push(format!("  CALL {}({})", func, Values(args))); }
                }
                Instr::Return { value } => {
                    if let Some(v) = value { out.push(format!("  RETURN {}", v)); }
                    else { out.push("  RETURN".to_string()); }
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

    /// `dst = *addr`
    Load { dst: String, addr: Value },

    /// `*addr = src`
    Store { addr: Value, src: Value },

    /// `if <left> <op> <right>: GOTO <target>`
    IfCmpGoto { left: Value, op: RelOp, right: Value, target: String },

    /// `if_false <value>: GOTO <target>` (used for non-relational truthiness)
    IfFalseGoto { value: Value, target: String },

    /// `GOTO <label>`
    Goto(String),

    /// `<label>:`
    Label(String),

    /// Function prologue start
    FuncStart { name: String, params: Vec<String> },
    /// Function epilogue end
    FuncEnd { name: String },
    /// Call a function with args; optional destination for return value
    Call { func: String, args: Vec<Value>, ret: Option<String> },
    /// Return from function with optional value
    Return { value: Option<Value> },
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
    AddrOf(String),
    /// Function argument R1..R8 mapped as 1..=8
    Arg(u8),
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
            Value::Imm(i) => write!(f, "{i}"),
            Value::Var(v) => write!(f, "{v}"),
            Value::AddrOf(v) => write!(f, "&{v}"),
            Value::Arg(i) => write!(f, "Arg{}", i),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ArithOp { Add, Sub, Mul, Div }

impl std::fmt::Display for ArithOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}",
            match self { ArithOp::Add => "+", ArithOp::Sub => "-", ArithOp::Mul => "*", ArithOp::Div => "/" })
    }
}

struct Values<'a>(&'a [Value]);
impl<'a> std::fmt::Display for Values<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, v) in self.0.iter().enumerate() {
            if i > 0 { write!(f, ", ")?; }
            write!(f, "{}", v)?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RelOp { Eq, Neq, Lt, Gt, Le, Ge }

impl std::fmt::Display for RelOp {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let s = match self { RelOp::Eq => "==", RelOp::Neq => "!=", RelOp::Lt => "<", RelOp::Gt => ">", RelOp::Le => "<=", RelOp::Ge => ">=" };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, Default)]
pub struct SourceMap {
    /// IR instruction index -> list of mappings
    mappings: Vec<Vec<AstMapping>>,
    /// Cached index of source text for line/column mappings (0-based line/col)
    source_index: Option<SourceIndex>,
    /// AST node spans by id (byte start..end)
    ast_spans: Vec<(AstNodeId, usize, usize, AstNodeKind)>,
}

impl SourceMap {
    pub fn new() -> Self { Self { mappings: Vec::new(), source_index: None, ast_spans: Vec::new() } }

    pub fn add_mapping(&mut self, instr_index: usize, mapping: AstMapping) {
        if self.mappings.len() <= instr_index {
            self.mappings.resize_with(instr_index + 1, Default::default);
        }
        self.mappings[instr_index].push(mapping);
    }

    pub fn get_mappings_for_instr(&self, instr_index: usize) -> Vec<&AstMapping> {
        if instr_index >= self.mappings.len() { return Vec::new(); }
        self.mappings[instr_index].iter().collect()
    }

    pub fn get_instrs_for_ast(&self, ast_id: AstNodeId) -> Vec<usize> {
        let mut out = Vec::new();
        for (i, mm) in self.mappings.iter().enumerate() {
            if mm.iter().any(|m| m.ast_node_id == ast_id) { out.push(i); }
        }
        out
    }

    pub fn get_instrs_for_component(&self, component: ControlFlowComponent) -> Vec<usize> {
        let mut out = Vec::new();
        for (i, mm) in self.mappings.iter().enumerate() {
            if mm.iter().any(|m| m.component == Some(component)) { out.push(i); }
        }
        out
    }

    pub fn init_source_index(&mut self, source: &str) {
        self.source_index = Some(SourceIndex::new(source));
    }

    pub fn add_ast_span(&mut self, ast_node_id: AstNodeId, start: usize, end: usize, kind: AstNodeKind) {
        self.ast_spans.push((ast_node_id, start, end, kind));
    }

    pub fn get_ast_at(&self, line: usize, column: usize) -> Option<AstNodeId> {
        let idx = self.source_index.as_ref()?;
        let byte = idx.to_byte(line, column)?;
        self.ast_spans
            .iter()
            .find(|(_, s, e, _)| byte >= *s && byte < *e)
            .map(|(id, _, _, _)| *id)
    }

    pub fn get_instrs_at(&self, line: usize, column: usize) -> Vec<usize> {
        // Accept 0-based line/column from callers (TUI/editor is 0-based)
        let idx = match &self.source_index { Some(i) => i, None => return Vec::new() };
        let byte = match idx.to_byte(line, column) { Some(b) => b, None => return Vec::new() };

        // Map each IR instruction to whether any of its AST mappings cover the byte
        let mut out = Vec::new();
        for (i, mm) in self.mappings.iter().enumerate() {
            let mut hit = false;
            for m in mm {
                if let Some(info) = self.get_ast_info_by_id(m.ast_node_id) {
                    if info.span.contains(byte) { hit = true; break; }
                } else if m.source_span.contains(byte) {
                    // Fallback in case source_span was populated elsewhere
                    hit = true; break;
                }
            }
            if hit { out.push(i); }
        }
        out
    }

    pub fn get_ast_info_at(&self, line: usize, column: usize) -> Option<AstSpanInfo> {
        let idx = self.source_index.as_ref()?;
        let byte = idx.to_byte(line, column)?;
        self.get_ast_info_by_byte(byte)
    }

    pub fn get_ast_info_by_id(&self, id: AstNodeId) -> Option<AstSpanInfo> {
        self.ast_spans
            .iter()
            .find(|(aid, _, _, _)| *aid == id)
            .map(|(id, s, e, k)| AstSpanInfo { id: *id, span: SourceSpan { start: *s, end: *e }, kind: *k })
    }

    pub fn list_ast_spans_by_line(&self) -> Vec<(AstNodeId, usize, usize, AstNodeKind)> { self.ast_spans.clone() }

    /// Finalize the source map after all AST spans are registered.
    /// This retroactively fills mapping.source_span for faster lookups and consistency.
    pub fn finalize(&mut self) {
        // Build a quick lookup map from AstNodeId -> SourceSpan
        use std::collections::HashMap;
        let mut map: HashMap<AstNodeId, SourceSpan> = HashMap::new();
        for (id, s, e, _) in &self.ast_spans { map.insert(*id, SourceSpan { start: *s, end: *e }); }

        for mm in &mut self.mappings {
            for m in mm {
                if let Some(span) = map.get(&m.ast_node_id) { m.source_span = *span; }
            }
        }
    }

    fn get_ast_info_by_byte(&self, byte: usize) -> Option<AstSpanInfo> {
        // Prefer the smallest covering node that has IR mappings; fallback to smallest covering node.
        let mut best_with_ir: Option<(usize, AstSpanInfo)> = None; // (len, info)
        let mut best_any: Option<(usize, AstSpanInfo)> = None;

        for (id, s, e, k) in &self.ast_spans {
            if byte >= *s && byte < *e {
                let len = e - s;
                let info = AstSpanInfo { id: *id, span: SourceSpan { start: *s, end: *e }, kind: *k };
                let has_ir = !self.get_instrs_for_ast(*id).is_empty();
                if has_ir {
                    match &best_with_ir {
                        Some((best_len, _)) if len >= *best_len => {}
                        _ => best_with_ir = Some((len, info.clone())),
                    }
                }
                match &best_any {
                    Some((best_len, _)) if len >= *best_len => {}
                    _ => best_any = Some((len, info)),
                }
            }
        }
        best_with_ir.or(best_any).map(|(_, info)| info)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct AstNodeId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum AstNodeKind { Assign, If, While, For, Number, Variable, Binary, Unary }

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ControlFlowComponent {
    /// Condition node of if/while
    Condition,
    /// Then branch block of an if
    ThenBranch,
    /// Else branch block of an if
    ElseBranch,
    /// Loop body block of a while/for
    LoopBody,
    /// Pieces of code that glue branches/loops (labels, gotos)
    ControlFlowGlue,
}

#[derive(Debug, Clone)]
pub struct AstMapping {
    pub ast_node_id: AstNodeId,
    pub description: String,
    pub component: Option<ControlFlowComponent>,
    pub source_span: SourceSpan,
}

impl AstMapping {
    pub fn new(ast_node_id: AstNodeId, description: String) -> Self {
        Self { ast_node_id, description, component: None, source_span: SourceSpan { start: 0, end: 0 } }
    }
    pub fn with_component(mut self, component: ControlFlowComponent) -> Self {
        self.component = Some(component);
        self
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SourceSpan { pub start: usize, pub end: usize }
impl SourceSpan { pub fn len(&self) -> usize { self.end - self.start } pub fn contains(&self, pos: usize) -> bool { pos >= self.start && pos < self.end } }

#[derive(Debug, Clone)]
pub struct AstSpanInfo {
    pub id: AstNodeId,
    pub span: SourceSpan,
    pub kind: AstNodeKind,
}

#[derive(Debug, Clone)]
pub struct SourceIndex {
    line_starts: Vec<usize>,
}

impl SourceIndex {
    pub fn new(source: &str) -> Self {
        let mut starts = vec![0];
        for (i, ch) in source.char_indices() {
            if ch == '\n' { starts.push(i + 1); }
        }
        Self { line_starts: starts }
    }

    /// Convert 0-based (line, column) to byte offset.
    pub fn to_byte(&self, line: usize, column: usize) -> Option<usize> {
        let start = *self.line_starts.get(line)?;
        Some(start + column)
    }

    /// Convert byte offset to 0-based (line, column) using only line starts.
    pub fn to_line_col(&self, byte: usize) -> (usize, usize) {
        // Find the last line start <= byte
        match self.line_starts.binary_search(&byte) {
            Ok(line_idx) => (line_idx, 0),
            Err(insert_pos) => {
                let line_idx = insert_pos.saturating_sub(1);
                let start = self.line_starts.get(line_idx).copied().unwrap_or(0);
                (line_idx, byte.saturating_sub(start))
            }
        }
    }
}
