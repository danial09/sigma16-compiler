use super::super::abi::Register;

#[derive(Debug, Clone)]
pub enum AsmItem {
    Label(String, Option<usize>),
    Instruction {
        text: String,
        ir_map: Option<usize>,
    },
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
}

impl AsmItem {
    pub fn as_label(&self) -> Option<&str> {
        match self {
            AsmItem::Label(s, _) => Some(s),
            _ => None,
        }
    }
}
