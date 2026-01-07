use crate::ir::*;
use super::IrPass;

pub struct ConstantFolder;

impl IrPass for ConstantFolder {
    fn run(&mut self, program: &mut ProgramIR) {
        for instr in &mut program.instrs {
            if let Instr::Assign { dst: _, src } = instr {
                if let Rhs::Binary { op, left: Value::Imm(l), right: Value::Imm(r) } = src {
                    let (l_val, r_val) = (*l, *r);
                    let result = match op {
                        ArithOp::Add => Some(l_val + r_val),
                        ArithOp::Sub => Some(l_val - r_val),
                        ArithOp::Mul => Some(l_val * r_val),
                        ArithOp::Div if r_val != 0 => Some(l_val / r_val),
                        _ => None,
                    };
                    if let Some(val) = result {
                        *src = Rhs::Value(Value::Imm(val));
                    }
                }
            }
        }
    }
}
