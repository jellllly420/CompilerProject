use koopa::ir::{ Program, Function, FunctionData, Value, Type };
use koopa::ir::entities::ValueKind::*;
use super::symTab::SymbolTable;
use crate::frontend::ast::*;

pub trait EvaluateConstant {
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32>;
}

impl EvaluateConstant for Exp {
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32> {
        match self {
            Self::InnerLOrExp(lor_exp) => lor_exp.evaluate(program, symbol_table)
        }
    }
}

impl EvaluateConstant for LOrExp {
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32> {
        match self {
            Self::InnerLAndExp(land_exp) => land_exp.evaluate(program, symbol_table),
            Self::InnerLOrExp(lor_exp, land_exp) => {
                match (lor_exp.evaluate(program, symbol_table), land_exp.evaluate(program, symbol_table)) {
                    (Some(lhs), Some(rhs)) => Some((lhs != 0 || rhs != 0) as i32),
                    _ => None,
                }
            }
        }
    }
}

impl EvaluateConstant for LAndExp{
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32> {
        match self {
            Self::InnerEqExp(eq_exp) => eq_exp.evaluate(program, symbol_table),
            Self::InnerLAndExp(land_exp, eq_exp) => {
                match (land_exp.evaluate(program, symbol_table), eq_exp.evaluate(program, symbol_table)) {
                    (Some(lhs), Some(rhs)) => Some((lhs != 0 && rhs != 0) as i32),
                    _ => None,
                }
            }
        }
    }
}

impl EvaluateConstant for EqExp {
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32> {
        match self {
            Self::InnerRelExp(rel_exp) => rel_exp.evaluate(program, symbol_table),
            Self::InnerEqExp(eq_exp, eq_op, rel_exp) => {
                match (eq_exp.evaluate(program, symbol_table), rel_exp.evaluate(program, symbol_table)) {
                    (Some(lhs), Some(rhs)) => {
                        match eq_op {
                            EqOp::EQUAL => Some((lhs == rhs) as i32),
                            EqOp::NOTEQUAL => Some((lhs != rhs) as i32),
                        }
                    }
                    _ => None,
                }
            }
        }
    }
}

impl EvaluateConstant for RelExp {
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32> {
        match self {
            Self::InnerAddExp(add_exp) => add_exp.evaluate(program, symbol_table),
            Self::InnerRelExp(rel_exp, rel_op, add_exp) => {
                match (rel_exp.evaluate(program, symbol_table), add_exp.evaluate(program, symbol_table)) {
                    (Some(lhs), Some(rhs)) => {
                        match rel_op {
                            RelOp::LESSTHAN => Some((lhs < rhs) as i32),
                            RelOp::GREATERTHAN => Some((lhs > rhs) as i32),
                            RelOp::LESSTHANOREQUAL => Some((lhs <= rhs) as i32),
                            RelOp::GREATERTHANOREQUAL => Some((lhs >= rhs) as i32),
                        }
                    }
                    _ => None,
                }
            }
        }
    }
}

impl EvaluateConstant for AddExp {
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32> {
        match self {
            Self::InnerMulExp(mul_exp) => mul_exp.evaluate(program, symbol_table),
            Self::InnerAddExp(add_exp, add_op, mul_exp) => {
                match (add_exp.evaluate(program, symbol_table), mul_exp.evaluate(program, symbol_table)) {
                    (Some(lhs), Some(rhs)) => {
                        match add_op {
                            AddOp::ADD => Some(lhs + rhs),
                            AddOp::SUBTRACT => Some(lhs - rhs),
                        }
                    }
                    _ => None,
                }
            }
        }
    }
}

impl EvaluateConstant for MulExp {
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32> {
        match self {
            Self::InnerUnaryExp(unary_exp) => unary_exp.evaluate(program, symbol_table),
            Self::InnerMulExp(mul_exp, mul_op, unary_exp) => {
                match (mul_exp.evaluate(program, symbol_table), unary_exp.evaluate(program, symbol_table)) {
                    (Some(lhs), Some(rhs)) => {
                        match mul_op {
                            MulOp::MULTIPLY => Some(lhs * rhs),
                            MulOp::DIVIDE => Some(lhs / rhs),
                            MulOp::MOD => Some(lhs % rhs),
                        }
                    }
                    _ => None,
                }
            }
        }
    }
}

impl EvaluateConstant for UnaryExp {
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32> {
        match self {
            Self::InnerPrimaryExp(primary_exp) => primary_exp.evaluate(program, symbol_table),
            Self::InnerUnaryExp(unary_op, unary_exp) => {
                match (unary_exp.evaluate(program, symbol_table)) {
                    (Some(rhs)) => {
                        match unary_op {
                            UnaryOp::POSITIVE => Some(0 + rhs),
                            UnaryOp::NEGATIVE => Some(0 - rhs),
                            UnaryOp::NOT => Some((0 == rhs) as i32),
                        }
                    }
                    _ => None,
                }
            }
        }
    }
}

impl EvaluateConstant for PrimaryExp {
    fn evaluate(&self, program: &Program, symbol_table: &SymbolTable) -> Option<i32> {
        match self {
            Self::InnerExp(exp) => exp.evaluate(program, symbol_table),
            Self::InnerLVal(lval) => {
                let val = symbol_table.get_const_val(lval);
                if !val.is_ok() {
                    return None;
                }
                let dfg = program.func(symbol_table.cur_func().unwrap()).dfg();
                let val_data = dfg.value(val.unwrap());
                match val_data.kind() {
                    Integer(integer) => Some(integer.value()),
                    _ => unreachable!(),
                }
            }
            Self::Number(num) => Some(*num),
        }
    }
}