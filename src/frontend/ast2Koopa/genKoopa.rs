use koopa::ir::{ Program, Function, FunctionData, Value, Type };
use koopa::ir::values::BinaryOp;
use koopa::ir::builder::{ BasicBlockBuilder, LocalInstBuilder, ValueBuilder };
use crate::frontend::ast::*;
use super::symTab::SymbolTable;
use super::{ Error, Result };

pub(super) trait GenerateKoopa<'a> {
    type Out;

    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out>;

}

impl<'a> GenerateKoopa<'a> for CompUnit {
    type Out = ();

    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        self.func_def.generate(program, symbol_table)?;
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for FuncDef {
    type Out = ();

    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident).to_string(),
            vec![],
            match self.func_type {
                FuncType::Int => Type::get_i32()
            },
        ));

        symbol_table.new_func(self.ident.clone(), func);
        symbol_table.set_cur_func(func);
        symbol_table.get_in();
        self.block.generate(program, symbol_table)?;
        symbol_table.get_out();
        symbol_table.reset_cur_func();
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for Block {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        self.stmt.generate(program, symbol_table)?;
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for Stmt {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::ReturnStmt(exp) => {
                let mut func = symbol_table.cur_func().unwrap();
                let mut func_data = program.func_mut(func);
                let mut entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
                func_data.layout_mut().bbs_mut().extend([entry]);
                symbol_table.set_cur_bb(entry);

                let ret_val = exp.generate(program, symbol_table)?;

                func = symbol_table.cur_func().unwrap();
                func_data = program.func_mut(func);
                entry = *func_data.layout_mut().bbs().back_key().unwrap();
                let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
                func_data.layout_mut().bb_mut(entry).insts_mut().extend([ret]);
                Ok(())
            }
            _ => Err(Error::UnknownStatementType)
        }
    }
}

impl<'a> GenerateKoopa<'a> for Exp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerLOrExp(lor_exp) => lor_exp.generate(program, symbol_table),
        }
    }
}

impl<'a> GenerateKoopa<'a> for UnaryExp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerPrimaryExp(primary_exp) => primary_exp.generate(program, symbol_table),
            Self::InnerUnaryExp(unary_op, unary_exp) => {
                let val = unary_exp.generate(program, symbol_table)?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let zero = func_data.dfg_mut().new_value().integer(0);
                let inst = match unary_op {
                    UnaryOp::POSITIVE => func_data.dfg_mut().new_value().binary(BinaryOp::Add, zero, val),
                    UnaryOp::NEGATIVE => func_data.dfg_mut().new_value().binary(BinaryOp::Sub, zero, val),
                    UnaryOp::NOT => func_data.dfg_mut().new_value().binary(BinaryOp::Eq, zero, val),
                };
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([inst]);
                Ok(inst)
            }
        }
    }
}

impl<'a> GenerateKoopa<'a> for PrimaryExp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerExp(exp) => exp.generate(program, symbol_table),
            Self::Number(num) => {
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let val = func_data.dfg_mut().new_value().integer(*num);
                Ok(val)
            }
        }
    }
}

impl<'a> GenerateKoopa<'a> for AddExp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerMulExp(mul_exp) => mul_exp.generate(program, symbol_table),
            Self::InnerAddExp(add_exp, add_op, mul_exp) => {
                let val1 = add_exp.generate(program, symbol_table)?;
                let val2 = mul_exp.generate(program, symbol_table)?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let inst = match add_op {
                    AddOp::ADD => func_data.dfg_mut().new_value().binary(BinaryOp::Add, val1, val2),
                    AddOp::SUBTRACT => func_data.dfg_mut().new_value().binary(BinaryOp::Sub, val1, val2),
                };
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([inst]);
                Ok(inst)
            }
        }
    }
}

impl<'a> GenerateKoopa<'a> for MulExp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerUnaryExp(unary_exp) => unary_exp.generate(program, symbol_table),
            Self::InnerMulExp(mul_exp, mul_op, unary_exp) => {
                let val1 = mul_exp.generate(program, symbol_table)?;
                let val2 = unary_exp.generate(program, symbol_table)?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let inst = match mul_op {
                    MulOp::MULTIPLY => func_data.dfg_mut().new_value().binary(BinaryOp::Mul, val1, val2),
                    MulOp::DIVIDE => func_data.dfg_mut().new_value().binary(BinaryOp::Div, val1, val2),
                    MulOp::MOD => func_data.dfg_mut().new_value().binary(BinaryOp::Mod, val1, val2),
                };
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([inst]);
                Ok(inst)
            }
        }
    }
}

impl<'a> GenerateKoopa<'a> for LOrExp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerLAndExp(land_exp) => land_exp.generate(program, symbol_table),
            Self::InnerLOrExp(lor_exp, land_exp) => {
                let val1 = lor_exp.generate(program, symbol_table)?;
                let val2 = land_exp.generate(program, symbol_table)?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let zero = func_data.dfg_mut().new_value().integer(0);
                let inst1 = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, val1);
                let inst2 = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, val2);
                let inst = func_data.dfg_mut().new_value().binary(BinaryOp::Or, inst1, inst2);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([inst1, inst2, inst]);
                Ok(inst)
            }
        }
    }
}

impl<'a> GenerateKoopa<'a> for LAndExp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerEqExp(eq_exp) => eq_exp.generate(program, symbol_table),
            Self::InnerLAndExp(land_exp, eq_exp) => {
                let val1 = land_exp.generate(program, symbol_table)?;
                let val2 = eq_exp.generate(program, symbol_table)?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let zero = func_data.dfg_mut().new_value().integer(0);
                let inst1 = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, val1);
                let inst2 = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, val2);
                let inst = func_data.dfg_mut().new_value().binary(BinaryOp::And, inst1, inst2);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([inst1, inst2, inst]);
                Ok(inst)
            }
        }
    }
}

impl<'a> GenerateKoopa<'a> for EqExp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerRelExp(rel_exp) => rel_exp.generate(program, symbol_table),
            Self::InnerEqExp(eq_exp, eq_op, rel_exp) => {
                let val1 = eq_exp.generate(program, symbol_table)?;
                let val2 = rel_exp.generate(program, symbol_table)?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let inst = match eq_op {
                    EqOp::EQUAL => func_data.dfg_mut().new_value().binary(BinaryOp::Eq, val1, val2),
                    EqOp::NOTEQUAL => func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, val1, val2),
                };
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([inst]);
                Ok(inst)
            }
        }
    }
}

impl<'a> GenerateKoopa<'a> for RelExp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerAddExp(add_exp) => add_exp.generate(program, symbol_table),
            Self::InnerRelExp(rel_exp, rel_op, add_exp) => {
                let val1 = rel_exp.generate(program, symbol_table)?;
                let val2 = add_exp.generate(program, symbol_table)?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let inst = match rel_op {
                    RelOp::LESSTHAN => func_data.dfg_mut().new_value().binary(BinaryOp::Lt, val1, val2),
                    RelOp::GREATERTHAN => func_data.dfg_mut().new_value().binary(BinaryOp::Gt, val1, val2),
                    RelOp::LESSTHANOREQUAL => func_data.dfg_mut().new_value().binary(BinaryOp::Le, val1, val2),
                    RelOp::GREATERTHANOREQUAL => func_data.dfg_mut().new_value().binary(BinaryOp::Ge, val1, val2),
                };
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([inst]);
                Ok(inst)
            }
        }
    }
}