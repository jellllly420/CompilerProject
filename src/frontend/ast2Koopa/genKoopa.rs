use koopa::ir::{ Program, Function, FunctionData, Value, Type };
use koopa::ir::values::BinaryOp;
use koopa::ir::entities::ValueKind::*;
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::builder::{ BasicBlockBuilder, LocalInstBuilder, ValueBuilder };
use crate::frontend::ast::*;
use super::symTab::{ SymbolTable, CustomValue };
use super::constEval::EvaluateConstant;
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

        let mut func_data = program.func_mut(func);
        let mut entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);
        symbol_table.set_cur_bb(entry);
        let mut end = func_data.dfg_mut().new_bb().basic_block(Some("%end".into()));
        symbol_table.set_end(end);

        match self.func_type {
            FuncType::Int => {
                let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                symbol_table.new_val("ret".to_string(), alloc);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc]);
                symbol_table.set_ret_val(alloc);
            }
        };

        self.block.generate(program, symbol_table)?;

        
        let mut func_data = program.func_mut(func);
        let jump = func_data.dfg_mut().new_value().jump(symbol_table.get_end()?.unwrap());
        func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([jump]);
        func_data.layout_mut().bbs_mut().extend([end]);
        symbol_table.set_cur_bb(end);

        match self.func_type {
            FuncType::Int => {
                let load = func_data.dfg_mut().new_value().load(symbol_table.get_ret_val()?.unwrap());
                let ret = func_data.dfg_mut().new_value().ret(Some(load));
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([load, ret]);
            }
        };

        symbol_table.get_out();
        symbol_table.reset_cur_func();
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for Block {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        /*let mut func = symbol_table.cur_func().unwrap();
        let mut func_data = program.func_mut(func);
        let mut entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
        func_data.layout_mut().bbs_mut().extend([entry]);
        symbol_table.set_cur_bb(entry);*/

        for block_item in &self.block_items {
            block_item.generate(program, symbol_table)?;
        }
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for BlockItem {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerDecl(decl) => decl.generate(program, symbol_table)?,
            Self::InnerStmt(stmt) => stmt.generate(program, symbol_table)?,
        };
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for Stmt {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::ReturnStmt(exp) => {
                let ret_val = exp.generate(program, symbol_table)?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let store = func_data.dfg_mut().new_value().store(ret_val, symbol_table.get_ret_val()?.unwrap());
                let jump = func_data.dfg_mut().new_value().jump(symbol_table.get_end()?.unwrap());
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([store, jump]);
                let mut bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                func_data.layout_mut().bbs_mut().extend([bb]);
                symbol_table.add_bb_cnt();
                symbol_table.set_cur_bb(bb);
                Ok(())
            }
            Self::AssignStmt(ident, exp) => {
                match symbol_table.get_val(&ident)? {
                    CustomValue::Value(val) => {
                        let src = exp.generate(program, symbol_table)?;
                        let func = symbol_table.cur_func().unwrap();
                        let func_data = program.func_mut(func);
                        let store = func_data.dfg_mut().new_value().store(src, val);
                        func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([store]);
                        Ok(())
                    }
                    CustomValue::ConstValue(val) => Err(Error::AssignToConst)
                }
            }
            Self::ExpStmt(exp) => {
                match exp {
                    Some(e) => { e.generate(program, symbol_table)?; }
                    None => {}
                };
                Ok(())
            }
            Self::Block(block) => {
                symbol_table.get_in();
                block.generate(program, symbol_table)?;
                symbol_table.get_out();
                Ok(())
            }
            Self::IfStmt(if_stmt) => {
                let cond = if_stmt.exp.generate(program, symbol_table)?;

                let func = symbol_table.cur_func().unwrap();
                let mut func_data = program.func_mut(func);
                let mut true_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();
                let mut false_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();
                let mut then_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();
                
                //let load = func_data.dfg_mut().new_value().load(cond);
                let branch = func_data.dfg_mut().new_value().branch(cond, true_bb, false_bb);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([branch]);

                func_data.layout_mut().bbs_mut().extend([true_bb]);
                symbol_table.set_cur_bb(true_bb);
                if_stmt.then.generate(program, symbol_table)?;
                func_data = program.func_mut(func);
                let jump = func_data.dfg_mut().new_value().jump(then_bb);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([jump]);

                func_data.layout_mut().bbs_mut().extend([false_bb]);
                symbol_table.set_cur_bb(false_bb);
                match &if_stmt.else_then {
                    Some(else_stmt) => { else_stmt.generate(program, symbol_table)?; } 
                    None => {}
                };
                func_data = program.func_mut(func);
                let jump = func_data.dfg_mut().new_value().jump(then_bb);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([jump]);

                func_data.layout_mut().bbs_mut().extend([then_bb]);
                symbol_table.set_cur_bb(then_bb);

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
            Self::InnerLVal(lval) => {
                match symbol_table.get_val(&lval)? {
                    CustomValue::Value(val) => {
                        let func = symbol_table.cur_func().unwrap();
                        let func_data = program.func_mut(func);
                        let load = func_data.dfg_mut().new_value().load(val);
                        func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([load]);
                        Ok(load)
                    }
                    CustomValue::ConstValue(val) => Ok(val)
                }
            }
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
                let mut val = lor_exp.generate(program, symbol_table)?;

                let func = symbol_table.cur_func().unwrap();
                let mut func_data = program.func_mut(func);

                let mut false_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();
                let mut then_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();

                let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                let zero = func_data.dfg_mut().new_value().integer(0);
                let mut ne = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, val);
                let mut store = func_data.dfg_mut().new_value().store(ne, alloc);
                let branch = func_data.dfg_mut().new_value().branch(ne, then_bb, false_bb);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc, ne, store, branch]);

                func_data.layout_mut().bbs_mut().extend([false_bb]);
                symbol_table.set_cur_bb(false_bb);
                val = land_exp.generate(program, symbol_table)?;
                func_data = program.func_mut(func);
                ne = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, val);
                store = func_data.dfg_mut().new_value().store(ne, alloc);
                let jump = func_data.dfg_mut().new_value().jump(then_bb);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([ne, store, jump]);

                func_data.layout_mut().bbs_mut().extend([then_bb]);
                symbol_table.set_cur_bb(then_bb);
                let load = func_data.dfg_mut().new_value().load(alloc);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([load]);

                Ok(load)
            }
            /*Self::InnerLOrExp(lor_exp, land_exp) => {
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
            }*/
        }
    }
}

impl<'a> GenerateKoopa<'a> for LAndExp {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerEqExp(eq_exp) => eq_exp.generate(program, symbol_table),
            Self::InnerLAndExp(land_exp, eq_exp) => {
                let mut val = land_exp.generate(program, symbol_table)?;

                let func = symbol_table.cur_func().unwrap();
                let mut func_data = program.func_mut(func);

                let mut true_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();
                let mut then_bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();

                let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                let zero = func_data.dfg_mut().new_value().integer(0);
                let mut ne = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, val);
                let mut store = func_data.dfg_mut().new_value().store(ne, alloc);
                let branch = func_data.dfg_mut().new_value().branch(ne, true_bb, then_bb);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc, ne, store, branch]);

                func_data.layout_mut().bbs_mut().extend([true_bb]);
                symbol_table.set_cur_bb(true_bb);
                val = eq_exp.generate(program, symbol_table)?;
                func_data = program.func_mut(func);
                ne = func_data.dfg_mut().new_value().binary(BinaryOp::NotEq, zero, val);
                store = func_data.dfg_mut().new_value().store(ne, alloc);
                let jump = func_data.dfg_mut().new_value().jump(then_bb);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([ne, store, jump]);

                func_data.layout_mut().bbs_mut().extend([then_bb]);
                symbol_table.set_cur_bb(then_bb);
                let load = func_data.dfg_mut().new_value().load(alloc);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([load]);

                Ok(load)
            }
            /*Self::InnerLAndExp(land_exp, eq_exp) => {
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
            }*/
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

impl<'a> GenerateKoopa<'a> for Decl {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::InnerConstDecl(const_decl) => const_decl.generate(program, symbol_table),
            Self::InnerVarDecl(var_decl) => var_decl.generate(program, symbol_table),
        }
    }
}

impl<'a> GenerateKoopa<'a> for ConstDecl {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        for const_def in &self.const_defs {
            const_def.generate(program, symbol_table)?;
        }
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for VarDecl {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        for var_def in &self.var_defs {
            var_def.generate(program, symbol_table)?;
        }
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for VarDef {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        let mut func_data = program.func_mut(symbol_table.cur_func().unwrap());
        match self {
            Self::InnerNoInit(ident) => {
                if symbol_table.check(&ident) {
                    return Err(Error::DuplicateDefinition);
                }
                let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                symbol_table.new_val(ident.clone(), alloc);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc]);
            }
            Self::InnerInit(ident, initval) => {          
                if symbol_table.check(&ident) {
                    return Err(Error::DuplicateDefinition);
                }
                let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                symbol_table.new_val(ident.clone(), alloc);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc]);
                let init = initval.generate(program, symbol_table)?;
                func_data = program.func_mut(symbol_table.cur_func().unwrap());
                let store = func_data.dfg_mut().new_value().store(init, alloc);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([store]);
            }
        }
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for ConstDef {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        if symbol_table.check(&self.ident) {
            return Err(Error::DuplicateDefinition);
        }
        let num = self.const_initval.generate(program, symbol_table)?;
        let func_data = program.func_mut(symbol_table.cur_func().unwrap());
        let val = func_data.dfg_mut().new_value().integer(num);
        symbol_table.new_const_val(self.ident.clone(), val);
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for InitVal {
    type Out = Value;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        self.exp.generate(program, symbol_table)
    }
}

impl<'a> GenerateKoopa<'a> for ConstInitVal {
    type Out = i32;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        fn calculate(program: &mut Program, symbol_table: &SymbolTable, inst: Value) -> i32 {
            /*
            Error when get varients
            */
            let mut func_data = program.func_mut(symbol_table.cur_func().unwrap());
            let mut dfg = func_data.dfg_mut();
            match dfg.value(inst).kind() {
                Integer(num) => num.value(),
                Binary(bin) => {
                    let binary = bin.clone();
                    let num = match binary.op() {
                        BinaryOp::Eq => (calculate(program, symbol_table, binary.lhs()) == calculate(program, symbol_table, binary.rhs())) as i32,
                        BinaryOp::NotEq => (calculate(program, symbol_table, binary.lhs()) != calculate(program, symbol_table, binary.rhs())) as i32,
                        BinaryOp::Gt => (calculate(program, symbol_table, binary.lhs()) > calculate(program, symbol_table, binary.rhs())) as i32,
                        BinaryOp::Lt => (calculate(program, symbol_table, binary.lhs()) < calculate(program, symbol_table, binary.rhs())) as i32,
                        BinaryOp::Ge => (calculate(program, symbol_table, binary.lhs()) >= calculate(program, symbol_table, binary.rhs())) as i32,
                        BinaryOp::Le => (calculate(program, symbol_table, binary.lhs()) <= calculate(program, symbol_table, binary.rhs())) as i32,
                        BinaryOp::Add => calculate(program, symbol_table, binary.lhs()) + calculate(program, symbol_table, binary.rhs()),
                        BinaryOp::Sub => calculate(program, symbol_table, binary.lhs()) - calculate(program, symbol_table, binary.rhs()),
                        BinaryOp::Mul => calculate(program, symbol_table, binary.lhs()) * calculate(program, symbol_table, binary.rhs()),
                        BinaryOp::Div => calculate(program, symbol_table, binary.lhs()) / calculate(program, symbol_table, binary.rhs()),
                        BinaryOp::Mod => calculate(program, symbol_table, binary.lhs()) % calculate(program, symbol_table, binary.rhs()),
                        BinaryOp::And => calculate(program, symbol_table, binary.lhs()) & calculate(program, symbol_table, binary.rhs()),
                        BinaryOp::Or => calculate(program, symbol_table, binary.lhs()) | calculate(program, symbol_table, binary.rhs()),
                        _ => unreachable!(),
                    };
                    func_data = program.func_mut(symbol_table.cur_func().unwrap());
                    func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().remove(&inst);
                    //dfg = func_data.dfg_mut();
                    //dfg.remove_value(inst);
                    num
                }
                _ => unreachable!(),
            }
        }

        let num = self.const_exp.generate(program, symbol_table)?;
        //let num = calculate(program, symbol_table, inst);
        //let inst = self.const_exp.generate(program, symbol_table)?;
        //let num = calculate(program, symbol_table, inst);


        Ok(num)

    }
}

impl<'a> GenerateKoopa<'a> for ConstExp {
    type Out = i32;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        Ok(self.exp.evaluate(program, symbol_table).unwrap())
        //self.exp.generate(program, symbol_table)
    }
}