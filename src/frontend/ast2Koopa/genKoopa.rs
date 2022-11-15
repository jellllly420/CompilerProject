use koopa::ir::{ Program, Function, FunctionData, Value, Type };
use koopa::ir::builder::{ BasicBlockBuilder, LocalInstBuilder, ValueBuilder };
use crate::frontend::ast::*;
use super::symTab::SymbolTable;
use super::{ Error, Result };

pub(super) trait GenerateKoopa<'a> {
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable<'a>) -> Result<()>;
}

impl<'a> GenerateKoopa<'a> for CompUnit {
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable<'a>) -> Result<()> {
        self.func_def.generate(program, symbol_table)?;
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for FuncDef {
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable<'a>) -> Result<()> {
        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident).to_string(),
            vec![],
            match self.func_type {
                FuncType::Int => Type::get_i32()
            },
        ));

        symbol_table.new_func(&self.ident, func);
        symbol_table.set_cur_func(func);
        symbol_table.get_in();
        self.block.generate(program, symbol_table)?;
        symbol_table.get_out();
        symbol_table.reset_cur_func();
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for Block {
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable<'a>) -> Result<()> {
        self.stmt.generate(program, symbol_table)?;
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for Stmt {
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable<'a>) -> Result<()> {
        let func = symbol_table.cur_func().unwrap();
        let func_data = program.func_mut(func);
        match self {
            Stmt::ReturnStmt { retval } => {
                let entry = func_data.dfg_mut().new_bb().basic_block(Some("%entry".into()));
                func_data.layout_mut().bbs_mut().extend([entry]);
                let ret_val = func_data.dfg_mut().new_value().integer(*retval);
                let ret = func_data.dfg_mut().new_value().ret(Some(ret_val));
                func_data.layout_mut().bb_mut(entry).insts_mut().extend([ret]);
                Ok(())
            }
            _ => Err(Error::UnknownStatementType)
        }
    }
}