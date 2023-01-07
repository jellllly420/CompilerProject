use koopa::ir::{ Program, Function, FunctionData, Value, Type };
use koopa::ir::values::BinaryOp;
use koopa::ir::entities::ValueKind::*;
use koopa::ir::dfg::DataFlowGraph;
use koopa::ir::builder::{ BasicBlockBuilder, LocalInstBuilder, ValueBuilder, GlobalInstBuilder };
use koopa::ir::types::TypeKind;
use std::cell::{ RefCell, Ref };
use crate::frontend::ast::*;
use super::symTab::{ SymbolTable, CustomValue, ConstValue };
use super::constEval::EvaluateConstant;
use super::{ Error, Result };

pub(super) trait GenerateKoopa<'a> {
    type Out;

    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out>;
    
    fn construct_type(dims: &[i32]) -> Type {
        if dims.len() == 0 {
            return Type::get_i32();
        }
        Type::get_array(Self::construct_type(&dims[0..(dims.len() - 1)]), dims[dims.len() - 1] as usize)
    }
}

impl<'a> GenerateKoopa<'a> for CompUnit {
    type Out = ();

    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        {
            let mut func = program.new_func(FunctionData::with_param_names(
                "@getint".into(),
                vec![],
                Type::get_i32(),
            ));
            symbol_table.new_func("getint".to_string(), func);
            func = program.new_func(FunctionData::with_param_names(
                "@getch".into(),
                vec![],
                Type::get_i32(),
            ));
            symbol_table.new_func("getch".to_string(), func);
            func = program.new_func(FunctionData::with_param_names(
                "@getarray".into(),
                vec![(None, Type::get_pointer(Type::get_i32()))],
                Type::get_i32(),
            ));
            symbol_table.new_func("getarray".to_string(), func);
            func = program.new_func(FunctionData::with_param_names(
                "@putint".into(),
                vec![(None, Type::get_i32())],
                Type::get_unit(),
            ));
            symbol_table.new_func("putint".to_string(), func);
            func = program.new_func(FunctionData::with_param_names(
                "@putch".into(),
                vec![(None, Type::get_i32())],
                Type::get_unit(),
            ));
            symbol_table.new_func("putch".to_string(), func);
            func = program.new_func(FunctionData::with_param_names(
                "@putarray".into(),
                vec![(None, Type::get_i32()), (None, Type::get_pointer(Type::get_i32()))],
                Type::get_unit(),
            ));
            symbol_table.new_func("putarray".to_string(), func);
            func = program.new_func(FunctionData::with_param_names(
                "@starttime".into(),
                vec![],
                Type::get_unit(),
            ));
            symbol_table.new_func("starttime".to_string(), func);
            func = program.new_func(FunctionData::with_param_names(
                "@stoptime".into(),
                vec![],
                Type::get_unit(),
            ));
            symbol_table.new_func("stoptime".to_string(), func);
        }

        for global_item in &self.global_items {
            global_item.generate(program, symbol_table)?;
        }
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for GlobalItem {
    type Out = ();
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            Self::Decl(decl) => decl.generate(program, symbol_table)?,
            Self::FuncDef(func_def) => func_def.generate(program, symbol_table)?,
        };
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for FuncDef {
    type Out = ();

    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        if symbol_table.check(&self.ident) {
            return Err(Error::DuplicateDefinition);
        }

        let fparams = {
            let mut params = vec![];
            for param in &self.func_fparams {
                //println!("{}", (*param).clone());
                match param {
                    FuncFParam::IntegerFParam(ident) => {
                        params.push((Some(format!("%{}", ident).to_string()), Type::get_i32()));
                    }
                    FuncFParam::ArrayFParam(ident, dims) => {
                        let mut _dims = vec![];
                        for dim in dims {
                            let _dim = dim.generate(program, symbol_table)?;
                            _dims.push(_dim);
                        }
                        _dims.reverse();
                        params.push((Some(format!("%{}", ident).to_string()), Type::get_pointer(Self::construct_type(&_dims))));
                    }
                }
            }
            params
        };

        let func = program.new_func(FunctionData::with_param_names(
            format!("@{}", self.ident).to_string(),
            fparams,
            match self.func_type {
                FuncType::INT => Type::get_i32(),
                FuncType::VOID => Type::get_unit(),
            },
        ));

        symbol_table.new_func(self.ident.clone(), func);
        symbol_table.set_cur_func(func);
        symbol_table.get_in();

        let mut func_data = program.func_mut(func);
        let mut entry = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
        symbol_table.add_bb_cnt();
        func_data.layout_mut().bbs_mut().extend([entry]);
        symbol_table.set_cur_bb(entry);
        let mut end = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
        symbol_table.add_bb_cnt();
        symbol_table.set_end(end);

        match self.func_type {
            FuncType::INT => {
                let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                symbol_table.new_val("ret".to_string(), alloc);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc]);
                symbol_table.set_ret_val(alloc);
            }
            FuncType::VOID => {}
        };

        for index in 0..self.func_fparams.len() {
            let src = func_data.params()[index];
            func_data = program.func_mut(func);
            let _type = (*func_data.dfg().value(src).ty()).clone();
            let alloc = func_data.dfg_mut().new_value().alloc(_type);
            let store = func_data.dfg_mut().new_value().store(src, alloc);
            func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc, store]);
            let name = &func_data.dfg_mut().value(src).name().as_ref().unwrap()[1..];
            symbol_table.new_val(name.to_string(), alloc);
            symbol_table.add_fparam(name.to_string());
        }

        self.block.generate(program, symbol_table)?;

        
        let mut func_data = program.func_mut(func);
        let jump = func_data.dfg_mut().new_value().jump(symbol_table.get_end()?.unwrap());
        func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([jump]);
        func_data.layout_mut().bbs_mut().extend([end]);
        symbol_table.set_cur_bb(end);

        match self.func_type {
            FuncType::INT => {
                let load = func_data.dfg_mut().new_value().load(symbol_table.get_ret_val()?.unwrap());
                let ret = func_data.dfg_mut().new_value().ret(Some(load));
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([load, ret]);
            }
            FuncType::VOID => {
                let ret = func_data.dfg_mut().new_value().ret(None);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([ret]);
            }
        };

        symbol_table.clear_fparams();
        symbol_table.reset_cur_func();
        symbol_table.get_out();
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
            Self::ReturnStmt(ret) => {
                match ret {
                    ReturnStmt::Int(exp) => {
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
                    ReturnStmt::VOID => {
                        let func = symbol_table.cur_func().unwrap();
                        let func_data = program.func_mut(func);
                        let jump = func_data.dfg_mut().new_value().jump(symbol_table.get_end()?.unwrap());
                        func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([jump]);
                        let mut bb = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                        func_data.layout_mut().bbs_mut().extend([bb]);
                        symbol_table.add_bb_cnt();
                        symbol_table.set_cur_bb(bb);
                        Ok(())
                    }
                }
            }
            Self::AssignStmt(lval, exp) => {
                // Type check
                match symbol_table.get_val(&lval.ident)? {
                    CustomValue::Value(val) => {
                        let src = exp.generate(program, symbol_table)?;
                        let func = symbol_table.cur_func().unwrap();
                        let mut func_data = program.func_mut(func);
                        let mut dest = val;
                        if lval.dims.len() > 0 {
                            if symbol_table.is_fparam(lval.ident.clone()) {
                                let load = func_data.dfg_mut().new_value().load(dest);
                                let index = lval.dims[0].generate(program, symbol_table)?;
                                func_data = program.func_mut(func);
                                dest = func_data.dfg_mut().new_value().get_ptr(load, index);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([load, dest]);
                            } else {
                                let index = lval.dims[0].generate(program, symbol_table)?;
                                func_data = program.func_mut(func);
                                dest = func_data.dfg_mut().new_value().get_elem_ptr(dest, index);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([dest]);
                            }
                            for dim in &lval.dims[1..] {
                                let index = dim.generate(program, symbol_table)?;
                                func_data = program.func_mut(func);
                                dest = func_data.dfg_mut().new_value().get_elem_ptr(dest, index);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([dest]);
                            }
                        } 
                        /*for dim in &lval.dims {
                            let index = dim.generate(program, symbol_table)?;
                            func_data = program.func_mut(func);
                            dest = func_data.dfg_mut().new_value().get_elem_ptr(dest, index);
                            func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([dest]);
                        }*/
                        let store = func_data.dfg_mut().new_value().store(src, dest);
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
            Self::WhileStmt(while_stmt) => {
                let func = symbol_table.cur_func().unwrap();
                let mut func_data = program.func_mut(func);
                let mut entry = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();
                let mut body = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();
                let mut next = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();

                let mut jump = func_data.dfg_mut().new_value().jump(entry);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([jump]);
                func_data.layout_mut().bbs_mut().extend([entry]);
                symbol_table.set_cur_bb(entry);

                let cond = while_stmt.exp.generate(program, symbol_table)?;
                func_data = program.func_mut(func);
                let branch = func_data.dfg_mut().new_value().branch(cond, body, next);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([branch]);
                func_data.layout_mut().bbs_mut().extend([body]);
                symbol_table.set_cur_bb(body);

                symbol_table.push_loop_entry(entry);
                symbol_table.push_loop_next(next);

                while_stmt.stmt.generate(program, symbol_table)?;

                symbol_table.pop_loop_entry();
                symbol_table.pop_loop_next();

                func_data = program.func_mut(func);
                jump = func_data.dfg_mut().new_value().jump(entry);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([jump]);
                func_data.layout_mut().bbs_mut().extend([next]);
                symbol_table.set_cur_bb(next);

                Ok(())
            }
            Self::BREAK => {
                let next = symbol_table.get_loop_next()?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let jump = func_data.dfg_mut().new_value().jump(next);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([jump]);
                let body2 = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();
                func_data.layout_mut().bbs_mut().extend([body2]);
                symbol_table.set_cur_bb(body2);

                Ok(())
            }
            Self::CONTINUE => {
                let entry = symbol_table.get_loop_entry()?;
                let func = symbol_table.cur_func().unwrap();
                let func_data = program.func_mut(func);
                let jump = func_data.dfg_mut().new_value().jump(entry);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([jump]);
                let body2 = func_data.dfg_mut().new_bb().basic_block(Some(format!("%bb_{}", symbol_table.get_bb_cnt()?.to_string()).as_str().into()));
                symbol_table.add_bb_cnt();
                func_data.layout_mut().bbs_mut().extend([body2]);
                symbol_table.set_cur_bb(body2);

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
            Self::InnerCall(ident, func_rparams) => {
                let func = symbol_table.cur_func().unwrap();
                let mut func_data = program.func_mut(func);
                let mut params = vec![];
                for func_rparam in func_rparams {
                    symbol_table.store_getting_rparam();
                    symbol_table.start_getting_rparam();
                    let param = func_rparam.generate(program, symbol_table)?;
                    symbol_table.stop_getting_rparam();
                    symbol_table.restore_getting_rparam();
                    params.push(param);
                }
                func_data = program.func_mut(func);
                let call = func_data.dfg_mut().new_value().call(symbol_table.get_func(ident)?, params);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([call]);
                Ok(call)
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
                let func = symbol_table.cur_func().unwrap();
                let mut func_data = program.func_mut(func);
                match symbol_table.get_val(&lval.ident)? {
                    CustomValue::Value(val) => {
                        let mut src = val;
                        if lval.dims.len() > 0 {
                            if symbol_table.is_fparam(lval.ident.clone()) {
                                let load = func_data.dfg_mut().new_value().load(src);
                                symbol_table.store_getting_rparam();
                                let index = lval.dims[0].generate(program, symbol_table)?;
                                symbol_table.restore_getting_rparam();
                                func_data = program.func_mut(func);
                                src = func_data.dfg_mut().new_value().get_ptr(load, index);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([load, src]);
                            } else {
                                symbol_table.store_getting_rparam();
                                let index = lval.dims[0].generate(program, symbol_table)?;
                                symbol_table.restore_getting_rparam();
                                func_data = program.func_mut(func);
                                src = func_data.dfg_mut().new_value().get_elem_ptr(src, index);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([src]);
                            }
                            for dim in &lval.dims[1..] {
                                symbol_table.store_getting_rparam();
                                let index = dim.generate(program, symbol_table)?;
                                symbol_table.restore_getting_rparam();
                                func_data = program.func_mut(func);
                                src = func_data.dfg_mut().new_value().get_elem_ptr(src, index);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([src]);
                            }
                        } /*else {
                            match func_data.dfg().value(src).ty().kind() {
                                TypeKind::Pointer(array) => {
                                    match array.kind() {
                                        TypeKind::Array(..) => {
                                            let zero = func_data.dfg_mut().new_value().integer(0);
                                            let get_elem_ptr = func_data.dfg_mut().new_value().get_elem_ptr(src, zero);
                                            func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([get_elem_ptr]);
                                            return Ok(get_elem_ptr);
                                        }
                                        _ => {}
                                    }
                                }
                                _ => {}
                            }
                        }*/
                        if symbol_table.is_getting_rparam() {
                            if src.is_global() {
                                let mut flag = false;
                                match program.borrow_value(src).ty().kind() {
                                    TypeKind::Pointer(array) => {
                                        match array.kind() {
                                            TypeKind::Array(..) => {
                                                flag = true;
                                            }
                                            _ => {}
                                        }
                                    }
                                    _ => {}
                                };
                                if flag {
                                    func_data = program.func_mut(func);
                                    let zero = func_data.dfg_mut().new_value().integer(0);
                                    let get_elem_ptr = func_data.dfg_mut().new_value().get_elem_ptr(src, zero);
                                    func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([get_elem_ptr]);
                                    return Ok(get_elem_ptr);
                                }
                            } else {
                                match func_data.dfg().value(src).ty().kind() {
                                    TypeKind::Pointer(array) => {
                                        match array.kind() {
                                            TypeKind::Array(..) => {
                                                let zero = func_data.dfg_mut().new_value().integer(0);
                                                let get_elem_ptr = func_data.dfg_mut().new_value().get_elem_ptr(src, zero);
                                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([get_elem_ptr]);
                                                return Ok(get_elem_ptr);
                                            }
                                            _ => {}
                                        }
                                    }
                                    _ => {}
                                };

                            }
                        }
                        func_data = program.func_mut(func);
                        
                        let load = func_data.dfg_mut().new_value().load(src);
                        func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([load]);
                        Ok(load)
                    }
                    CustomValue::ConstValue(val) => {
                        match val {
                            ConstValue::Integer(num) => {
                                let num = func_data.dfg_mut().new_value().integer(num);
                                Ok(num)
                            }
                            ConstValue::Array(val) => {
                                let mut src = val;
                                for dim in &lval.dims {
                                    let index = dim.generate(program, symbol_table)?;
                                    func_data = program.func_mut(func);
                                    src = func_data.dfg_mut().new_value().get_elem_ptr(src, index);
                                    func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([src]);
                                }
                                let load = func_data.dfg_mut().new_value().load(src);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([load]);
                                Ok(load)
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
            Self::Number(num) => {
                if symbol_table.is_global() {
                    let val = program.new_value().integer(*num);
                    Ok(val)
                } else {
                    let func = symbol_table.cur_func().unwrap();
                    let func_data = program.func_mut(func);
                    let val = func_data.dfg_mut().new_value().integer(*num);
                    Ok(val)
                }
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
        match self {
            Self::InnerNoInit(ident, dims) => {
                if symbol_table.check(&ident) {
                    return Err(Error::DuplicateDefinition);
                }
                if symbol_table.is_global() {
                    let init = if dims.len() == 0 { 
                        program.new_value().zero_init(Type::get_i32())
                    } else {
                        let mut _dims = vec![];
                        for dim in dims {
                            let _dim = dim.generate(program, symbol_table)?;
                            _dims.push(_dim);
                        }
                        _dims.reverse();
                        program.new_value().zero_init(Self::construct_type(&_dims))
                    };
                    let global_alloc = program.new_value().global_alloc(init);
                    program.set_value_name(global_alloc, Some(format!("@{}", ident).to_string()));
                    symbol_table.new_val(ident.clone(), global_alloc);
                } else {
                    let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                    if dims.len() == 0 {
                        let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                        symbol_table.new_val(ident.clone(), alloc);
                        func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc]);
                    } else {
                        let mut _dims = vec![];
                        for dim in dims {
                            let _dim = dim.generate(program, symbol_table)?;
                            _dims.push(_dim);
                        }
                        _dims.reverse();
                        let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                        let alloc = func_data.dfg_mut().new_value().alloc(Self::construct_type(&_dims));
                        func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc]);
                        symbol_table.new_val(ident.clone(), alloc);    
                        let val = ConstValue::Init(vec![]);
                        let initval = val.reshape(0, program, symbol_table, &_dims, _dims.len())?;
                        initval.local_init(program, symbol_table, alloc)?;
                    }
                }
            }
            Self::InnerInit(ident, dims, initval) => {          
                if symbol_table.check(&ident) {
                    return Err(Error::DuplicateDefinition);
                }
                /*if symbol_table.is_global() {
                    let num = initval.exp.evaluate(program, symbol_table).unwrap();
                    let init = program.new_value().integer(num);
                    let global_alloc = program.new_value().global_alloc(init);
                    program.set_value_name(global_alloc, Some(format!("@{}", ident).to_string()));
                    symbol_table.new_val(ident.clone(), global_alloc);
                } else {
                    let mut func_data = program.func_mut(symbol_table.cur_func().unwrap());
                    let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                    symbol_table.new_val(ident.clone(), alloc);
                    func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc]);
                    let init = initval.generate(program, symbol_table)?;
                    func_data = program.func_mut(symbol_table.cur_func().unwrap());
                    let store = func_data.dfg_mut().new_value().store(init, alloc);
                    func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([store]);
                }*/
                let val = initval.generate(program, symbol_table)?;
                if dims.len() == 0 {
                    match val {
                        ConstValue::Integer(num) => {
                            if symbol_table.is_global() {
                                let init = program.new_value().integer(num);
                                let global_alloc = program.new_value().global_alloc(init);
                                program.set_value_name(global_alloc, Some(format!("@{}", ident).to_string()));
                                symbol_table.new_val(ident.clone(), global_alloc);    
                            } else {
                                let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                                let init = func_data.dfg_mut().new_value().integer(num);
                                let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                                let store = func_data.dfg_mut().new_value().store(init, alloc);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc, store]);
                                symbol_table.new_val(ident.clone(), alloc);    
                            }
                        } 
                        ConstValue::Array(value) => {
                            if symbol_table.is_global() {
                                return Err(Error::ConstantNotEvaluated);
                            } else {
                                let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                                let alloc = func_data.dfg_mut().new_value().alloc(Type::get_i32());
                                let store = func_data.dfg_mut().new_value().store(value, alloc);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc, store]);
                                symbol_table.new_val(ident.clone(), alloc);    
                            }
                        }
                        _ => unreachable!(),
                    }
                } else {
                    match val {
                        ConstValue::Init(ref init) => {
                            let mut _dims = vec![];
                            for dim in dims {
                                let _dim = dim.generate(program, symbol_table)?;
                                _dims.push(_dim);
                            }
                            _dims.reverse();
                            let initval = val.reshape(0, program, symbol_table, &_dims, _dims.len())?;
                            if symbol_table.is_global() {
                                let _initval = initval.global_init(program, symbol_table)?;
                                let global_alloc = program.new_value().global_alloc(_initval);
                                program.set_value_name(global_alloc, Some(format!("@{}", ident).to_string()));
                                symbol_table.new_val(ident.clone(), global_alloc);
                            } else {
                                let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                                let alloc = func_data.dfg_mut().new_value().alloc(Self::construct_type(&_dims));
                                symbol_table.new_val(ident.clone(), alloc);
                                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc]);
                                initval.local_init(program, symbol_table, alloc)?;
                            }
                        }
                        _ => unreachable!(),
                    }
                }

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
        let val = self.const_initval.generate(program, symbol_table)?;
        if self.dims.len() == 0 {
            match val {
                ConstValue::Integer(num) => {
                    symbol_table.new_const_integer(self.ident.clone(), num);
                }
                _ => unreachable!(),
            }
        } else {
            match val {
                ConstValue::Init(ref init) => {
                    let mut _dims = vec![];
                    for dim in &self.dims {
                        let _dim = dim.generate(program, symbol_table)?;
                        _dims.push(_dim);
                    }
                    _dims.reverse();
                    let initval = val.reshape(0, program, symbol_table, &_dims, _dims.len())?;
                    if symbol_table.is_global() {
                        let _initval = initval.global_init(program, symbol_table)?;
                        let global_alloc = program.new_value().global_alloc(_initval);
                        program.set_value_name(global_alloc, Some(format!("@{}", &self.ident).to_string()));
                        symbol_table.new_const_array(self.ident.clone(), global_alloc);
                    } else {
                        let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                        let alloc = func_data.dfg_mut().new_value().alloc(Self::construct_type(&_dims));
                        symbol_table.new_const_array(self.ident.clone(), alloc);
                        func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([alloc]);
                        initval.local_init(program, symbol_table, alloc)?;
                    }
                }
                _ => unreachable!(),
            }
        }
        //println!("{}", num);

        /*if symbol_table.is_global() {
            let val = program.new_value().integer(num);
            symbol_table.new_const_val(self.ident.clone(), val);
        } else {
            let func_data = program.func_mut(symbol_table.cur_func().unwrap());
            let val = func_data.dfg_mut().new_value().integer(num);
            symbol_table.new_const_val(self.ident.clone(), val);
        }*/
        Ok(())
    }
}

impl<'a> GenerateKoopa<'a> for InitVal {
    type Out = ConstValue;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        match self {
            InitVal::Integer(exp) => {
                if symbol_table.is_global() {
                    let num = exp.evaluate(program, symbol_table).unwrap();
                    Ok(ConstValue::Integer(num))
                } else {
                    let num = exp.generate(program, symbol_table)?;
                    Ok(ConstValue::Array(num))
                }
            }
            InitVal::Array(initvals) => {
                let mut vals = vec![];
                for initval in initvals {
                    let val = initval.generate(program, symbol_table)?;
                    vals.push(val);
                }
                Ok(ConstValue::Init(vals))

            }
        }
    }
}

impl<'a> GenerateKoopa<'a> for ConstInitVal {
    type Out = ConstValue;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        /*fn calculate(program: &mut Program, symbol_table: &SymbolTable, inst: Value) -> i32 {
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
        }*/

        match self {
            ConstInitVal::ConstInteger(const_exp) => {
                let num = const_exp.generate(program, symbol_table)?;
                Ok(ConstValue::Integer(num))
            }
            ConstInitVal::ConstArray(const_initvals) => {
                let mut vals = vec![];
                for const_initval in const_initvals {
                    let val = const_initval.generate(program, symbol_table)?;
                    /*match val {
                        ConstValue::Integer(num) => {
                            if symbol_table.is_global() {
                                let mut final_val = program.new_value().integer(num);
                                final_val = program.new_value().aggregate(vec![final_val]);
                                vals.push(final_val);

                            } else{
                                let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                                let mut final_val = func_data.dfg_mut().new_value().integer(num);
                                final_val = func_data.dfg_mut().new_value().aggregate(vec![final_val]);
                                vals.push(final_val);
                            }
                        }
                        ConstValue::Array(val) => {
                            vals.push(val);
                        }
                    }*/
                    vals.push(val);
                }
                Ok(ConstValue::Init(vals))
            }
        }
        //let num = calculate(program, symbol_table, inst);
        //let inst = self.const_exp.generate(program, symbol_table)?;
        //let num = calculate(program, symbol_table, inst);
    }
}

impl<'a> GenerateKoopa<'a> for ConstExp {
    type Out = i32;
    
    fn generate(&'a self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Self::Out> {
        Ok(self.exp.evaluate(program, symbol_table).unwrap())
        //self.exp.generate(program, symbol_table)
    }
}