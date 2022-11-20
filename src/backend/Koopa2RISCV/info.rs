use koopa::ir::entities::{ Program, Function, FunctionData };
use super::{ Error, Result };

pub struct Information<'a> {
    program: &'a Program,
    cur_func: Option<Function>,
}

impl<'a> Information<'a> {
    pub fn new(program: &'a Program) -> Self {
        Self{
            program: program,
            cur_func: None,
        }
    }

    pub fn program(&self) -> &Program {
        self.program
    }

    pub fn cur_func(&self) -> Option<Function> {
        self.cur_func
    }

    pub fn cur_func_data(&self) -> &FunctionData {
        self.program().func(self.cur_func().unwrap())
    }

    pub fn set_cur_func(&mut self, func: Function) -> Result<()> {
        self.cur_func = Some(func);
        Ok(())
    }

    pub fn reset_cur_func(&mut self) -> Result<()> {
        self.cur_func = None;
        Ok(())
    }
}