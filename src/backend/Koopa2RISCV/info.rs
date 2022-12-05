use koopa::ir::entities::{ Program, Function, FunctionData, Value };
use super::{ Error, Result };
use std::collections::HashMap;

pub struct Information<'a> {
    cur_func: Option<Function>,
    cur_inst: Vec<Option<Value>>,
    register_map: HashMap<Value, usize>,
    registers: Vec<&'a str>,
}

impl<'a> Information<'a> {
    pub fn new() -> Self {
        Self{
            cur_func: None,
            cur_inst: vec![],
            register_map: HashMap::<Value, usize>::new(),
            registers: vec![ "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a1", "a2", "a3", "a4", "a5", "a6", ],
        }
    }

    pub fn cur_func(&self) -> Option<Function> {
        self.cur_func
    }

    pub fn set_cur_func(&mut self, func: Function) -> Result<()> {
        self.cur_func = Some(func);
        Ok(())
    }

    pub fn reset_cur_func(&mut self) -> Result<()> {
        self.cur_func = None;
        Ok(())
    }

    pub fn cur_inst(&self) -> Option<Value> {
        *self.cur_inst.last().unwrap()
    }

    pub fn add_cur_inst(&mut self, val: Value) -> Result<()> {
        self.cur_inst.push(Some(val));
        Ok(())
    }

    pub fn remove_cur_inst(&mut self, val: Value) -> Result<()> {
        self.cur_inst.pop();
        Ok(())
    }

    pub fn alloc_register(&mut self, val: Value) -> Result<String> {
        if self.register_map.len() == self.registers.len() {
            Err(Error::RegisterRunOut)
        } else {
            self.register_map.insert(val, self.register_map.len());
            Ok(self.registers[self.register_map.len() - 1].to_string())
        }
    }

    pub fn get_register(&self, val: Value) -> Result<String> {
        if self.register_map.contains_key(&val) {
            Ok(self.registers[self.register_map[&val]].to_string())
        } else {
            Err(Error::NoSuchInstruction)
        }
    }

    pub fn free_register(&mut self, val: Value) -> Result<()> {
        if self.register_map.contains_key(&val) {
            self.register_map.remove(&val);
            Ok(())
        } else {
            Err(Error::NoSuchInstruction)
        }
    }

    pub fn get_ret_val(&self) -> Result<String> {
        Ok(self.registers[self.register_map.len() - 1].to_string())
    }
}