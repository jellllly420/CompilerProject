use koopa::ir::entities::{ Function, Value };
use super::{ Error, Result };
use std::collections::HashMap;

pub struct Information {
    cur_func: Option<Function>,
    cur_inst: Vec<Option<Value>>,
    //register_map: HashMap<Value, usize>,
    memory_map: HashMap<Value, i32>,
    //registers: Vec<&'a str>,
    offset: i32,
    stack_length: i32,
    alloc_slot: bool,
}

impl Information {
    pub fn new() -> Self {
        Self{
            cur_func: None,
            cur_inst: vec![],
            //register_map: HashMap::<Value, usize>::new(),
            memory_map: HashMap::<Value, i32>::new(),
            //registers: vec![ "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a1", "a2", "a3", "a4", "a5", "a6", ],
            offset: 0,
            stack_length: 0,
            alloc_slot: true,
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

    pub fn remove_cur_inst(&mut self) -> Result<()> {
        self.cur_inst.pop();
        Ok(())
    }

    /*pub fn alloc_register(&mut self, val: Value) -> Result<String> {
        if self.register_map.len() == self.registers.len() {
            Err(Error::RegisterRunOut)
        } else {
            self.register_map.insert(val, self.register_map.len());
            Ok(self.registers[self.register_map.len() - 1].to_string())
        }
    }*/

    pub fn alloc_register(&mut self) -> Result<String> {
        self.alloc_slot = !self.alloc_slot;
        if self.alloc_slot {
            Ok("t1".to_string())
        } else {
            Ok("t0".to_string())
        }
    }

    pub fn alloc_memory(&mut self, val: Value, len: i32) -> Result<i32> {
        self.memory_map.insert(val, self.offset);
        self.offset += len;
        Ok(self.offset - len)
    }

    /*pub fn get_register(&self, val: Value) -> Result<String> {
        if self.register_map.contains_key(&val) {
            Ok(self.registers[self.register_map[&val]].to_string())
        } else {
            Err(Error::NoSuchInstruction)
        }
    }*/

    pub fn get_memory(&self, val: Value) -> Result<i32> {
        if self.memory_map.contains_key(&val) {
            Ok(self.memory_map[&val])
        } else {
            Err(Error::NoSuchInstruction)
        }
    }

    /*pub fn free_register(&mut self, val: Value) -> Result<()> {
        if self.register_map.contains_key(&val) {
            self.register_map.remove(&val);
            Ok(())
        } else {
            Err(Error::NoSuchInstruction)
        }
    }*/

    pub fn free_register(&mut self) -> Result<()> {
        self.alloc_slot = true;
        Ok(())
    }

    /*pub fn get_ret_val(&self) -> Result<String> {
        Ok(self.registers[self.register_map.len() - 1].to_string())
    }*/

    pub fn set_stack_length(&mut self, stk_length: i32) -> Result<()> {
        self.stack_length = stk_length;
        Ok(())
    }

    pub fn get_stack_length(&self) -> Result<i32> {
        Ok(self.stack_length)
    }

    pub fn set_offset(&mut self, offset: i32) -> Result<()> {
        self.offset = offset;
        Ok(())
    }
}