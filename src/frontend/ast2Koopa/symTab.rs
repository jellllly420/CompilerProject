use std::collections::HashMap;
use koopa::ir::{ Function, Value, BasicBlock };
use super::{ Error, Result };

#[derive(Copy, Clone)]
pub enum CustomValue {
    Value(Value),
    ConstValue(i32),
}

pub struct SymbolTable {
    depth: usize,
    funcs: HashMap<String, Function>,
    vals: Vec<HashMap<String, CustomValue>>,
    cur_func: Option<Function>,
    cur_bb: Option<BasicBlock>,
    block_cnt: i32,
    ret_val: Option<Value>,
    end: Option<BasicBlock>,
    loop_entry: Vec<BasicBlock>,
    loop_next: Vec<BasicBlock>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            depth: 0,
            funcs: HashMap::<String, Function>::new(),
            vals: vec![HashMap::<String, CustomValue>::new()],
            cur_func: None,
            cur_bb: None,
            block_cnt: 0,
            ret_val: None,
            end: None,
            loop_entry: vec![],
            loop_next: vec![],
        }
    }

    pub fn push_loop_entry(&mut self, entry: BasicBlock) -> Result<()> {
        self.loop_entry.push(entry);
        Ok(())
    }

    pub fn push_loop_next(&mut self, next: BasicBlock) -> Result<()> {
        self.loop_next.push(next);
        Ok(())
    }

    pub fn pop_loop_entry(&mut self) -> Result<()> {
        self.loop_entry.pop();
        Ok(())
    }

    pub fn pop_loop_next(&mut self) -> Result<()> {
        self.loop_next.pop();
        Ok(())
    }

    pub fn get_loop_entry(&self) -> Result<BasicBlock> {
        if self.loop_entry.len() == 0 {
            return Err(Error::NoLoopWrapped);
        }
        Ok(*self.loop_entry.last().unwrap())
    }

    pub fn get_loop_next(&self) -> Result<BasicBlock> {
        if self.loop_next.len() == 0 {
            return Err(Error::NoLoopWrapped);
        }
        Ok(*self.loop_next.last().unwrap())
    }

    pub fn set_ret_val(&mut self, ret_val: Value) -> Result<()> {
        self.ret_val = Some(ret_val);
        Ok(())
    }

    pub fn get_ret_val(&self) -> Result<Option<Value>> {
        Ok(self.ret_val)
    }

    pub fn set_end(&mut self, end: BasicBlock) -> Result<()> {
        self.end = Some(end);
        Ok(())
    }

    pub fn get_end(&self) -> Result<Option<BasicBlock>> {
        Ok(self.end)
    }

    pub fn is_global(&self) -> bool {
        self.depth == 0
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

    pub fn cur_bb(&self) -> Option<BasicBlock> {
        self.cur_bb
    }

    pub fn set_cur_bb(&mut self, bb: BasicBlock) -> Result<()> {
        self.cur_bb = Some(bb);
        Ok(())
    } 

    pub fn add_bb_cnt(&mut self) -> Result<()> {
        self.block_cnt = self.block_cnt + 1;
        Ok(())
    }

    pub fn get_bb_cnt(&self) -> Result<i32> {
        Ok(self.block_cnt)
    }

    pub fn new_func(&mut self, id: String, func: Function) -> Result<()> {
        if !self.is_global() {
            Err(Error::WrongPosition)
        } else if self.check(&id) {
            Err(Error::DuplicateDefinition)
        } else {
            self.funcs.insert(id, func);
            Ok(())
        }
    }

    pub fn get_func(&self, id: &String) -> Result<Function> {
        if self.funcs.contains_key(id) {
            Ok(self.funcs[id])
        } else {
            Err(Error::UndefinedFunction)
        }
    }

    pub fn new_val(&mut self, id: String, val: Value) -> Result<()> {
        let curSymTab = self.vals.last_mut().unwrap();
        curSymTab.insert(id, CustomValue::Value(val));
        Ok(())
    }


    pub fn new_const_val(&mut self, id: String, val: i32) -> Result<()> {
        let curSymTab = self.vals.last_mut().unwrap();
        curSymTab.insert(id, CustomValue::ConstValue(val));
        Ok(())
    }

    pub fn get_val(&self, id: &String) -> Result<CustomValue> {
        /*let curSymTab = self.vals.last().unwrap();
        if curSymTab.contains_key(id) {
            Ok(curSymTab[id])
        } else {
            Err(Error::UndefinedLVal)
        }*/
        for depth in (0..=self.depth).rev() {
            if let Some(symTab) = self.vals.get(depth) {
                if symTab.contains_key(id) {
                    return Ok(symTab[id]);
                }
            }
        }
        return Err(Error::UndefinedLVal);
    }

    pub fn get_const_val(&self, id: &String) -> Result<i32> {
        /*let curSymTab = self.vals.last().unwrap();
        if curSymTab.contains_key(id) {
            match curSymTab[id] {
                CustomValue::Value(val) => Err(Error::WrongTypeValue),
                CustomValue::ConstValue(val) => Ok(val),
            }
        } else {
            Err(Error::UndefinedLVal)
        }*/
        for depth in (0..=self.depth).rev() {
            if let Some(symTab) = self.vals.get(depth) {
                if symTab.contains_key(id) {
                    return match symTab[id] {
                        CustomValue::Value(val) => Err(Error::WrongTypeValue),
                        CustomValue::ConstValue(val) => Ok(val),
                    };
                }
            }
        }
        Err(Error::UndefinedLVal)
    }

    pub fn check(&self, id: &String) -> bool {
        //self.get_val(id).is_ok()
        self.vals.last().unwrap().contains_key(id) || (self.is_global() && self.funcs.contains_key(id))
    }

    pub fn get_in(&mut self) -> () {
        self.depth = self.depth + 1;
        self.vals.push(HashMap::<String, CustomValue>::new());
    }

    pub fn get_out(&mut self) -> () {
        self.depth = self.depth - 1;
        self.vals.pop();
    }
}