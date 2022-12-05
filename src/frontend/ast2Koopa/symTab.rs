use std::collections::HashMap;
use koopa::ir::{ Function, Value, BasicBlock };
use super::{ Error, Result };

pub struct SymbolTable {
    depth: i32,
    funcs: HashMap<String, Function>,
    vals: Vec<HashMap<String, Value>>,
    cur_func: Option<Function>,
    cur_bb: Option<BasicBlock>
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            depth: 0,
            funcs: HashMap::<String, Function>::new(),
            vals: vec![HashMap::<String, Value>::new()],
            cur_func: None,
            cur_bb: None,
        }
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

    pub fn new_func(&mut self, id: String, func: Function) -> Result<()> {
        if !self.is_global() {
            Err(Error::WrongPosition)
        } else if self.funcs.contains_key(&id) || self.vals[0].contains_key(&id) {
            Err(Error::DuplicateDefinition)
        } else {
            self.funcs.insert(id, func);
            Ok(())
        }
    }

    pub fn new_val(&mut self, id: String, val: Value) -> Result<()> {
        let is_global = self.is_global();
        let curSymTab = self.vals.last_mut().unwrap();
        if curSymTab.contains_key(&id) || (is_global && self.funcs.contains_key(&id)) {
            Err(Error::DuplicateDefinition)
        } else {
            curSymTab.insert(id, val);
            Ok(())
        }
    }

    pub fn get_in(&mut self) -> () {
        self.depth = self.depth + 1;
        self.vals.push(HashMap::<String, Value>::new());
    }

    pub fn get_out(&mut self) -> () {
        self.depth = self.depth - 1;
        self.vals.pop();
    }
}