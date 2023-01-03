use std::collections::HashMap;
use koopa::ir::{ Function, Value, BasicBlock };
use super::{ Error, Result };

#[derive(Copy, Clone)]
pub enum CustomValue {
    Value(Value),
    ConstValue(Value),
}

pub struct SymbolTable {
    depth: usize,
    funcs: HashMap<String, Function>,
    vals: Vec<HashMap<String, CustomValue>>,
    cur_func: Option<Function>,
    cur_bb: Option<BasicBlock>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            depth: 0,
            funcs: HashMap::<String, Function>::new(),
            vals: vec![HashMap::<String, CustomValue>::new()],
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
        } else if self.check(&id) {
            Err(Error::DuplicateDefinition)
        } else {
            self.funcs.insert(id, func);
            Ok(())
        }
    }

    pub fn new_val(&mut self, id: String, val: Value) -> Result<()> {
        let curSymTab = self.vals.last_mut().unwrap();
        curSymTab.insert(id, CustomValue::Value(val));
        Ok(())
    }


    pub fn new_const_val(&mut self, id: String, val: Value) -> Result<()> {
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

    pub fn get_const_val(&self, id: &String) -> Result<Value> {
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