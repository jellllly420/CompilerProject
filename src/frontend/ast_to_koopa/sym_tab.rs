use std::collections::HashMap;
use koopa::ir::{ Program, Function, Value, BasicBlock };
use koopa::ir::builder::{ ValueBuilder, LocalInstBuilder };
use super::{ Error, Result };

#[derive(Clone)]
pub enum ConstValue {
    Integer(i32),
    Array(Value),
    Init(Vec<ConstValue>),
}

#[derive(Clone)]
pub enum CustomValue {
    Value(Value),
    ConstValue(ConstValue),
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
    fparams: Vec<String>,
    getting_rparam: bool,
    getting_rparam_history: Vec<bool>,
}

impl ConstValue {
    /// all input correct
    pub fn reshape(&self, mut pos: usize, program: &Program, symbol_table: &SymbolTable, dims: &[i32], depth: usize) -> Result<Self> {
        let mut result: Vec<Vec<ConstValue>> = vec![];
        for _ in 0..depth {
            result.push(vec![]);
        }
        match self {
            Self::Init(init) => {
                for val in init {
                    match val {
                        Self::Integer(_) => {
                            pos += 1;
                            result[0].push(val.clone());
                            Self::check(&mut result, dims, depth)?;
                        }
                        Self::Array(_) => {
                            pos += 1;
                            result[0].push(val.clone());
                            Self::check(&mut result, dims, depth)?;
                        }
                        Self::Init(_) => {
                            let mut align = 1;
                            for index in 0..depth {
                                align *= dims[index] as usize;
                                if pos % align == 0 {
                                    pos += align;
                                    align = index + 1;
                                    break;
                                }
                            }
                            let new_val = val.reshape(0, program, symbol_table, dims, align)?;
                            result[align].push(new_val);
                            Self::check(&mut result, dims, depth)?;
                        }
                    }
                }
            }
            _ => unreachable!()
        }
        while result[depth - 1].len() != dims[depth - 1] as usize {
            result[0].push(Self::Integer(0));
            Self::check(&mut result, dims, depth)?;
        }
        Ok(Self::Init(result[depth - 1].clone()))

        
    }
    
    fn check(result: &mut Vec<Vec<Self>>, dims: &[i32], depth: usize) -> Result<()> {
        for index in 0..(depth - 1) {
            if result[index].len() == dims[index] as usize {
                let val = result[index].clone();
                result[index + 1].push(Self::Init(val));
                result[index].clear();
            }
        }
        Ok(())
    }


    pub fn global_init(&self, program: &mut Program, symbol_table: &mut SymbolTable) -> Result<Value> {
        match self {
            Self::Integer(integer) => {
                Ok(program.new_value().integer(*integer))
            }
            Self::Init(init) => {
                let mut temp = vec![];
                for val in init {
                    temp.push(val.global_init(program, symbol_table)?);
                }
                Ok(program.new_value().aggregate(temp))
            }
            _ => unreachable!(),
        }
    }

    pub fn local_init(&self, program: &mut Program, symbol_table: &mut SymbolTable, dest: Value) -> Result<()> {
        match self {
            Self::Integer(integer) => {
                let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                let val = func_data.dfg_mut().new_value().integer(*integer);
                let store = func_data.dfg_mut().new_value().store(val, dest);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([store]);
            }
            Self::Array(val) => {
                let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                let store = func_data.dfg_mut().new_value().store(*val, dest);
                func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([store]);
            }
            Self::Init(init) => {
                for (_index, val) in init.iter().enumerate() {
                    let func_data = program.func_mut(symbol_table.cur_func().unwrap());
                    let index = func_data.dfg_mut().new_value().integer(_index as i32);
                    let get_elem_ptr = func_data.dfg_mut().new_value().get_elem_ptr(dest, index);
                    func_data.layout_mut().bb_mut(symbol_table.cur_bb().unwrap()).insts_mut().extend([get_elem_ptr]);
                    val.local_init(program, symbol_table, get_elem_ptr)?;
                }
            }
        };
        Ok(())
    }
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
            fparams: vec![],
            getting_rparam: false,
            getting_rparam_history: vec![],
        }
    }

    pub fn is_getting_rparam(&self) -> bool {
        self.getting_rparam
    }

    pub fn start_getting_rparam(&mut self) -> Result<()> {
        self.getting_rparam = true;
        Ok(())
    }

    pub fn stop_getting_rparam(&mut self) -> Result<()> {
        self.getting_rparam = false;
        Ok(())
    }

    pub fn store_getting_rparam(&mut self) -> Result<()> {
        self.getting_rparam_history.push(self.getting_rparam);
        self.getting_rparam = false;
        Ok(())
    }

    pub fn restore_getting_rparam(&mut self) -> Result<()> {
        self.getting_rparam = self.getting_rparam_history.pop().unwrap();
        Ok(())
    }

    pub fn add_fparam(&mut self, ident: String) -> Result<()> {
        self.fparams.push(ident);
        Ok(())
    }

    pub fn clear_fparams(&mut self) -> Result<()> {
        self.fparams.clear();
        Ok(())
    }

    pub fn is_fparam(&self, ident: String) -> bool {
        self.fparams.contains(&ident)
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
        let cur_sym_tab = self.vals.last_mut().unwrap();
        cur_sym_tab.insert(id, CustomValue::Value(val));
        Ok(())
    }


    pub fn new_const_integer(&mut self, id: String, val: i32) -> Result<()> {
        let cur_sym_tab = self.vals.last_mut().unwrap();
        cur_sym_tab.insert(id, CustomValue::ConstValue(ConstValue::Integer(val)));
        Ok(())
    }

    pub fn new_const_array(&mut self, id: String, val: Value) -> Result<()> {
        let cur_sym_tab = self.vals.last_mut().unwrap();
        cur_sym_tab.insert(id, CustomValue::ConstValue(ConstValue::Array(val)));
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
            if let Some(sym_tab) = self.vals.get(depth) {
                if sym_tab.contains_key(id) {
                    return Ok(sym_tab[id].clone());
                }
            }
        }
        return Err(Error::UndefinedLVal);
    }

    pub fn get_const_val(&self, id: &String) -> Result<ConstValue> {
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
            if let Some(sym_tab) = self.vals.get(depth) {
                if sym_tab.contains_key(id) {
                    return match &sym_tab[id] {
                        CustomValue::Value(_) => Err(Error::WrongTypeValue),
                        CustomValue::ConstValue(val) => Ok(val.clone()),
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