use super::{ Error, Result };
use super::info::Information;
use koopa::ir::entities::{ Program, FunctionData, ValueData, ValueKind, Value };
use koopa::ir::types::{ TypeKind, Type };
use koopa::ir::values::*;
use std::cmp::max;

pub(super) trait GenerateRISCV {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()>;
}

impl GenerateRISCV for Program {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {   
        for &val in self.inst_layout() {
            let val_data = self.borrow_value(val);
            // generate for global value
            match val_data.kind() {
                ValueKind::GlobalAlloc(global_alloc) => {
                    info.add_cur_inst(val);
                    global_alloc.generate(RISCV, info, program);
                    info.remove_cur_inst();
                }
                _ => {}
            }
        };
        for &func in self.func_layout() {
            let func_data = self.func(func);
            info.set_cur_func(func);
            func_data.generate(RISCV, info, program)?;
            info.reset_cur_func();
        }
        Ok(())
    }
}

impl GenerateRISCV for FunctionData {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        if self.layout().entry_bb().is_none() {
            return Ok(());
        }
        RISCV.push_str("  .text\n");
        let name = &self.name().to_string()[1..];
        RISCV.push_str(format!("  .global {}\n", name).as_str());
        RISCV.push_str(format!("{}:\n", name).as_str());
        let mut byte = 0;
        let dfg = self.dfg();
        let mut flag = false;
        let mut arg_cnt = 0;
        for (&bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                match dfg.value(inst).kind() {
                    ValueKind::Call(call) => {
                        flag = true;
                        if call.args().len() > 8 {
                            arg_cnt = max(call.args().len() - 8, arg_cnt);
                        }
                    }
                    _ => {}
                }
                if dfg.value(inst).ty().is_unit() {
                    continue;
                }
                match dfg.value(inst).kind() {
                    ValueKind::Alloc(alloc) => {
                        let _type = dfg.value(inst).ty();
                        match _type.kind() {
                            TypeKind::Pointer(__type) => {
                                byte += __type.size();
                            }
                            _ => unreachable!(),
                        };
                    }
                    _ => {
                        byte += 4;
                    }
                };

            }
        }
        if flag {
            byte += 4;
            byte += arg_cnt * 4;
            info.set_offset((arg_cnt * 4) as i32);
        }
        // return val slot???????????????
        if byte % 16 != 0 {
            byte /= 16;
            byte *= 16;
            byte += 16;
        }
        info.set_stack_length(byte as i32)?;

        if byte > 0 {
            if byte > 2048 {
                RISCV.push_str(format!("  li t0, -{}\n", byte.to_string()).as_str());
                RISCV.push_str("  add sp, sp, t0\n");
            } else {
                RISCV.push_str(format!("  addi sp, sp, -{}\n", byte.to_string()).as_str());
            }
        }
        if flag {
            if byte - 4 > 2047 {
                RISCV.push_str(format!("  li t0, {}\n", (byte - 4).to_string()).as_str());
                RISCV.push_str("  add t0, sp, t0\n");
                RISCV.push_str("  sw ra, 0(t0)\n");

            } else {
                RISCV.push_str(format!("  sw ra, {}(sp)\n", (byte - 4).to_string()).as_str());
            }
        }

        for (&bb, node) in self.layout().bbs() {
            RISCV.push_str(format!(".{}:\n", self.dfg().bb(bb).name().as_ref().unwrap()[1..].to_string()).as_str());
            //println!("{}", node.insts().len());
            for &inst in node.insts().keys() {
                //println!("!");
                info.add_cur_inst(inst);
                let val_data = self.dfg().value(inst);
                val_data.generate(RISCV, info, program);
                info.remove_cur_inst();
                info.free_register();
            }
        }

        if flag {
            if byte - 4 > 2047 {
                RISCV.push_str(format!("  li t0, {}\n", (byte - 4).to_string()).as_str());
                RISCV.push_str("  add t0, sp, t0\n");
                RISCV.push_str("  lw ra, 0(t0)\n");

            } else {
                RISCV.push_str(format!("  lw ra, {}(sp)\n", (byte - 4).to_string()).as_str());
            }
            //RISCV.push_str(format!("  lw ra, {}(sp)\n", (byte - 4).to_string()).as_str());
        }
        if byte > 0 {
            if byte > 2047 {
                RISCV.push_str(format!("  li t0, {}\n", byte.to_string()).as_str());
                RISCV.push_str("  add sp, sp, t0\n");
            } else {
                RISCV.push_str(format!("  addi sp, sp, {}\n", byte.to_string()).as_str());
            }
        }
        info.set_stack_length(0);
        info.set_offset(0);
        RISCV.push_str("  ret\n\n");
        Ok(())
    }
}

impl GenerateRISCV for Value {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        let val_data = program.func(info.cur_func().unwrap()).dfg().value(*self);
        match val_data.kind() {
            ValueKind::Integer(integer) => { integer.generate(RISCV, info, program); }
            ValueKind::GetElemPtr(get_elem_ptr) => {
                let offset = info.get_memory(*self)?;
                if offset > 2047 {
                    RISCV.push_str(format!("  li t1, {}\n", offset.to_string()).as_str());
                    RISCV.push_str("  add t1, sp, t1\n");
                    RISCV.push_str("  lw t1, 0(t1)\n");
                    RISCV.push_str(format!("  lw {}, 0(t1)\n", info.alloc_register()?).as_str());
                } else {
                    RISCV.push_str(format!("  lw t1, {}(sp)\n", offset.to_string()).as_str());
                    RISCV.push_str(format!("  lw {}, 0(t1)\n", info.alloc_register()?).as_str());
                }
            }
            ValueKind::GetPtr(get_ptr) => {
                let offset = info.get_memory(*self)?;
                if offset > 2047 {
                    RISCV.push_str(format!("  li t1, {}\n", offset.to_string()).as_str());
                    RISCV.push_str("  add t1, sp, t1\n");
                    RISCV.push_str("  lw t1, 0(t1)\n");
                    RISCV.push_str(format!("  lw {}, 0(t1)\n", info.alloc_register()?).as_str());
                } else {
                    RISCV.push_str(format!("  lw t1, {}(sp)\n", offset.to_string()).as_str());
                    RISCV.push_str(format!("  lw {}, 0(t1)\n", info.alloc_register()?).as_str());
                }
            }
            _ => {
                let offset = info.get_memory(*self)?;
                if offset > 2047 {
                    RISCV.push_str(format!("  li t1, {}\n", offset.to_string()).as_str());
                    RISCV.push_str("  add t1, sp, t1\n");
                    RISCV.push_str(format!("  lw {}, 0(t1)\n", info.alloc_register()?).as_str());

                } else {
                    RISCV.push_str(format!("  lw {}, {}(sp)\n", info.alloc_register()?, offset.to_string()).as_str());
                }
            }
        };
        Ok(())
    }
}

impl GenerateRISCV for ValueData {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        match self.kind() {
            ValueKind::Return(ret) => ret.generate(RISCV, info, program),
            ValueKind::Integer(integer) => integer.generate(RISCV, info, program),
            ValueKind::Binary(binary) => binary.generate(RISCV, info, program),
            ValueKind::Alloc(alloc) => alloc.generate(RISCV, info, program),
            ValueKind::Load(load) => load.generate(RISCV, info, program),
            ValueKind::Store(store) => store.generate(RISCV, info, program),
            ValueKind::Branch(branch) => branch.generate(RISCV, info, program),
            ValueKind::Jump(jump) => jump.generate(RISCV, info, program),
            ValueKind::Call(call) => call.generate(RISCV, info, program),
            ValueKind::ZeroInit(zero_init) => zero_init.generate(RISCV, info, program),
            ValueKind::Aggregate(aggregate) => aggregate.generate(RISCV, info, program),
            ValueKind::GetElemPtr(get_elem_ptr) => get_elem_ptr.generate(RISCV, info, program),
            ValueKind::GetPtr(get_ptr) => get_ptr.generate(RISCV, info, program),
            //ValueKind::GlobalAlloc(global_alloc) => global_alloc.generate(RISCV, info, program),
            _ => unreachable!(),
        }
    }
}

impl GenerateRISCV for Return {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        //println!("!");
        /*let func_data = program.func(info.cur_func().unwrap());
        let ret_val = self.value().unwrap();
        match func_data.dfg().value(ret_val).kind() {
            ValueKind::Integer(num) => {
                info.add_cur_inst(ret_val);
                func_data.dfg().value(ret_val).generate(RISCV, info, program); 
                info.remove_cur_inst(ret_val);
            },
            _ => {},
        };
        //println!("!");
        RISCV.push_str(format!("  mv a0, {}\n", info.get_register(self.value().unwrap())?).as_str());*/
        let val = self.value();

        match val {
            Some(ret_val) => {
                let dfg = program.func(info.cur_func().unwrap()).dfg();
                match dfg.value(self.value().unwrap()).kind() {
                    ValueKind::Integer(integer) => {
                        RISCV.push_str(format!("  li a0, {}\n", integer.value().to_string()).as_str());
                    }
                    _ => {
                        let offset = info.get_memory(self.value().unwrap())?;
                        if offset > 2047 {
                            RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
                            RISCV.push_str("  add t0, sp, t0\n");
                            RISCV.push_str("  lw a0, 0(t0)\n");
        
                        } else {
                            RISCV.push_str(format!("  lw a0, {}(sp)\n", offset.to_string()).as_str());
                        }
                    }
        
                };
            }
            None => {}
        };
        
        Ok(())
    }
}

impl GenerateRISCV for Integer {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        if info.cur_func() == None {
            RISCV.push_str(format!("  .word {}\n", self.value().to_string()).as_str())
        } else {
            RISCV.push_str(format!("  li {}, {}\n", info.alloc_register()?, self.value().to_string()).as_str());
        }
        Ok(())
    }
}

impl GenerateRISCV for Binary {
    /*fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        if info.get_register(info.cur_inst().unwrap()).is_ok() {
            return Err(Error::InstructionGenerated);
        }
        info.add_cur_inst(self.lhs());
        let result1 = program.func(info.cur_func().unwrap()).dfg().value(self.lhs()).generate(RISCV, info, program);
        info.remove_cur_inst(self.lhs());

        info.add_cur_inst(self.rhs());
        let result2 = program.func(info.cur_func().unwrap()).dfg().value(self.rhs()).generate(RISCV, info, program);
        info.remove_cur_inst(self.rhs());

        let oprand1 = info.get_register(self.lhs())?;
        let oprand2 = info.get_register(self.rhs())?;
        if let Err(Error::InstructionGenerated) = result1 {
            ()
        } else {
            info.free_register(self.lhs());
        }
        if let Err(Error::InstructionGenerated) = result2 {
            ()
        } else {
            info.free_register(self.rhs());
        }
        let result = info.alloc_register(info.cur_inst().unwrap())?;
        match self.op() {
            BinaryOp::Eq => {
                RISCV.push_str(format!("  xor  {}, {}, {}\n", result, oprand1, oprand2).as_str());
                RISCV.push_str(format!("  seqz  {}, {}\n", result, result).as_str());
            },
            BinaryOp::Add => RISCV.push_str(format!("  add  {}, {}, {}\n", result, oprand1, oprand2).as_str()),
            BinaryOp::Sub => RISCV.push_str(format!("  sub  {}, {}, {}\n", result, oprand1, oprand2).as_str()),
            BinaryOp::Mul => RISCV.push_str(format!("  mul  {}, {}, {}\n", result, oprand1, oprand2).as_str()),
            BinaryOp::Div => RISCV.push_str(format!("  div  {}, {}, {}\n", result, oprand1, oprand2).as_str()),
            BinaryOp::Mod => RISCV.push_str(format!("  rem  {}, {}, {}\n", result, oprand1, oprand2).as_str()),
            BinaryOp::Or => RISCV.push_str(format!("  or  {}, {}, {}\n", result, oprand1, oprand2).as_str()),
            BinaryOp::And => RISCV.push_str(format!("  and  {}, {}, {}\n", result, oprand1, oprand2).as_str()),
            BinaryOp::Eq => {
                RISCV.push_str(format!("  xor  {}, {}, {}\n", result, oprand1, oprand2).as_str());
                RISCV.push_str(format!("  seqz  {}, {}\n", result, result).as_str());
            },
            BinaryOp::NotEq => {
                RISCV.push_str(format!("  xor  {}, {}, {}\n", result, oprand1, oprand2).as_str());
                RISCV.push_str(format!("  snez  {}, {}\n", result, result).as_str());
            },
            BinaryOp::Lt => RISCV.push_str(format!("  slt  {}, {}, {}\n", result, oprand1, oprand2).as_str()),
            BinaryOp::Gt => RISCV.push_str(format!("  sgt  {}, {}, {}\n", result, oprand1, oprand2).as_str()),
            BinaryOp::Le => {
                RISCV.push_str(format!("  sgt  {}, {}, {}\n", result, oprand1, oprand2).as_str());
                RISCV.push_str(format!("  seqz  {}, {}\n", result, result).as_str());
            },
            BinaryOp::Ge => {
                RISCV.push_str(format!("  slt  {}, {}, {}\n", result, oprand1, oprand2).as_str());
                RISCV.push_str(format!("  seqz  {}, {}\n", result, result).as_str());
            },
            _ => unreachable!(),
        }
        Ok(())
    }*/
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        self.lhs().generate(RISCV, info, program)?;
        self.rhs().generate(RISCV, info, program)?;
        match self.op() {
            BinaryOp::Add => RISCV.push_str("  add t0, t0, t1\n"),
            BinaryOp::Sub => RISCV.push_str("  sub t0, t0, t1\n"),
            BinaryOp::Mul => RISCV.push_str("  mul t0, t0, t1\n"),
            BinaryOp::Div => RISCV.push_str("  div t0, t0, t1\n"),
            BinaryOp::Mod => RISCV.push_str("  rem t0, t0, t1\n"),
            BinaryOp::Or => RISCV.push_str("  or t0, t0, t1\n"),
            BinaryOp::And => RISCV.push_str("  and t0, t0, t1\n"),
            BinaryOp::Eq => {
                RISCV.push_str("  xor t0, t0, t1\n");
                RISCV.push_str("  seqz t0, t0\n");
            },
            BinaryOp::NotEq => {
                RISCV.push_str("  xor t0, t0, t1\n");
                RISCV.push_str("  snez t0, t0\n");
            },
            BinaryOp::Lt => RISCV.push_str("  slt t0, t0, t1\n"),
            BinaryOp::Gt => RISCV.push_str("  sgt t0, t0, t1\n"),
            BinaryOp::Le => {
                RISCV.push_str("  sgt t0, t0, t1\n");
                RISCV.push_str("  seqz t0, t0\n");
            },
            BinaryOp::Ge => {
                RISCV.push_str("  slt t0, t0, t1\n");
                RISCV.push_str("  seqz t0, t0\n");
            },
            _ => unreachable!(),
        }
        let offset = info.alloc_memory(info.cur_inst().unwrap(), 4)?;
        if offset > 2047 {
            RISCV.push_str(format!("  li t1, {}\n", offset.to_string()).as_str());
            RISCV.push_str("  add t1, sp, t1\n");
            RISCV.push_str("  sw t0, 0(t1)\n");

        } else {
            RISCV.push_str(format!("  sw t0, {}(sp)\n", offset.to_string()).as_str());
        }
        Ok(())
    }
}

impl GenerateRISCV for Alloc {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        let dfg = program.func(info.cur_func().unwrap()).dfg();
        let _type = dfg.value(info.cur_inst().unwrap()).ty();
        match _type.kind() {
            TypeKind::Pointer(__type) => {
                let len = __type.size();
                info.alloc_memory(info.cur_inst().unwrap(), len as i32);
            }
            _ => unreachable!(),
        };
        Ok(())
    }
}

impl GenerateRISCV for Load {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        let src = self.src();
        if src.is_global() {
            RISCV.push_str(format!("  la t0, {}\n", program.borrow_value(src).name().as_ref().unwrap()[1..].to_string()).as_str());
            RISCV.push_str("  lw t0, 0(t0)\n");
        } else {
            self.src().generate(RISCV, info, program)?;
        }
        info.alloc_memory(info.cur_inst().unwrap(), 4);
        let offset = info.get_memory(info.cur_inst().unwrap())?;
        if offset > 2047 {
            RISCV.push_str(format!("  li t1, {}\n", offset.to_string()).as_str());
            RISCV.push_str("  add t1, sp, t1\n");
            RISCV.push_str("  sw t0, 0(t1)\n");
        } else {
            RISCV.push_str(format!("  sw t0, {}(sp)\n", offset.to_string()).as_str());
        }
        Ok(())
    }
}

impl GenerateRISCV for Store {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        fn get_param(index: usize, stack_length: usize) -> String {
            if index < 8 {
                format!("a{}", index.to_string()).to_string()
            } else {
                format!("{}", (4 * (index - 8) + stack_length).to_string()).to_string()
            }
        }

        let func_data = program.func(info.cur_func().unwrap());
        let src = self.value();
        let dest = self.dest();

        /*match func_data.dfg().value(src).kind() {
            ValueKind::Call(call) => {
                let offset = info.get_memory(self.dest())?;
                RISCV.push_str(format!("  sw a0, {}(sp)\n", offset.to_string()).as_str());
                return Ok(());
            }
            _ => {}
        };*/

        let mut flag = false;
        for (index, value) in func_data.params().iter().enumerate() {
            if src == *value {
                flag = true;
                let stack_length = info.get_stack_length()?;
                let pos = get_param(index, stack_length as usize);
                if index < 8 {
                    RISCV.push_str(format!("  mv t0, {}\n", pos).as_str());
                } else {
                    if pos.parse::<i32>().unwrap() > 2047 {
                        RISCV.push_str(format!("  li t0, {}\n", pos).as_str());
                        RISCV.push_str("  add t0, sp, t0\n");
                        RISCV.push_str("  lw t0, 0(t0)\n");
                    } else {
                        RISCV.push_str(format!("  lw t0, {}(sp)\n", pos).as_str());
                    }
                }
                info.alloc_register()?;
                break;
            }
        }

        if !flag {
            self.value().generate(RISCV, info, program)?;
        }

        if dest.is_global() {
            RISCV.push_str(format!("  la t1, {}\n", program.borrow_value(dest).name().as_ref().unwrap()[1..].to_string()).as_str());
            RISCV.push_str("  sw t0, 0(t1)\n");
        } else {
            let offset = info.get_memory(self.dest())?;
            if offset > 2047 {
                RISCV.push_str(format!("  li t1, {}\n", offset.to_string()).as_str());
                RISCV.push_str("  add t1, sp, t1\n");
                match func_data.dfg().value(dest).kind() {
                    ValueKind::GetElemPtr(..) => RISCV.push_str("  lw t1, 0(t1)\n"),
                    ValueKind::GetPtr(..) => RISCV.push_str("  lw t1, 0(t1)\n"),
                    _ => {}
                }
                /*match func_data.dfg().value(dest).ty().kind() {
                    TypeKind::Pointer(_type) => {
                        match _type.kind() {
                            TypeKind::Pointer(__type) => RISCV.push_str("  lw t1, 0(t1)\n"),
                            _ => {}
                        }
                    }
                    _ => {}
                };*/
                RISCV.push_str("  sw t0, 0(t1)\n");
            } else {
                /*match func_data.dfg().value(dest).ty().kind() {
                    TypeKind::Pointer(_type) => {
                        match _type.kind() {
                            TypeKind::Pointer(__type) => {
                                RISCV.push_str(format!("  lw t1, {}(sp)\n", offset.to_string()).as_str());
                                RISCV.push_str("  sw t0, 0(t1)\n");
                                return Ok(())
                            }
                            _ => {}
                        }
                    }
                    _ => {}
                };*/
                match func_data.dfg().value(dest).kind() {
                    ValueKind::GetElemPtr(..) => {//println!("!!!!!!!!!!!!!!!!!!!\n");
                        RISCV.push_str(format!("  lw t1, {}(sp)\n", offset.to_string()).as_str());
                        RISCV.push_str("  sw t0, 0(t1)\n");
                        return Ok(());
                    }
                    ValueKind::GetPtr(..) => {
                        RISCV.push_str(format!("  lw t1, {}(sp)\n", offset.to_string()).as_str());
                        RISCV.push_str("  sw t0, 0(t1)\n");
                        return Ok(());
                    }
                    ValueKind::Alloc(..) => {}
                    _ => {}
                };
                RISCV.push_str(format!("  sw t0, {}(sp)\n", offset.to_string()).as_str());
            }
        }
        Ok(())
    }
}

impl GenerateRISCV for Branch {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        self.cond().generate(RISCV, info, program)?;
        let dfg = program.func(info.cur_func().unwrap()).dfg();
        /*let mem = info.get_memory(self.cond());
        if mem.is_ok() {
            RISCV.push_str(format!("  lw t0, {}(sp)\n", mem.unwrap().to_string()).as_str());
        }*/
        RISCV.push_str(format!("  bnez t0, .{}\n", dfg.bb(self.true_bb()).name().as_ref().unwrap()[1..].to_string()).as_str());
        RISCV.push_str(format!("  j .{}\n", dfg.bb(self.false_bb()).name().as_ref().unwrap()[1..].to_string()).as_str());
        Ok(())
    }
}

impl GenerateRISCV for Jump {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        let dfg = program.func(info.cur_func().unwrap()).dfg();
        RISCV.push_str(format!("  j .{}\n", dfg.bb(self.target()).name().as_ref().unwrap()[1..].to_string()).as_str());
        Ok(())
    }
}

impl GenerateRISCV for Call {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        let mut index = 0;
        let mut dest = String::new();
        let dfg = program.func(info.cur_func().unwrap()).dfg();
        for arg in self.args() {
            if index < 8 {
                dest = format!("a{}", index).to_string();
            } else {
                dest = format!("{}", 4 * (index - 8)).to_string();
            }
            match dfg.value(*arg).kind() {
                ValueKind::Integer(integer) => {
                    if index < 8 {
                        RISCV.push_str(format!("  li {}, {}\n", dest, integer.value().to_string()).as_str());
                    } else {
                        RISCV.push_str(format!("  li t0, {}\n", integer.value().to_string()).as_str());
                        if dest.parse::<i32>().unwrap() > 2047 {
                            RISCV.push_str(format!("  li t1, {}\n", dest).as_str());
                            RISCV.push_str("  add t1, sp, t1\n");
                            RISCV.push_str("  sw t0, 0(t1)\n");
                        } else {
                            RISCV.push_str(format!("  sw t0, {}(sp)\n", dest).as_str()); 
                        }
                    }
                }
                _ => {
                    let offset = info.get_memory(*arg)?;
                    if index < 8 {
                        if offset > 2047 {
                            RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
                            RISCV.push_str("  add t0, sp, t0\n");
                            RISCV.push_str(format!("  lw {}, 0(t0)\n", dest).as_str());
                        } else {
                            RISCV.push_str(format!("  lw {}, {}(sp)\n", dest, offset.to_string()).as_str());
                        }
                    } else {
                        if offset > 2047 {
                            RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
                            RISCV.push_str("  add t0, sp, t0\n");
                            RISCV.push_str("  lw t0, 0(t0)\n");
                        } else {
                            RISCV.push_str(format!("  lw t0, {}(sp)\n", offset.to_string()).as_str());
                        }
                        RISCV.push_str(format!("  sw t0, {}\n", dest).as_str());
                    }
                }
    
            };
            index += 1;
        }
        RISCV.push_str(format!("  call {}\n", program.func(self.callee()).name()[1..].to_string()).as_str());
        if let TypeKind::Function(_, ret_type) = program.func(self.callee()).ty().kind() {
            if !ret_type.is_unit() {
                let offset = info.alloc_memory(info.cur_inst().unwrap(), 4)?;
                if offset > 2047 {
                    RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
                    RISCV.push_str("  add t0, sp, t0\n");
                    RISCV.push_str("  sw a0, 0(t0)\n");
                } else {
                    RISCV.push_str(format!("  sw a0, {}(sp)\n", offset.to_string()).as_str());
                }
            }
        }

        Ok(())
    }
}

impl GenerateRISCV for GlobalAlloc {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        let val_data = program.borrow_value(info.cur_inst().unwrap());
        RISCV.push_str("  .data\n");
        RISCV.push_str(format!("  .global {}\n", val_data.name().as_ref().unwrap()[1..].to_string()).as_str());
        RISCV.push_str(format!("{}:\n", val_data.name().as_ref().unwrap()[1..].to_string()).as_str());
        /*match program.borrow_value(self.init()).kind() {
            ValueKind::ZeroInit(zero_init) => RISCV.push_str(format!("  .zero {}\n", self.ty().size() * 4).as_str()),
            ValueKind::Integer(integer) => RISCV.push_str(format!("  .word {}\n", integer.value()).as_str()),
            ValueKind::Aggregate(aggregate) => 
            _ => {}
        };*/
        program.borrow_value(self.init()).generate(RISCV, info, program)?;
        Ok(())
    }
}

impl GenerateRISCV for ZeroInit {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        if info.cur_func() == None {
            match program.borrow_value(info.cur_inst().unwrap()).ty().kind() {
                TypeKind::Pointer(_type) => RISCV.push_str(format!("  .zero {}\n", _type.size()).as_str()),
                _ => unreachable!(),
            }
        } else {
            return Err(Error::ZeroInit);
        }
        Ok(())
    }
}

impl GenerateRISCV for Aggregate {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        for elem in self.elems() {
            program.borrow_value(*elem).generate(RISCV, info, program)?;
        }
        Ok(())
    }
}

impl GenerateRISCV for GetElemPtr {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        let src = self.src();
        let index = self.index();
        if src.is_global() {
            RISCV.push_str(format!("  la t0, {}\n", program.borrow_value(src).name().as_ref().unwrap()[1..].to_string()).as_str());
        } else {
            let offset = info.get_memory(src)?;
            match program.func(info.cur_func().unwrap()).dfg().value(src).kind() {
                ValueKind::GetElemPtr(..) => {
                    if offset > 2047 {
                        RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
                        RISCV.push_str("  add t0, sp, t0\n");
                        RISCV.push_str("  lw t0, 0(t0)\n");
                    } else {
                        RISCV.push_str(format!("  lw t0, {}(sp)\n", offset.to_string()).as_str());
                    }
                }
                ValueKind::GetPtr(..) => {
                    if offset > 2047 {
                        RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
                        RISCV.push_str("  add t0, sp, t0\n");
                        RISCV.push_str("  lw t0, 0(t0)\n");
                    } else {
                        RISCV.push_str(format!("  lw t0, {}(sp)\n", offset.to_string()).as_str());
                    }
                }
                _ => {
                    if offset > 2047 {
                        RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
                        RISCV.push_str("  add t0, sp, t0\n");
                    } else {
                        RISCV.push_str(format!("  addi t0, sp, {}\n", offset.to_string()).as_str());
                    }
                }
            }
            /*let offset = info.get_memory(src)?;
            if offset > 2047 {
                RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
                RISCV.push_str("  add t0, sp, t0\n");
            } else {
                RISCV.push_str(format!("  addi t0, sp, {}\n", offset.to_string()).as_str());
            }*/
        }
        info.alloc_register()?;
        index.generate(RISCV, info, program)?;
        if src.is_global() {
            match program.borrow_value(src).ty().kind() {
                TypeKind::Pointer(_type) => {
                    match _type.kind() {
                        TypeKind::Array(__type, len) => RISCV.push_str(format!("  li t2, {}\n", __type.size()).as_str()),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
        } else {
            match program.func(info.cur_func().unwrap()).dfg().value(src).ty().kind() {
                //TypeKind::Array(_type, len) => RISCV.push_str(format!("  li t2, {}\n", _type.size() * 4).as_str()),
                TypeKind::Pointer(_type) => {
                    match _type.kind() {
                        TypeKind::Array(__type, len) => RISCV.push_str(format!("  li t2, {}\n", __type.size()).as_str()),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
        }
        RISCV.push_str("  mul t1, t1, t2\n");
        RISCV.push_str("  add t0, t0, t1\n");
        info.alloc_memory(info.cur_inst().unwrap(), 4);
        let offset = info.get_memory(info.cur_inst().unwrap())?;
        if offset > 2047 {
            RISCV.push_str(format!("  li t1, {}\n", offset.to_string()).as_str());
            RISCV.push_str("  add t1, sp, t1\n");
            RISCV.push_str("  sw t0, 0(t1)\n");
        } else {
            RISCV.push_str(format!("  sw t0, {}(sp)\n", offset.to_string()).as_str());
        }
        
        Ok(())
    }
}

impl GenerateRISCV for GetPtr {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        let src = self.src();
        let index = self.index();
        let offset = info.get_memory(src)?;
        if offset > 2047 {
            RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
            RISCV.push_str("  add t0, sp, t0\n");
            RISCV.push_str("  lw t0, 0(t0)\n");
        } else {
            RISCV.push_str(format!("  lw t0, {}(sp)\n", offset.to_string()).as_str());
        }

            /*let offset = info.get_memory(src)?;
            if offset > 2047 {
                RISCV.push_str(format!("  li t0, {}\n", offset.to_string()).as_str());
                RISCV.push_str("  add t0, sp, t0\n");
            } else {
                RISCV.push_str(format!("  addi t0, sp, {}\n", offset.to_string()).as_str());
            }*/
        info.alloc_register()?;
        index.generate(RISCV, info, program)?;
        /*if src.is_global() {
            match program.borrow_value(src).ty().kind() {
                TypeKind::Pointer(_type) => {
                    match _type.kind() {
                        TypeKind::Array(__type, len) => RISCV.push_str(format!("  li t2, {}\n", __type.size()).as_str()),
                        TypeKind::Int32 => RISCV.push_str(format!("  li t2, {}\n", Type::get_i32().size()).as_str()),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
        } else {*/
            match program.func(info.cur_func().unwrap()).dfg().value(src).ty().kind() {
                //TypeKind::Array(_type, len) => RISCV.push_str(format!("  li t2, {}\n", _type.size() * 4).as_str()),
                TypeKind::Pointer(_type) => {
                    match _type.kind() {
                        TypeKind::Array(..) => RISCV.push_str(format!("  li t2, {}\n", _type.size()).as_str()),
                        TypeKind::Int32 => RISCV.push_str(format!("  li t2, {}\n", Type::get_i32().size()).as_str()),
                        _ => unreachable!(),
                    }
                }
                _ => unreachable!(),
            };
        //}
        RISCV.push_str("  mul t1, t1, t2\n");
        RISCV.push_str("  add t0, t0, t1\n");
        info.alloc_memory(info.cur_inst().unwrap(), 4);
        let offset = info.get_memory(info.cur_inst().unwrap())?;
        if offset > 2047 {
            RISCV.push_str(format!("  li t1, {}\n", offset.to_string()).as_str());
            RISCV.push_str("  add t1, sp, t1\n");
            RISCV.push_str("  sw t0, 0(t1)\n");
        } else {
            RISCV.push_str(format!("  sw t0, {}(sp)\n", info.get_memory(info.cur_inst().unwrap())?.to_string()).as_str());
        }
        
        Ok(())
    }
}