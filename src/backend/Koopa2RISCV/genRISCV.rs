use super::{ Error, Result };
use super::info::Information;
use koopa::ir::entities::{ Program, FunctionData, ValueData, ValueKind, Value };
use koopa::ir::values::*;

pub(super) trait GenerateRISCV {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()>;
}

impl GenerateRISCV for Program {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {   
        RISCV.push_str("  .data\n");
        for &val in self.inst_layout() {
            let val_data = self.borrow_value(val);
            // generate for global value
        }
        RISCV.push_str("  .text\n");
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
        let name = &self.name().to_string()[1..];
        RISCV.push_str(format!("  .global {}\n", name).as_str());
        RISCV.push_str(format!("{}:\n", name).as_str());
        let mut byte = 0;
        let dfg = self.dfg();
        for (&bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                if dfg.value(inst).ty().is_unit() {
                    continue;
                }
                byte = byte + 4;
            }
        }
        byte = byte + 4;
        if byte % 16 != 0 {
            byte /= 16;
            byte *= 16;
            byte += 16;
        }
        RISCV.push_str(format!("  addi sp, sp, -{}\n", byte.to_string()).as_str());
        info.set_stack_length(byte)?;
        for (&bb, node) in self.layout().bbs() {
            RISCV.push_str(format!(".{}:\n", self.dfg().bb(bb).name().as_ref().unwrap()[1..].to_string()).as_str());
            //println!("{}", node.insts().len());
            for &inst in node.insts().keys() {
                //println!("!");
                info.add_cur_inst(inst);
                let val_data = self.dfg().value(inst);
                val_data.generate(RISCV, info, program);
                info.remove_cur_inst(inst);
                info.free_register();
            }
        }
        Ok(())
    }
}

impl GenerateRISCV for Value {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        let val_data = program.func(info.cur_func().unwrap()).dfg().value(*self);
        match val_data.kind() {
            ValueKind::Integer(integer) => { integer.generate(RISCV, info, program); }
            _ => {
                let offset = info.get_memory(*self)?;
                RISCV.push_str(format!("  lw {}, {}(sp)\n", info.alloc_register()?, offset.to_string()).as_str());
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
        let dfg = program.func(info.cur_func().unwrap()).dfg();
        match dfg.value(self.value().unwrap()).kind() {
            ValueKind::Integer(integer) => {
                RISCV.push_str(format!("  li a0, {}\n", integer.value().to_string()).as_str());
            }
            _ => {
                let offset = info.get_memory(self.value().unwrap())?;
                RISCV.push_str(format!("  lw a0, {}(sp)\n", offset.to_string()).as_str());
            }

        };
        RISCV.push_str(format!("  addi sp, sp, {}\n", info.get_stack_length()?.to_string()).as_str());
        info.set_stack_length(0);
        RISCV.push_str("  ret\n");
        Ok(())
    }
}

impl GenerateRISCV for Integer {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        RISCV.push_str(format!("  li {}, {}\n", info.alloc_register()?, self.value().to_string()).as_str());
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
        let offset = info.alloc_memory(info.cur_inst().unwrap())?;      
        RISCV.push_str(format!("  sw t0, {}(sp)\n", offset.to_string()).as_str());
        Ok(())
    }
}

impl GenerateRISCV for Alloc {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        info.alloc_memory(info.cur_inst().unwrap());
        Ok(())
    }
}

impl GenerateRISCV for Load {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        self.src().generate(RISCV, info, program)?;
        info.alloc_memory(info.cur_inst().unwrap());   
        RISCV.push_str(format!("  sw t0, {}(sp)\n", info.get_memory(info.cur_inst().unwrap())?.to_string()).as_str());
        Ok(())
    }
}

impl GenerateRISCV for Store {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        self.value().generate(RISCV, info, program)?;
        let offset = info.get_memory(self.dest())?;
        RISCV.push_str(format!("  sw t0, {}(sp)\n", offset.to_string()).as_str());
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