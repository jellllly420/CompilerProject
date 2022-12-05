use super::{ Error, Result };
use super::info::Information;
use koopa::ir::entities::{ Program, FunctionData, ValueData, ValueKind };
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
        for (&bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                info.add_cur_inst(inst);
                let val_data = self.dfg().value(inst);
                val_data.generate(RISCV, info, program);
                info.remove_cur_inst(inst);
            }
        }
        Ok(())
    }
}

impl GenerateRISCV for ValueData {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        match self.kind() {
            ValueKind::Return(ret) => ret.generate(RISCV, info, program)?,
            ValueKind::Integer(num) => num.generate(RISCV, info, program)?,
            ValueKind::Binary(binary) => binary.generate(RISCV, info, program)?,
            _ => unreachable!(),
        }
        Ok(())
    }
}

impl GenerateRISCV for Return {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        RISCV.push_str(format!("  mv a0, {}\n", info.get_register(self.value().unwrap())?).as_str());
        RISCV.push_str("  ret\n");
        Ok(())
    }
}

impl GenerateRISCV for Integer {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
        RISCV.push_str(format!("  li  {}, {}\n", info.alloc_register(info.cur_inst().unwrap())?, self.value().to_string()).as_str());
        Ok(())
    }
}

impl GenerateRISCV for Binary {
    fn generate(&self, RISCV: &mut String, info: &mut Information, program: &Program) -> Result<()> {
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
    }
}