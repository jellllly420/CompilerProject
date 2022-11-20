use super::{ Error, Result };
use super::info::Information;
use koopa::ir::entities::{ Program, FunctionData, ValueData, ValueKind };
use koopa::ir::values::*;

pub(super) trait GenerateRISCV {
    fn generate(&self, RISCV: &mut String, info: &mut Information) -> Result<()>;
}

impl GenerateRISCV for Program {
    fn generate(&self, RISCV: &mut String, info: &mut Information) -> Result<()> {   
        RISCV.push_str("  .data\n");
        for &val in self.inst_layout() {
            let val_data = self.borrow_value(val);
            // generate for global value
        }
        RISCV.push_str("  .text\n");
        for &func in self.func_layout() {
            let func_data = self.func(func);
            info.set_cur_func(func);
            func_data.generate(RISCV, info)?;
            info.reset_cur_func();
        }
        Ok(())
    }
}

impl GenerateRISCV for FunctionData {
    fn generate(&self, RISCV: &mut String, info: &mut Information) -> Result<()> {
        if self.layout().entry_bb().is_none() {
            return Ok(());
        }
        let name = &self.name().to_string()[1..];
        RISCV.push_str(format!("  .global {}\n", name).as_str());
        RISCV.push_str(format!("{}:\n", name).as_str());
        for (&bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let val_data = self.dfg().value(inst);
                val_data.generate(RISCV, info)?;
            }
        }
        Ok(())
    }
}

impl GenerateRISCV for ValueData {
    fn generate(&self, RISCV: &mut String, info: &mut Information) -> Result<()> {
        match self.kind() {
            ValueKind::Return(ret) => ret.generate(RISCV, info)?,
            _ => unreachable!(),
        }
        Ok(())
    }
}

impl GenerateRISCV for Return {
    fn generate(&self, RISCV: &mut String, info: &mut Information) -> Result<()> {
        let ret_val_data = info.cur_func_data().dfg().value(self.value().unwrap());
        match ret_val_data.kind() {
            ValueKind::Integer(ret_val) => RISCV.push_str(format!("  li a0, {}\n", ret_val.value().to_string()).as_str()),
            _ => unreachable!(),
        }
        RISCV.push_str("  ret\n");
        Ok(())
    }
}