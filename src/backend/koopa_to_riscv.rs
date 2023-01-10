use koopa::ir::Program;

mod gen_riscv;
mod info;

use gen_riscv::GenerateRISCV;

#[allow(dead_code)]
#[derive(Debug)]
pub enum Error {
    // PlaceHolder,
    // RegisterRunOut,
    NoSuchInstruction,
    // InstructionGenerated,
    ZeroInit,
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn koopa_to_riscv(program: &Program) -> Result<String> {
    let mut riscv = String::new();
    let mut info = info::Information::new();
    program.generate(&mut riscv, &mut info, &program)?;
    Ok(riscv)
}