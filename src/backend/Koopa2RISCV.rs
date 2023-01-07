use koopa::ir::Program;

mod genRISCV;
mod info;

use genRISCV::GenerateRISCV;

#[derive(Debug)]
pub enum Error {
    PlaceHolder,
    RegisterRunOut,
    NoSuchInstruction,
    InstructionGenerated,
    ZeroInit,
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn Koopa2RISCV(program: &Program) -> Result<String> {
    let mut RISCV = String::new();
    let mut info = info::Information::new();
    program.generate(&mut RISCV, &mut info, &program);
    Ok(RISCV)
}