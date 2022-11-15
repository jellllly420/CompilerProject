use koopa::front::Driver;
use std::fs::read_to_string;
use super::ast::*;
use koopa::ir::Program;

mod symTab;
mod genKoopa;

use genKoopa::GenerateKoopa;

#[derive(Debug)]
pub enum Error {
    WrongPosition,
    DuplicateDefinition,
    UnknownStatementType,
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn ast2Koopa(ast: &CompUnit) -> Result<Program> {
    let lib = read_to_string("src/frontend/ast2Koopa/lib.txt").unwrap();
    let driver: Driver<_> = lib.into();
    let mut program = driver.generate_program().unwrap();
    let mut symbol_table = symTab::SymbolTable::new();
    for (func, func_data) in program.funcs() {
        symbol_table.new_func("a", func.clone());
    }
    ast.generate(&mut program, &mut symbol_table)?;
    Ok(program)
}