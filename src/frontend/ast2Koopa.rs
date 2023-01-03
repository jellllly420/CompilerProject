use koopa::front::Driver;
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
    UndefinedLVal,
    WrongTypeValue,
    NeedAssignment,
    AssignToConst
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn ast2Koopa(ast: &CompUnit) -> Result<Program> {
    let lib = r"decl @getint(): i32
decl @getch(): i32
decl @getarray(*i32): i32
decl @putint(i32): i32
decl @putch(i32): i32
decl @putarray(i32, *i32): i32
decl @starttime(): i32
decl @stoptime(): i32";
    let driver: Driver<_> = lib.into();
    let mut program = driver.generate_program().unwrap();
    let mut symbol_table = symTab::SymbolTable::new();
    for (func, func_data) in program.funcs() {
        symbol_table.new_func(func_data.name().to_string(), func.clone());
    }
    ast.generate(&mut program, &mut symbol_table)?;
    Ok(program)
}