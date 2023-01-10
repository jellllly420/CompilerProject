use super::ast::*;
use koopa::ir::Program;

mod sym_tab;
mod gen_koopa;
mod const_eval;

use gen_koopa::GenerateKoopa;

#[allow(dead_code)]
#[derive(Debug)]
pub enum Error {
    WrongPosition,
    DuplicateDefinition,
    UnknownStatementType,
    UndefinedLVal,
    WrongTypeValue,
    NeedAssignment,
    AssignToConst,
    NoLoopWrapped,
    UndefinedFunction,
    ConstantNotEvaluated,
}

pub type Result<T> = std::result::Result<T, Error>;

pub fn ast_to_koopa(ast: &CompUnit) -> Result<Program> {
    /*let lib = r"decl @getint(): i32
decl @getch(): i32
decl @getarray(*i32): i32
decl @putint(i32): i32
decl @putch(i32): i32
decl @putarray(i32, *i32): i32
decl @starttime(): i32
decl @stoptime(): i32";
    let driver: Driver<_> = lib.into();
    let mut program = driver.generate_program().unwrap();*/
    let mut program = Program::new();
    let mut symbol_table = sym_tab::SymbolTable::new();
    for (func, func_data) in program.funcs() {
        symbol_table.new_func(func_data.name().to_string(), func.clone())?;
    }
    ast.generate(&mut program, &mut symbol_table)?;
    Ok(program)
}