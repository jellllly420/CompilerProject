mod frontend;
mod backend;

use lalrpop_util::lalrpop_mod;
use koopa::back::KoopaGenerator;
use std::env::args;
use std::fs::{ read_to_string, write };
use std::io::Result;
use std::str::from_utf8;

// 引用 lalrpop 生成的解析器
// 因为我们刚刚创建了 sysy.lalrpop, 所以模块名是 sysy
lalrpop_mod!(sysy);

fn main() -> Result<()> {
    // 解析命令行参数
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();
    let input = args.next().unwrap();
    args.next();
    let output = args.next().unwrap();

    // 读取输入文件
    let input = read_to_string(input)?;

    // 调用 lalrpop 生成的 parser 解析输入文件
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();


    let program = frontend::ast2Koopa::ast2Koopa(&ast).unwrap();

    

    match mode.as_str() {
        "-koopa" => {
            let mut generator = KoopaGenerator::new(Vec::new());
            generator.generate_on(&program).unwrap();
            let text_form_ir = from_utf8(&generator.writer()).unwrap().to_string();
            write(output, text_form_ir);
        }
        "-riscv" => {
            let RISCV = backend::Koopa2RISCV::Koopa2RISCV(&program).unwrap();
            write(output, RISCV);
        }
        _ => unreachable!(),
    }

    // 输出解析得到的 AST
    //println!("{:#?}", ast);
    Ok(())
}
