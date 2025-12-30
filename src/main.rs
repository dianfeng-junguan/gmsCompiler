use std::{fs::OpenOptions, io::Read, ptr::read};

use clap::{Parser};

use crate::lexer::do_lex;
pub mod lexer;
pub mod parser;
pub mod intercode;
pub mod asmgen;
pub mod sematic;
pub mod errs;
pub mod defs;
static TEMPLATE_CODE: &str = r#"
    fn main():int{
        let a=1;
        let b=2;
        let c=0;
        c=a+b;
        if a==1 {
            let d=11;
            d=d+1;
        }else if b==2{
            let e=2;
            let d=1;
        }else{
            let f=12;
        }
        let i=0;
        while i<10 {
            i=i+1;
        }
        printf(c);
        return 0;
    }
    "#;

#[derive(Parser,Debug)]
#[command(version("1.0.0"),about, long_about=None)]
struct CommandLineArgs{
    #[clap(short, long)]
    inputfile: String,
    #[clap(short, long)]
    output_path: String
}
fn main() {
    let args=CommandLineArgs::parse();
    let input_path=args.inputfile;
    let output_path=args.output_path;
    let mut file=OpenOptions::new().read(true).open(input_path).expect("failed to open input file");
    let mut code= String::new();
    file.read_to_string(&mut code).expect("failed to read input file");
    //词法分析
    let tokens=do_lex(&code);
    let ast=parser::generate_ast(&tokens).unwrap();
    let intercodes=intercode::generate_intermediate_code(&ast).unwrap();
    let nasmcode=asmgen::generate_nasm(&intercodes).unwrap();
    std::fs::write(output_path, nasmcode).expect("failed to write output file");
}
#[test]
fn test_whole(){
    let tokens=do_lex(TEMPLATE_CODE);
    let ast=parser::generate_ast(&tokens).unwrap();
    let intercodes=intercode::generate_intermediate_code(&ast).unwrap();
    println!("Generated Intermediate Codes:");
    for code in &intercodes {
        println!("{:?}", code);
    }
    let nasmcode=asmgen::generate_nasm(&intercodes).unwrap();
    println!("Generated NASM Code:\n{}", nasmcode);
}
