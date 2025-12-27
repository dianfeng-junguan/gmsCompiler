use crate::lexer::do_lex;
pub mod lexer;
pub mod parser;
pub mod intercode;
pub mod asmgen;
static TEMPLATE_CODE: &str = r#"
fn main():int {
    int a=1;
    int b=2;
    int c=a+b;
}
    "#;
fn main() {
    //词法分析
    let tokens=do_lex(TEMPLATE_CODE);

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
