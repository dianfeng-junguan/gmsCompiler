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
        printf(c);
        return 0;
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
