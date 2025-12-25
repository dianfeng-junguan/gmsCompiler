use crate::lexer::do_lex;
pub mod lexer;
pub mod parser;
static TEMPLATE_CODE: &str = r#"
fn main() {
    int a=1;
    int b=2;
    int c=a+b;
    return c;
}
    "#;
fn main() {
    //词法分析
    let tokens=do_lex(TEMPLATE_CODE);

}
