/*
    definitions of error data structures used.

*/

use std::sync::Mutex;

pub(crate) const ERR_LEXER:&str="Lexer Error";
pub(crate) const ERR_PARSER:&str="Parser Error";
pub(crate) const ERR_SEMATIC:&str="Sematic Error";
pub(crate) type ErrList=Vec<CompilerError>;
#[derive(Debug, Clone)]
pub struct CompilerError{
    // who raised the error(lexer, parser etc.)
    pub sender: String,
    pub err_msg: String,
    pub line: usize,
    pub column: usize,

}
impl CompilerError {
    pub fn new(sender: &str, err_msg: &str, line: usize, column: usize) -> Self {
        CompilerError {
            sender: sender.to_string(),
            err_msg: err_msg.to_string(),
            line,
            column,
        }
    }
}
static ERRS:Mutex<ErrList>=Mutex::new(Vec::new());
/// cry out an error and push it to the error list
pub fn cry_err(sender:&str, err_msg:&str, line:usize, column:usize){
    push_err( sender, err_msg, line, column);
    println!("Error from {} at (line {}, column {}): {}", sender, line, column, err_msg);
}
pub fn push_err(sender:&str, err_msg:&str, line:usize, column:usize){
    let err=CompilerError::new(sender, err_msg, line, column);
    ERRS.lock().unwrap().push(err);
}