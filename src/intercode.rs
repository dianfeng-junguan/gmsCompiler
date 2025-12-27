use crate::parser::{ASTNode, ExprNode, ExprNodeType, FunctionNode, StatementNode, StatementType};

/*
    specifices of intermediate code
    example:
    fn main():int{
        int a=1;
        int b=2;
        int c=0;
        c=a+b;
    }
    to:
    func main 4 ()
    alloc a 4
    mov a 1
    alloc b 4
    mov b 2
    alloc c 4
    mov c 0
    alloc tmp 4
    mov tmp a
    add tmp b
    mov c tmp
    retemp
    (if we have return c we write 'ret c')
    basic intercode:
    alloc varname size
    mov varname value/varname
    add varname value/varname
    sub varname value/varname
    mul varname value/varname
    div varname value/varname
    func funcname retvsize (arg1,arg2,...)
    retemp
    ret varname/value
*/
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IntermediateCodeType {
    Alloc,
    Mov,
    Add,
    Sub,
    Mul,
    Div,
    FuncCall,
    PropertyVisit,
    Retemp,
    Ret,
    FuncDef
}
#[derive(Debug, Clone)]
pub struct IntermediateCode {
    pub code_type: IntermediateCodeType,
    pub operands: Vec<String>,
}
impl IntermediateCode {
    pub fn new(code_type: IntermediateCodeType, operands: Vec<String>) -> Self {
        IntermediateCode {
            code_type,
            operands,
        }
    }
}
/// translate an expression node to intermediate code
/// return None if translation fails
/// # Arguments
/// * `exprnode` - expression node to translate
/// * `alloced_tmpvar_num` - number of temporary variables already allocated, used to generate new temporary variable names
/// # Returns
/// Option containing a tuple of vector of intermediate codes and the result variable name
/// if the expr is under another expr, the returned code is supposed to be appended before the upper expr's code
/// and the upper expr can use the returned variable name as operand
/// usize is the number of temporary variables allocated in this translation
fn translate_expr(exprnode:&ExprNode, alloced_tmpvar_num: usize)->Option<(Vec<IntermediateCode>,String, usize)>{
    let mut tmpnamec=0;
    let mut intercodes:Vec<IntermediateCode>=Vec::new();
    match exprnode.nodetype {
        ExprNodeType::VALUE=>{
            // no extra intercode and directly return the value
            return Some((vec![], exprnode.value.clone().unwrap(), 0));
        },
        ExprNodeType::ADD|ExprNodeType::SUB|ExprNodeType::MUL|ExprNodeType::DIV|ExprNodeType::FUNCCALL=>{
            // two-operand node need to translate left and right nodes
            let (left_codes, left_var, left_tmpnum)=translate_expr(exprnode.left.as_ref().unwrap(), alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=left_tmpnum;
            intercodes.extend(left_codes);
            let (right_codes, right_var, right_tmpnum)=translate_expr(exprnode.right.as_ref().unwrap(), alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=right_tmpnum;
            intercodes.extend(right_codes);
            // now generate a new temporary variable to store the result
            let tmp_var_name=format!("tmp{}", alloced_tmpvar_num + tmpnamec);
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Alloc, vec![tmp_var_name.clone(), "4".to_string()]));
            tmpnamec+=1;
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Mov, vec![tmp_var_name.clone(),left_var.clone()]));
            // generate the operation code
            let op_type=match exprnode.nodetype {
                ExprNodeType::ADD=>IntermediateCodeType::Add,
                ExprNodeType::SUB=>IntermediateCodeType::Sub,
                ExprNodeType::MUL=>IntermediateCodeType::Mul,
                ExprNodeType::DIV=>IntermediateCodeType::Div,
                ExprNodeType::FUNCCALL=>IntermediateCodeType::FuncCall,
                ExprNodeType::PROPERTYVISIT=>IntermediateCodeType::PropertyVisit,
                _=>return None,// should not reach here
            };
            intercodes.push(IntermediateCode::new(op_type, vec![tmp_var_name.clone(), right_var]));
            return Some((intercodes, tmp_var_name, tmpnamec));
        },
        // single-operand node
        _=>{}
    }
    None
}
fn translate_stmt(stmt:&StatementNode, alloced_tmpvar_num: usize)->Option<(Vec<IntermediateCode>, usize)>{
    let mut tmpnamec=0;
    let mut intercodes:Vec<IntermediateCode>=Vec::new();
    match stmt.stmttype{
        StatementType::Definition=>{
            // translate the expression on the right side
            let (expr_codes, expr_var, expr_tmpnum)=translate_expr(&stmt.expr, alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=expr_tmpnum;
            intercodes.extend(expr_codes);
            // allocate the variable
            // TODO : determine variable size based on type
            let varsize="4".to_string();
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Alloc, vec![stmt.id.value.clone(), varsize]));
            // move the value to the variable
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Mov, vec![stmt.id.value.clone(), expr_var]));
            return Some((intercodes, tmpnamec));
        },
        StatementType::Assign=>{
            // translate the expression on the right side
            let (expr_codes, expr_var, expr_tmpnum)=translate_expr(&stmt.expr, alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=expr_tmpnum;
            intercodes.extend(expr_codes);
            // move the value to the variable
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Mov, vec![stmt.id.value.clone(), expr_var]));
            return Some((intercodes, tmpnamec));
        },
        StatementType::SingleExpr=>{
            // translate the expression
            let (expr_codes, _expr_var, expr_tmpnum)=translate_expr(&stmt.expr, alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=expr_tmpnum;
            intercodes.extend(expr_codes);
            return Some((intercodes, tmpnamec));
        },
        _=>{}
    }
    None
}
fn translate_function(funcnode:&FunctionNode,alloced_tmpvar_num: usize)->Option<(Vec<IntermediateCode>,usize)>{
    let mut intercodes:Vec<IntermediateCode>=Vec::new();
    let statements=&funcnode.stmts;
    let mut tmpvarc=0;
    // TODO: determine return value size based on function return type
    let retv_size="4".to_string();
    //generate argument list string
    let arglist_str={
        let mut args=String::new();
        for (i,(paramid, _paramtype)) in funcnode.params.iter().enumerate() {
            args.push_str(&paramid.value);
            args.push_str(":");
            // TODO: determine value size based on function return type
            args.push_str("4");
            if i!=funcnode.params.len()-1 {
                args.push_str(",");
            }
        }
        args
    };
    // func definition code
    intercodes.push(IntermediateCode::new(IntermediateCodeType::FuncDef, vec![funcnode.id.value.clone(), retv_size, arglist_str]));
    for stmt in statements {
        let (stmt_codes, stmt_tmpnum)=translate_stmt(stmt, alloced_tmpvar_num)?;
        tmpvarc+=stmt_tmpnum;
        intercodes.extend(stmt_codes);
    }
    Some((intercodes, tmpvarc))
}
pub fn generate_intermediate_code(ast:&ASTNode)->Option<Vec<IntermediateCode>>{
    let mut intercodes:Vec<IntermediateCode>=Vec::new();
    for funcnode in ast.functions.iter() {
        let (func_codes, _func_tmpnum)=translate_function(funcnode, 0)?;
        intercodes.extend(func_codes);
    }
    Some(intercodes)
}
#[test]
fn test_generate_itermediate_code(){
    let source="
    fn main():int{
        int a=1;
        int b=2;
        int c=0;
        c=a+b;
    }
    ";
    let tokens=crate::lexer::do_lex(source);
    let ast=crate::parser::generate_ast(&tokens).unwrap();
    let intercodes=generate_intermediate_code(&ast).unwrap();
    println!("Intermediate Codes:");
    for code in &intercodes {
        println!("{:?}", code);
    }
}
#[test]
fn test_translate_expr(){
    // test translating the expression a + b
    let exprnode=ExprNode{
        nodetype:ExprNodeType::ADD,
        left:Some(Box::new(ExprNode{
            nodetype:ExprNodeType::VALUE,
            left:None,
            value:Some("a".to_string()),
            right:None,
        })),
        value:None,
        right:Some(Box::new(ExprNode{
            nodetype:ExprNodeType::VALUE,
            left:None,
            value:Some("b".to_string()),
            right:None,
        })),
    };
    let (intercodes, result_var, tmpnum)=translate_expr(&exprnode, 0).unwrap();
    println!("Intermediate Codes:");
    for code in &intercodes {
        println!("{:?}", code);
    }
    assert_eq!(tmpnum, 1);
    assert_eq!(result_var, "tmp0".to_string());
}