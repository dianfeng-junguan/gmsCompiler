use std::{collections::BTreeMap, fmt::Debug, ops, sync::Mutex, vec};

use crate::{defs::get_vartype_size, errs::{ERR_INTERCODER, cry_err}, parser::{ASTNode, ExprNode, ExprNodeType, FunctionNode, StatementNode, StatementType}};

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
    func main 8 ()
    alloc a 8
    mov a 1
    alloc b 8
    mov b 2
    alloc c 8
    mov c 0
    alloc tmp 8
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
    AllocGlobal,
    AllocLocal,
    AllocTemp,
    Free,
    ScopeStart,
    ScopeEnd,
    Mov,
    Add,
    Sub,
    Mul,
    Div,
    Cmp,
    Je,
    Ja,
    Jb,
    Jna,
    Jnb,
    Jmp,
    Label,
    FuncCall,
    StoreRetValue,
    PropertyVisit,
    Retemp,
    Ret,
    FuncDef
}

#[derive(Clone)]
pub struct IntermediateCode {
    pub code_type: IntermediateCodeType,
    pub operands: Vec<String>,
}
impl Debug for IntermediateCode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&format!("{:?} ", self.code_type))?;
        for ops in &self.operands {
            f.write_str(&format!("{},", ops))?;
        }
        Ok(())
    }
}
impl IntermediateCode {
    pub fn new(code_type: IntermediateCodeType, operands: Vec<String>) -> Self {
        IntermediateCode {
            code_type,
            operands,
        }
    }
    //
    pub fn alloc_temp(name:&str,size:usize)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::AllocTemp,
            operands:vec![name.to_string(), size.to_string()],
        }
    }

    pub fn alloc_local(name:&str,size:usize)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::AllocLocal,
            operands:vec![name.to_string(), size.to_string()],
        }
    }

    pub fn free(name:&str)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::Free,
            operands:vec![name.to_string()],
        }
    }

    pub const fn scope_start()->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::ScopeStart,
            operands:vec![],
        }
    }

    pub const fn scope_end()->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::ScopeEnd,
            operands:vec![],
        }
    }

    pub fn label(labelname:&str)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::Label,
            operands:vec![labelname.to_string()],
        }
    }

    pub fn compare(left:&str, right:&str)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::Cmp,
            operands:vec![left.to_string(), right.to_string()],
        }
    }

    pub fn je(label:&str)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::Je,
            operands:vec![label.to_string()],
        }
    }


    pub fn ja(label:&str)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::Ja,
            operands:vec![label.to_string()],
        }
    }

    pub fn jb(label:&str)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::Jb,
            operands:vec![label.to_string()],
        }
    }

    pub fn jna(label:&str)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::Jna,
            operands:vec![label.to_string()],
        }
    }

    pub fn jnb(label:&str)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::Jnb,
            operands:vec![label.to_string()],
        }
    }
    pub fn jmp(label:&str)->Self{
        IntermediateCode{
            code_type:IntermediateCodeType::Jmp,
            operands:vec![label.to_string()],
        }
    }
    
    // non-static methods

    pub fn get_funccall_args(&self)->Option<Vec<(String, usize)>>{
        if self.code_type!=IntermediateCodeType::FuncCall {
            return None;
        }
        if self.operands.len()<2 {
            return None;
        }
        let arglist_str=&self.operands[1];
        let mut args:Vec<(String, usize)>=vec![];
        if arglist_str.is_empty() {
            return Some(args);
        }
        let argpairs:Vec<&str>=arglist_str.split(",").collect();
        for argpair in argpairs {
            let parts:Vec<&str>=argpair.split(":").collect();
            args.push((parts[0].to_string(), 8));
            
        }
        Some(args)
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
        ExprNodeType::COMMALIST=>{
            /*
                for commalist, we:
                1. translate left node
                2. free left node temp var if any
                3. translate right node
                4. return the right node tempvar
             */
            let leftnode=exprnode.left.as_ref().unwrap();
            let rightnode=exprnode.right.as_ref().unwrap();

            let (left_codes, left_var, _)=translate_expr(leftnode, alloced_tmpvar_num + tmpnamec)?;
            intercodes.extend(left_codes);
            // free left node temp var
            intercodes.push(IntermediateCode::free(&left_var));
            // calc right node
            let (right_codes, right_var, right_tmpnum)=translate_expr(rightnode, alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=right_tmpnum;
            intercodes.extend(right_codes);
            return Some((intercodes, right_var, tmpnamec));
        },
        ExprNodeType::FUNCCALL=>{
            let leftnode=exprnode.left.as_ref().unwrap();
            let arglistnode=exprnode.right.as_ref().unwrap();
            // translate function addr
            let (funcaddr_codes, funcaddr_var, allocated_tmpvarnum)=translate_expr(leftnode, alloced_tmpvar_num + tmpnamec)?;
            intercodes.extend(funcaddr_codes);
            // translate arg list
            let mut arg_vars:Vec<String>=vec![];
            let mut tovisit=vec![arglistnode.as_ref()];
            while !tovisit.is_empty() {
                let current=tovisit.pop().unwrap();
                if current.nodetype==ExprNodeType::COMMALIST {
                    let left=current.left.as_ref().unwrap();
                    let right=current.right.as_ref().unwrap();
                    tovisit.push(right.as_ref());
                    tovisit.push(left.as_ref());
                }else if current.nodetype==ExprNodeType::VALUE {
                    let arg_var=current.value.clone().expect(format!("{}: met VALUE node without value in func call arg list", ERR_INTERCODER).as_str());
                    arg_vars.push(arg_var);
                }else{
                    cry_err(ERR_INTERCODER, "met unknown expr node type in func call arg list", 0, 0);
                    return None;
                }
            }
            // make the final func call code
            let mut funccall_oprands:Vec<String>=vec![funcaddr_var.clone()];
            funccall_oprands.extend(arg_vars);
            intercodes.push(IntermediateCode::new(IntermediateCodeType::FuncCall, funccall_oprands));
            // now free the func addr temp var
            if allocated_tmpvarnum>0 {
                // this is an calculated func addr, free it
                intercodes.push(IntermediateCode::free(&funcaddr_var));
            }
            // allocate a temp var to store the return value
            let tmp_var_name=format!("tmp{}", alloced_tmpvar_num + tmpnamec);
            intercodes.push(IntermediateCode::alloc_temp(&tmp_var_name, 8));
            tmpnamec+=1;
            // move the return value to the temp var
            intercodes.push(IntermediateCode::new(IntermediateCodeType::StoreRetValue,vec![tmp_var_name.clone()]));
            return Some((intercodes, tmp_var_name, tmpnamec));
        },
        ExprNodeType::EQUAL|ExprNodeType::GT|ExprNodeType::LT|ExprNodeType::GE|
        ExprNodeType::LE=>{
            let leftnode=exprnode.left.as_ref().unwrap();
            let rightnode=exprnode.right.as_ref().unwrap();
            let (left_codes, left_var, lefttmpvarnum)=translate_expr(leftnode, alloced_tmpvar_num + tmpnamec)?;
            // the sub expr is supposed to free it's subexpr's tmp var so it should only generate one temp var to store the result
            tmpnamec+=lefttmpvarnum;
            intercodes.extend(left_codes);
            let (right_codes, right_var, righttmpvarnum)=translate_expr(rightnode, alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=righttmpvarnum;
            intercodes.extend(right_codes);
            // now generate a new temporary variable to store the result
            let var_cmpres=format!("tmp{}", alloced_tmpvar_num + tmpnamec);
            intercodes.push(IntermediateCode::alloc_temp(&var_cmpres, 8));
            tmpnamec+=1;
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Mov, vec![var_cmpres.clone(), left_var.clone()]));

            // we compare the two by subtracting right from left and checking if the result is 0
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Sub, vec![var_cmpres.clone(),right_var.clone()]));
            intercodes.push(IntermediateCode::compare(&var_cmpres, "0"));
            // je
            let eqzero=format!("eqzero{}", alloc_global_id());
            intercodes.push(match exprnode.nodetype {
                ExprNodeType::EQUAL=>IntermediateCode::je(&eqzero),
                ExprNodeType::GT=>IntermediateCode::jna(&eqzero),
                ExprNodeType::LT=>IntermediateCode::jnb(&eqzero),
                ExprNodeType::GE=>IntermediateCode::jb(&eqzero),
                ExprNodeType::LE=>IntermediateCode::ja(&eqzero),
                _=>{
                    cry_err(ERR_INTERCODER, "met unknown comparation operator", 0, 0);
                    return None;
                }
            });
            // not equal path: set tmp_var_name to 1
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Mov, vec![var_cmpres.clone(), "1".to_string()]));
            // jmp to end
            let endlabel=format!("endeq{}", alloc_global_id());
            intercodes.push(IntermediateCode::jmp(&endlabel));
            // equal path: set tmp_var_name to 0
            intercodes.push(IntermediateCode::label(&eqzero));
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Mov, vec![var_cmpres.clone(), "0".to_string()]));
            // end label
            intercodes.push(IntermediateCode::label(&endlabel));
            // free left var and right var
            if leftnode.nodetype!=ExprNodeType::VALUE {
                intercodes.push(IntermediateCode::free(&left_var));
                tmpnamec-=1;
            }
            if rightnode.nodetype!=ExprNodeType::VALUE {
                intercodes.push(IntermediateCode::free(&right_var));
                tmpnamec-=1;
            }
            return Some((intercodes, var_cmpres, tmpnamec));
        },
        ExprNodeType::ADD|ExprNodeType::SUB|ExprNodeType::MUL|
        ExprNodeType::DIV=>{
            // two-operand node need to translate left and right nodes
            let leftnode=exprnode.left.as_ref().unwrap();
            let rightnode=exprnode.right.as_ref().unwrap();
            let (left_codes, left_var, _)=translate_expr(leftnode, alloced_tmpvar_num + tmpnamec)?;
            // the sub expr is supposed to free it's subexpr's tmp var so it should only generate one temp var to store the result
            tmpnamec+=1;
            intercodes.extend(left_codes);
            let (right_codes, right_var, _)=translate_expr(rightnode, alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=1;
            intercodes.extend(right_codes);
            // now generate a new temporary variable to store the result
            let tmp_var_name=format!("tmp{}", alloced_tmpvar_num + tmpnamec);
            intercodes.push(IntermediateCode::alloc_temp(&tmp_var_name, 8));
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
            intercodes.push(IntermediateCode::new(op_type, vec![tmp_var_name.clone(), right_var.clone()]));
            // now the temp var of the left and right expr can be freed
            if leftnode.nodetype!=ExprNodeType::VALUE {
                intercodes.push(IntermediateCode::free(&left_var));
            }
            if rightnode.nodetype!=ExprNodeType::VALUE {
                intercodes.push(IntermediateCode::free(&right_var));
            }
            return Some((intercodes, tmp_var_name, tmpnamec));
        },
        // single-operand node
        _=>{
            cry_err("Intermediate Code Generator", "met unknown expr node type", 0, 0);
        }
    }
    None
}
static GLOBAL_ID:Mutex<usize>=Mutex::new(0);
fn alloc_global_id()->usize{
    let mut id=GLOBAL_ID.lock().unwrap();
    let prev=*id;
    *id+=1;
    prev
}
fn translate_stmt(stmt:&StatementNode, alloced_tmpvar_num: usize, symbols:&mut Vec<Symbol>)->Option<(Vec<IntermediateCode>, usize)>{
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
            let varsize=8;
            intercodes.push(IntermediateCode::alloc_local(&stmt.id.value, varsize));
            // move the value to the variable
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Mov, vec![stmt.id.value.clone(), expr_var.clone()]));
            // free the temporary variables used in expression
            if stmt.expr.nodetype!=ExprNodeType::VALUE {
                intercodes.push(IntermediateCode::free(&expr_var));
            }
            // add to symbol table
            // set the address to 0 for now, will be set during memory allocation
            symbols.push(Symbol::new(stmt.id.value.clone(), SymbolType::Variable, false, vec![], 8, 0));
            return Some((intercodes, tmpnamec));
        },
        StatementType::Assign=>{
            // translate the expression on the right side
            let (expr_codes, expr_var, expr_tmpnum)=translate_expr(&stmt.expr, alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=1;
            intercodes.extend(expr_codes);
            // move the value to the variable
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Mov, vec![stmt.id.value.clone(), expr_var.clone()]));
            // free the temporary variables used in expression
            if stmt.expr.nodetype!=ExprNodeType::VALUE {
                intercodes.push(IntermediateCode::free(&expr_var));
            }
            return Some((intercodes, tmpnamec));
        },
        StatementType::SingleExpr=>{
            // translate the expression
            let (expr_codes, expr_var, expr_tmpnum)=translate_expr(&stmt.expr, alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=1;
            intercodes.extend(expr_codes);
            if stmt.expr.nodetype!=ExprNodeType::VALUE {
                intercodes.push(IntermediateCode::free(&expr_var));
            }
            return Some((intercodes, tmpnamec));
        },
        StatementType::If=>{
            /*
                1. calc the condition expr
                2. cmp the result with 0
                3. je to else
                4. put the if body codes
                5. jmp to end
                6. calc the elseif condition expr
                7. je to next elseif / else
                8. put the elseif body codes
                9. if more elseif, jmp to 6
                10. put the else body codes
                11. end:
            */
            let ifnode=&stmt.ifnode.as_ref().unwrap();
            let elseifnodes=stmt.elseifnodes.as_ref();
            let elsenode=stmt.elsenode.as_ref();
            let (condcode, condexprvarname, _)=translate_expr(&ifnode.condition, alloced_tmpvar_num)?;
            // condition expr code
            intercodes.extend(condcode);
            // compare with 0
            intercodes.push(IntermediateCode::compare(&condexprvarname, "0"));
            // free condition expr temp var
            intercodes.push(IntermediateCode::free(&condexprvarname));
            // jump if equal to 0 (false)
            let ifcond_false_label=format!("else{}",alloc_global_id());
            intercodes.push(IntermediateCode::je(&ifcond_false_label));
            // now put the if body
            let (ifcode,_)= translate_scope(&ifnode.stmts, alloced_tmpvar_num, symbols)?;
            intercodes.extend(ifcode);
            // here comes the "if" not true part
            intercodes.push(IntermediateCode::label(&ifcond_false_label));
            // now deal with multiple else-if
            if let Some(elseifnodes) = elseifnodes {
                for elifnode in elseifnodes.iter() {
                    // condition
                    let (elif_condcode, elif_condvar, _)=translate_expr(&elifnode.condition, alloced_tmpvar_num)?;
                    intercodes.extend(elif_condcode);
                    // cmp
                    intercodes.push(IntermediateCode::compare(&elif_condvar, "0"));
                    // free tmpvar
                    intercodes.push(IntermediateCode::free(&elif_condvar));
                    // je
                    let elseif_false_label=format!("else{}",alloc_global_id());
                    intercodes.push(IntermediateCode::je(&elseif_false_label));
                    // put the body
                    let (elseifcode,_)=translate_scope(&elifnode.stmts, alloced_tmpvar_num, symbols)?;
                    intercodes.extend(elseifcode);
                    //put the false label
                    intercodes.push(IntermediateCode::label(&elseif_false_label));
                }
            }
            // now the else
            if let Some(elsenode) = elsenode{
                let (elsecode,_)=translate_scope(&elsenode.stmts, alloced_tmpvar_num, symbols)?;
                intercodes.extend(elsecode);
            }
            return Some((intercodes, tmpnamec));
        },
        StatementType::Return=>{
            // translate the expression
            let (expr_codes, expr_var, expr_tmpnum)=translate_expr(&stmt.expr, alloced_tmpvar_num + tmpnamec)?;
            tmpnamec+=expr_tmpnum;
            intercodes.extend(expr_codes);
            // generate ret code
            intercodes.push(IntermediateCode::new(IntermediateCodeType::Ret, vec![expr_var.clone()]));
            // free the temporary variables used in expression
            if stmt.expr.nodetype!=ExprNodeType::VALUE {
                intercodes.push(IntermediateCode::free(&expr_var));
            }
            return Some((intercodes, tmpnamec));
        },
        StatementType::While=>{
            // similar to if 
            let (condcode, condexprvarname, _)=translate_expr(&stmt.expr, alloced_tmpvar_num)?;
            // put the while start label for repeating
            // each loop we calc the expr value and check if it's 0
            let whilestart_label=format!("whilestart{}",alloc_global_id());
            intercodes.push(IntermediateCode::label(&whilestart_label));
            // condition expr code
            intercodes.extend(condcode);
            // compare with 0
            intercodes.push(IntermediateCode::compare(&condexprvarname, "0"));
            // free condition expr temp var
            intercodes.push(IntermediateCode::free(&condexprvarname));
            // jump if equal to 0 (false)
            let whilecond_false_label=format!("whilend{}",alloc_global_id());
            intercodes.push(IntermediateCode::je(&whilecond_false_label));
            // now put the while body
            let (whilecode,_)= translate_scope(&stmt.body, alloced_tmpvar_num, symbols)?;
            intercodes.extend(whilecode);
            // jmp back to start
            intercodes.push(IntermediateCode::jmp(&whilestart_label));
            // here comes the "if" not true part
            intercodes.push(IntermediateCode::label(&whilecond_false_label));

            return Some((intercodes,tmpnamec));
        }
        _=>{}
    }
    None
}
/// translate a scope (a block of statements) to intermediate code
/// return None if translation fails
/// # Arguments
/// * `stmts` - vector of statement nodes in the scope
/// * `alloced_tmpvar_num` - number of temporary variables already allocated, used to generate new temporary variable names
/// * `symbols` - mutable reference to the symbol table
/// # Returns
/// Option containing a tuple of vector of intermediate codes and the number of temporary variables allocated in this
fn translate_scope(stmts:&Vec<StatementNode>, alloced_tmpvar_num: usize, symbols:&mut Vec<Symbol>)->Option<(Vec<IntermediateCode>, usize)>{
    let mut tmpnamec=0;
    let mut intercodes:Vec<IntermediateCode>=Vec::new();
    intercodes.push(IntermediateCode::scope_start());
    let mut scope_vars=vec![];
    // first collect local var defs in the scope to prepare free local code
    // in case we have return. we need to free locals before returning

    for stmt in stmts {
        // if stmt is definition, collect it
        if stmt.stmttype==StatementType::Definition {
            scope_vars.push(stmt.id.value.clone());
        }
    }
    let mut free_intercodes=vec![];
    // push FREE to free ALLOCed local vars collected
    for varname in scope_vars.iter() {
        free_intercodes.push(IntermediateCode::free(varname));
    }

    for stmt in stmts {
        let (stmt_codes, stmt_tmpnum)=translate_stmt(stmt, alloced_tmpvar_num + tmpnamec, symbols)?;
        tmpnamec+=stmt_tmpnum;
        if stmt.stmttype==StatementType::Return {
            // insert local-freeing code first
            intercodes.extend(free_intercodes.clone());
        }
        intercodes.extend(stmt_codes);
    }
    // insert local-freeing code at the end of the scope
    intercodes.extend(free_intercodes.clone());
    intercodes.push(IntermediateCode::scope_end());
    Some((intercodes, tmpnamec))
}
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolType{
    Variable,
    Function,
}
#[derive(Debug, Clone)]
pub struct Symbol{
    pub name:String,
    pub vartype:SymbolType,
    pub global:bool,
    pub modifiers:Vec<String>,
    pub size:usize,
    /// if local, address is offset from base pointer
    /// if global, address is absolute address
    pub address:usize,
}
impl Symbol {
    pub fn new(name:String, vartype:SymbolType, global:bool, modifiers:Vec<String>, size:usize, address:usize)->Self{
        Symbol{
            name,
            vartype,
            global,
            modifiers,
            size,
            address,
        }
    }
}
fn translate_function(funcnode:&FunctionNode,alloced_tmpvar_num: usize, symbols:&Vec<Symbol>)->Option<(Vec<IntermediateCode>,usize)>{
    let mut intercodes:Vec<IntermediateCode>=Vec::new();
    let statements=&funcnode.stmts;
    let mut tmpvarc=0;
    // local symbol table
    let mut local_symbols:Vec<Symbol>=symbols.clone();
    // TODO: determine return value size based on function return type
    let retv_size="8".to_string();
    //generate argument list string
    let arglist_str={
        let mut args=String::new();
        for (i,(paramid, _paramtype)) in funcnode.params.iter().enumerate() {
            args.push_str(&paramid.value);
            args.push_str(":");
            // TODO: determine value size based on function return type
            args.push_str("8");
            if i!=funcnode.params.len()-1 {
                args.push_str(",");
            }
        }
        args
    };
    // func definition code
    intercodes.push(IntermediateCode::new(IntermediateCodeType::FuncDef, vec![funcnode.id.value.clone(), retv_size, arglist_str]));
    let (bodycodes, usedtmpvarc)=translate_scope(&funcnode.stmts, alloced_tmpvar_num+tmpvarc, &mut local_symbols)?;
    intercodes.extend(bodycodes);

    // insert an empty return to make sure the function can return
    intercodes.push(IntermediateCode::new(IntermediateCodeType::Retemp, vec![]));
    tmpvarc+=usedtmpvarc;
    Some((intercodes, tmpvarc))
}
pub fn generate_intermediate_code(ast:&ASTNode)->Option<Vec<IntermediateCode>>{
    let mut intercodes:Vec<IntermediateCode>=Vec::new();
    let mut symbols:Vec<Symbol>=Vec::new();
    for funcnode in ast.functions.iter() {
        let (func_codes, _func_tmpnum)=translate_function(funcnode, 0, &symbols)?;
        intercodes.extend(func_codes);
    }
    Some(intercodes)
}
#[test]
fn test_generate_itermediate_code(){
    let source="
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