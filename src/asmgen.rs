use core::slice;
use std::{collections::BTreeMap, iter::zip};

use crate::{errs::{ERR_NASMGEN, cry_err}, intercode::{IntermediateCode, IntermediateCodeType, Symbol}};
type SymbolTable=BTreeMap<String, Symbol>;
/// generate nasm code for a scope from intermediate code and symbol table
fn genasm_scope(intercodes:&mut slice::Iter<IntermediateCode>, symbols:&mut SymbolTable)->Option<String>{
    let mut nasmcode=String::new();
    // collect ALLOC local
    let mut local_allocs:Vec<&IntermediateCode>=vec![];
    let mut alloced_stack_size=0; //预留栈空间
    let mut symbols=symbols.clone();
    // scan for it 
    let mut codeiter=intercodes.clone();
    loop {
        let code=codeiter.next();
        if code.is_none() {
            break;
        }
        let mut code=code.unwrap();
        if code.code_type==IntermediateCodeType::ScopeStart {
            // deeper scope, skip. we should not intervene vars defined inside
            let mut lvl=0;
            loop {
                match code.code_type {
                    IntermediateCodeType::ScopeStart=>lvl+=1,
                    IntermediateCodeType::ScopeEnd=>lvl-=1,
                    _=>{}
                }
                let codeu=codeiter.next();
                if lvl==0||codeu.is_none() {
                    break;
                }
                code=codeu.unwrap();
            }
            if lvl!=0 {
                // error: scope not matching
                cry_err("Asm Generator", "the scope intercodes do not match", 0, 0);
                return None;
            }
            
        }else if code.code_type==IntermediateCodeType::AllocLocal {
            alloced_stack_size+=code.operands[1].parse::<usize>().unwrap_or(0);
            local_allocs.push(code);
        }
    }
    // alloc stack addr for each sym and insert it into the symtab
    // gen equ macro for locals
    // first store the rbp
    nasmcode.push_str("push rbp\n mov rbp, rsp\n");
    // then alloc stack space
    nasmcode.push_str(&format!("sub rsp, {}\n", alloced_stack_size));
    let mut alloc_ptr=0;
    for localvar in local_allocs.iter() {
        let varname=&localvar.operands[0];
        let varsize=localvar.operands[1].parse::<usize>().unwrap_or(0);
        // calculate address
        alloc_ptr+=varsize;
        let varaddr=alloc_ptr;
        // insert into symtab
        let symbol=Symbol::new(
            varname.clone(),
            crate::intercode::SymbolType::Variable,
            false,
            vec![],
            varsize,
            varaddr,
        );
        symbols.insert(varname.clone(), symbol);
        // gen equ macro
        nasmcode.push_str(&format!("{} equ [rbp - {}]\n", varname, varaddr));
    }

    // make a table for temps to assign them to registers
    let mut temp_table=BTreeMap::<String, String>::new();
    let temp_regs=["rax", "rbx", "r10", "r11"];
    // then normally generate code
    while let Some(ic)=intercodes.next() {
        match &ic.code_type {
            IntermediateCodeType::FuncDef=>{
                nasmcode.push_str(&format!("global {}\n{}:\n", ic.operands[0], ic.operands[0]));
            },
            IntermediateCodeType::AllocLocal=>{
                // skip, already handled
                continue;  
                // nasmcode.push_str(&format!("    sub rsp, {}\n", ic.operands[1]));
                // alloced_stack_size+=ic.operands[1].parse::<usize>().unwrap_or(0);
            },
            IntermediateCodeType::AllocTemp=>{
                let tempname=&ic.operands[0];
                // assign a register
                if temp_table.len()>=temp_regs.len() {
                    cry_err(ERR_NASMGEN, "too many temps used simultaneously, spilling not supported yet", 0, 0);
                    return None;
                }
                let reg=temp_regs[temp_table.len()].to_string();
                temp_table.insert(tempname.clone(), reg.clone());
                nasmcode.push_str(&format!("{} equ {}\n", tempname, reg));
            },
            IntermediateCodeType::Free=>{
                // if temp
                let varname=&ic.operands[0];
                if temp_table.contains_key(varname) {
                    temp_table.remove(varname);
                }
                // for locals, we dont need to cope with it because we will free it anyway
                // at the end of the scope
            },
            IntermediateCodeType::Mov=>{
                nasmcode.push_str(&format!("    mov {}, {}\n", ic.operands[0], ic.operands[1]));
            },
            IntermediateCodeType::Add=>{
                nasmcode.push_str(&format!("    add {}, {}\n", ic.operands[0], ic.operands[1]));
            },
            IntermediateCodeType::Sub=>{
                nasmcode.push_str(&format!("    sub {}, {}\n", ic.operands[0], ic.operands[1]));
            },
            IntermediateCodeType::Mul=>{
                nasmcode.push_str(&format!("    imul {}, {}\n", ic.operands[0], ic.operands[1]));
            },
            IntermediateCodeType::Div=>{
                nasmcode.push_str(&format!("    push rdx\n    push rax\n    xor rdx, rdx\n    mov rax, {}\n    div {}\n    mov {}, rax\n    pop rax\n    pop rdx\n", ic.operands[0], ic.operands[1], ic.operands[0]));
            },
            IntermediateCodeType::Ret=>{
                nasmcode.push_str(&format!("    mov rax, {}\n    mov rsp,rbp\n    pop rbp\n    ret\n", ic.operands[0]));
            },
            IntermediateCodeType::Retemp=>{
                nasmcode.push_str(&format!("    mov rsp,rbp\n    pop rbp\n    ret\n"));
            },
            IntermediateCodeType::Cmp=>{
                nasmcode.push_str(&format!("    cmp {}, {}\n", ic.operands[0], ic.operands[1]));
            },
            IntermediateCodeType::Je=>{
                nasmcode.push_str(&format!("    je {}\n", ic.operands[0]));
            },
            IntermediateCodeType::Ja=>{
                nasmcode.push_str(&format!("    ja {}\n", ic.operands[0]));
            },
            IntermediateCodeType::Jb=>{
                nasmcode.push_str(&format!("    jb {}\n", ic.operands[0]));
            },
            IntermediateCodeType::Jmp=>{
                nasmcode.push_str(&format!("    jmp {}\n", ic.operands[0]));
            },
            IntermediateCodeType::Label=>{
                nasmcode.push_str(&format!("{}:\n", ic.operands[0]));
            },
            IntermediateCodeType::FuncCall=>{
                // set params
                let args=ic.get_funccall_args().unwrap();
                let reg_args=["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                if args.len()>6 {
                    cry_err(ERR_NASMGEN, "function call arguments more than 6 is currently unsupported", 0, 0);
                    return None;
                }
                // store stack
                for ((arg,_),reg) in zip(args.iter(),reg_args.iter()) {
                    nasmcode.push_str(&format!("    push {}\n    mov {},{}\n", reg, reg, arg));
                }
                nasmcode.push_str(&format!("    call {}\n", ic.operands[0]));
                // restore stack
                for reg in reg_args.iter().take(args.len()).rev() {
                    nasmcode.push_str(&format!("    pop {}\n", reg));
                }
            },
            IntermediateCodeType::ScopeStart=>{
                if let Some(scope_code)=genasm_scope(intercodes, &mut symbols){
                    nasmcode.push_str(&scope_code);
                }else {
                    cry_err(ERR_NASMGEN, "failed to gen asm for scopr", 0, 0);
                    return None;
                }
            },
            IntermediateCodeType::ScopeEnd=>{
                // done with this scope
                // free the stack alloced for local vars
                nasmcode.push_str(&format!("    mov rsp,rbp\n    pop rbp\n"));
                break;
            }
            _=>{}
        }
    }
    Some(nasmcode)
}
/// generate nasm code from intermediate code
/// * this is a toy implementation with out any symbol table operation
pub fn generate_nasm(intercodes:&Vec<IntermediateCode>)->Option<String>{
    let mut nasmcode=String::new();
    let mut symbols=SymbolTable::new();
    nasmcode.push_str("section .text\n");
    let mut codeiter=intercodes.iter();
    while let Some(ic)=codeiter.next() {
        match &ic.code_type {
            IntermediateCodeType::FuncDef=>{
                nasmcode.push_str(&format!("global {}\n{}:\n", ic.operands[0], ic.operands[0]));
                // translate the content after the func def
                if let Some(scope_code)=genasm_scope(&mut codeiter, &mut symbols){
                    nasmcode.push_str(&scope_code);
                }else {
                    cry_err(ERR_NASMGEN, "failed to gen asm for scope", 0, 0);
                    return None;
                }
            },
            IntermediateCodeType::Retemp=>{
                // this is to deal with the retemp following the scopeend of the func
                nasmcode.push_str(&format!("    mov rsp,rbp\n    pop rbp\n    ret\n"));
            },
            _=>{
                cry_err(ERR_NASMGEN, "other type of intercode on global level not supported yet", 0, 0);
            }
        }
    }
    Some(nasmcode)
}