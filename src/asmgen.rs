use crate::intercode::{IntermediateCode, IntermediateCodeType};

/// generate nasm code from intermediate code
/// * this is a toy implementation with out any symbol table operation
pub fn generate_nasm(intercodes:&Vec<IntermediateCode>)->Option<String>{
    let mut nasmcode=String::new();
    nasmcode.push_str("section .text\n");
    let mut alloced_stack_size=0; //预留栈空间
    for ic in intercodes.iter() {
        match &ic.code_type {
            IntermediateCodeType::FuncDef=>{
                nasmcode.push_str(&format!("global {}\n{}:\n", ic.operands[0], ic.operands[0]));
            },
            IntermediateCodeType::AllocLocal=>{
                // allocate space on stack
                nasmcode.push_str(&format!("    sub rsp, {}\n", ic.operands[1]));
                alloced_stack_size+=ic.operands[1].parse::<usize>().unwrap_or(0);
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
                nasmcode.push_str(&format!("    xor rdx, rdx\n    mov rax, {}\n    div {}\n    mov {}, rax\n", ic.operands[0], ic.operands[1], ic.operands[0]));
            },
            IntermediateCodeType::Ret=>{
                nasmcode.push_str(&format!("    mov rax, {}\n    add rsp, {}\n    ret\n", alloced_stack_size, ic.operands[1]));
            },
            IntermediateCodeType::Retemp=>{
                nasmcode.push_str(&format!("    add rsp, {}\n    ret\n", alloced_stack_size));
            },
            _=>{}
        }
    }
    Some(nasmcode)
}