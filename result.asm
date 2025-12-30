section .text
global main
main:
push rbp
 mov rbp, rsp
sub rsp, 0
push rbp
 mov rbp, rsp
sub rsp, 24
a equ [rbp - 8]
b equ [rbp - 16]
c equ [rbp - 24]
    mov a, 1
    mov b, 2
    mov c, 0
__tmp__2 equ rax
    mov __tmp__2, a
    add __tmp__2, b
    mov c, __tmp__2
__tmp__1 equ rax
    mov __tmp__1, a
    sub __tmp__1, 1
    cmp __tmp__1, 0
    je .eqzero0
    mov __tmp__1, 1
    jmp .endeq1
.eqzero0:
    mov __tmp__1, 0
.endeq1:
    cmp __tmp__1, 0
    je .else2
push rbp
 mov rbp, rsp
sub rsp, 8
d equ [rbp - 8]
    mov d, 11
__tmp__3 equ rax
    mov __tmp__3, d
    add __tmp__3, 1
    mov d, __tmp__3
    mov rsp,rbp
    pop rbp
.else2:
__tmp__1 equ rax
    mov __tmp__1, b
    sub __tmp__1, 2
    cmp __tmp__1, 0
    je .eqzero3
    mov __tmp__1, 1
    jmp .endeq4
.eqzero3:
    mov __tmp__1, 0
.endeq4:
    cmp __tmp__1, 0
    je .else5
push rbp
 mov rbp, rsp
sub rsp, 16
e equ [rbp - 8]
d equ [rbp - 16]
    mov e, 2
    mov d, 1
    mov rsp,rbp
    pop rbp
.else5:
push rbp
 mov rbp, rsp
sub rsp, 16
f equ [rbp - 8]
i equ [rbp - 16]
    mov f, 12
    mov rsp,rbp
    pop rbp
    mov i, 0
.whilestart8:
__tmp__1 equ rax
    mov __tmp__1, i
    sub __tmp__1, 10
    cmp __tmp__1, 0
    jnb .eqzero6
    mov __tmp__1, 1
    jmp .endeq7
.eqzero6:
    mov __tmp__1, 0
.endeq7:
    cmp __tmp__1, 0
    je .whilend9
push rbp
 mov rbp, rsp
sub rsp, 0
__tmp__3 equ rax
    mov __tmp__3, i
    add __tmp__3, 1
    mov i, __tmp__3
    mov rsp,rbp
    pop rbp
    jmp .whilestart8
.whilend9:
    push rdi
    mov rdi,c
    call printf
    pop rdi
__tmp__1 equ rax
    mov rax, 0
    mov rsp,rbp
    pop rbp
    ret
    mov rsp,rbp
    pop rbp
    mov rsp,rbp
    pop rbp
    ret
