global asm_func

;----------------------------------------
; DATA
;----------------------------------------
SECTION .data       

a:      dd  5       

;----------------------------------------
; CODE
;----------------------------------------
SECTION .text                   

test_call:
    mov eax, dword [esp+4]
    mov dword [a], eax
    mov eax, 2
    cmp dword [a], 1
    jne lol
    inc eax
    ret
lol:
    dec eax
    ret

tainted_write:
    mov dword [edx], 0
    ret

tainted_call:
    call [edx+0x40]
    ret

asm_func:
    call foo
    push 29ah
    ret
foo:
    mov esi, [esp+8]
    xor eax, eax
    lodsb
    xor edi, edi
    add edi, eax
    sub edi, 0x30
    pop eax
    push eax
    mov ecx, eax
    mov ebx, 2
    mov edx, dword [edi+0x11223344]
    ;push edx
    ;call test_call
    mov esi, 0
    ;rep movsb
    mov ebx, dword [a]
    cmp ebx, 5
    jz good
    nop
good:
    cmp edx, 1
    jnz nxt
eax1:
    inc eax
    mov eax, do_ret
    jmp eax
do_ret:
    call tainted_write
    ret
nxt:
    mov ecx, edx
    mov ebx, 1
    cmp ebx, 1
    jz bar
    cmp ebx, 2
    jz inf
    mov dword [eax], 1
bar:
    cmp edx, 3
    jz inf
    call tainted_call
inf:

    ret
