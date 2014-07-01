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

exploit_me:
    mov ecx, dword [esp+12]
    mov ebx, dword [esp+8]
    mov eax, dword [esp+4]
    
    mov edx, ebx
    and edx, 0ffh

    cmp eax, 1
    je d1
    cmp eax, 2
    je d2
    cmp eax, 3
    je d3
    cmp eax, 4
    je d4
    jmp die
    
d1:
    cmp edx, 0x11
    jne null
    jmp recurse

d2:
    cmp edx, 0x22
    jne null
    jmp recurse

d3:
    cmp edx, 0x33
    jne null
    jmp recurse

d4:
    cmp edx, 0x44
    jne die

    mov dword [esp+ecx], 29ah ;BOOM!
    jmp die

null:
    mov ecx, 0

recurse:
    shr ebx, 8
    push ecx
    push ebx
    inc eax
    push eax
    call exploit_me
die:
    ret

asm_func:
    mov esi, dword [esp+4]
    lodsd
    mov ebx, dword [eax]
    lodsd
    mov ecx, dword [eax]

    push ecx
    push ebx
    push 1
    call exploit_me

inf:
    jmp inf
    ret
