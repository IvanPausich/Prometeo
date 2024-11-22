section .data

section .bss
i resd 1
arr resd 1
stack_bottom:
    resb 16384  ; 16 KB stack
stack_top:

section .text
BITS 32
section .text
global _start
_start:
    mov esp, stack_top
    mov ebp, esp
; Public Module TestModule
section .text
main:
    push ebp
    mov ebp, esp
    ; Initialize i for loop
    mov dword [i], 0
.for_i_0:
    ; Compare i with end value
    mov eax, [i]
    cmp eax, 9
    jg .endfor_i_0
    ; Array assignment to arr
    xor eax, eax
    add eax, i
    lea ebx, [arr]
    add ebx, eax
    mov eax, [i]
    mov [ebx], eax
    ; Increment i
    add dword [i], 1
    jmp .for_i_0
.endfor_i_0:
    mov esp, ebp
    pop ebp
    ret
; End of module
