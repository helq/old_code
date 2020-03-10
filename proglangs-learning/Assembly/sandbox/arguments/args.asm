;  Executable name : args
;  Version         : 0.1
;  Created date    : 07/02/2012
;  Last update     : 07/02/2012
;  Author          : helq
;  Description     : A simple program in assembly for Linux, using NASM 2.10,
;    printing the second argv.
;
;  Build using these commands:
;    nasm -f elf -g -F stabs args.asm -l args.lst
;    ld -m elf_i386 -o args args.o
;

; Use -dDEBUG option
%macro nopDebug 0
    %ifdef DEBUG 
        nop
    %endif
%endmacro

; ==================   PRINT   ==================
%macro PutStr 2
    mov eax,0x4                ; Specify sys_write call
    mov ebx,0x1                ; Specify File Descriptor 1: Standard Output
    mov ecx,%1                 ; Pass offset of the message
    mov edx,%2                 ; Pass the length of the message
    int 0x80                   ; Make kernel call
%endmacro

; ==================   INPUT   ==================
%macro GetStr 2
    mov eax,0x3                ; Specify sys_read call
    mov ebx,0x0                ; Specify File Descriptor 0: Standard Input
    mov ecx,%1                 ; Pass offset of the message
    mov edx,%2                 ; Pass the length of the message
    int 0x80                   ; Make kernel call
%endmacro

SECTION .data
    space:       db   " ",0h

SECTION .bss

SECTION .text

global _start

_start:
    nopDebug

    pop esi                    ; argc
    dec esi                    ; argc--
    pop edi                    ; argv[0]

    loop1:                     ; Trough argv (except the last argv)
        mov ecx,edi                ; argv[n] to ecx     ; offset of the message
        pop edi                    ; argv[n+1]
        mov edx,edi                ; copy edi to edx
        sub edx,ecx                ; length(argv[n])    ; length of the message

        mov eax,0x4                ; Specify sys_write call
        mov ebx,0x1                ; Specify File Descriptor 1: Standard Output
        int 0x80                   ; Make kernel call

        PutStr space,0x1           ; printing an space

        dec esi                    ; argc--
    jnz loop1

    pop esi                    ; empty, api linux
    pop esi                    ; next value on the stack
    sub esi,edi                ; length of argv[n]
    PutStr edi,esi

; ==================   EXIT   ==================
    mov eax,0x1                ; Code for Exit Syscall
    mov ebx,0x0                ; Return a code of zero
    int 0x80                   ; Make kernel call
