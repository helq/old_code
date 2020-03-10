;  Executable name : toLowerCase
;  Version         : 0.1
;  Created date    : 07/02/2012
;  Last update     : 07/02/2012
;  Author          : helq
;  Description     : A simple program in assembly for Linux, using NASM 2.09.08,
;    changing from Uppercase to Lowercase characters input from consola.
;
;  Build using these commands:
;    nasm -f elf -g -F stabs toLowerCase.asm -l toLowerCase.lst
;    ld -m elf_i386 -o toLowerCase toLowerCase.o
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
	mov eax,0x3                ; Specify sys_write call
	mov ebx,0x0                ; Specify File Descriptor 0: Standard Input
	mov ecx,%1                 ; Pass offset of the message
	mov edx,%2                 ; Pass the length of the message
	int 0x80                   ; Make kernel call
%endmacro

SECTION .data

SECTION .bss

	String       resb 100H
	StringLen    equ $-String

SECTION .text

global _start

_start:
	nopDebug

	GetStr String,StringLen

; ==================   UPER TO LOWER CASE   ==================
	mov ecx,StringLen          ; Length of String in Counter Register
	mov eax,String             ; Offset of String in eax

	loop1:                     ; Trough a String
		mov bl,[eax]               ; IF
		 cmp bl,0x40               ;    char => 0x41
		 jng nothing               ;            "A"
		 cmp bl,0x5b               ;    char <= 0x5a
		 jnl nothing               ;            "Z"
             ;                     ; THEN
			add [eax],byte 0x20        ; Do character: Upper to Lower case
		nothing:                   ; FI
		inc eax                    ; Move to next character
	loop loop1

	PutStr String,StringLen

; ==================   EXIT   ==================
	mov eax,0x1                ; Code for Exit Syscall
	mov ebx,0x0                ; Return a code of zero
	int 0x80                   ; Make kernel call
