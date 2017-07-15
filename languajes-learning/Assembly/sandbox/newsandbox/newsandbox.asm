; 	%idefine mov %?           ; Not USE
; 	%idefine mov add          ; NEVER

SECTION .data

	%define OO      4-isTrue
	%define isTrue  OO
	%xdefine isFalse isTrue
	%define OO      5
	%define isTrue  0

	val1:    db      isFalse

	%define OO      6
	%define isTrue  1
	
	%assign Oao 5

	val2:    db      isFalse

	db __BITS__

	db Oao

	%substr mychar 'xyzw' 2,-1
	db mychar

	Hi db `0xa\n`
	do "0xa"
	O_ol equ 'O_O"'
	db 2

	buffer: db      'hello, world' 
			times 64-$+buffer db ' '

	mov ax,05
	
	db __utf32__('C:\WINaoeuaoeDOWS'), 0
	dq    0x1.2p+32
	dt    3.141592653589793238462
	
	dq    +1.5,__Infinity__

	db "Here"
	dd 0.1
	dd 0
	dw 05f87h

SECTION .bss

	times 100 resb 1

SECTION .text

global _start

_start:
	nop

	push 0x12345678
	push 0x9abcdef0

	mov [256],word 1452

	pop ax
	pop bx

	pushfd
	pop eax
	xor eax,0010_0000_0000b
	push eax
	popfd

	pushf
	pushfd

	mov eax,$
	
	mov eax,$$
	
	mov eax, dword [buffer]

	int 0x10

	mov eax,0x1
	neg eax

	mov eax,0x0
	neg eax

	mov eax,0x0
; 	cmovz edx,0x0
	mov ebx,0xff
	div ebx

	mov eax,0xffffffff
	mov edx,0x1
	mov ebx,0x2
	div ebx

	mov eax,0xffffffff
	mov ebx,0x3b78
	mul ebx

 
	mov     ax,200          ; decimal 
	mov     ax,0200         ; still decimal 
	mov     ax,0200d        ; explicitly decimal 
	mov     ax,0d200        ; also decimal 
	mov     ax,0c8h         ; hex 
	mov     ax,$0c8         ; hex again: the 0 is required 
	mov     ax,0xc8         ; hex yet again 
	mov     ax,0hc8         ; still hex 
	mov     ax,310q         ; octal 
	mov     ax,310o         ; octal again 
	mov     ax,0o310        ; octal yet again 
	mov     ax,0q310        ; octal yet again 
	mov     ax,11001000b    ; binary 
	mov     ax,1100_1000b   ; same binary constant 
	mov     ax,1100_1000y   ; same binary constant once more 
	mov     ax,0b1100_1000  ; same binary constant yet again 
	mov     ax,0y1100_1000  ; same binary constant yet again

; 	mov    [buffer], tword __float64__(3.141592653589793238462)

	dt 312_345_678_901_245_678p 
	dt -12_345_678_901_245_678p 
	dt +0p33 
	dt 33p
	
	db 5<<3
	dd -5
	dd ~5
	dd !0x00000

	JMP $

	mov eax,$$
 
	%macro mpar 1-*
		db %{3:5}
	%endmacro

	mpar 1,2,3,4,5,6

	nop
