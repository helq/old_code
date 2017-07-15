%macro nwln 0
	mov eax,0x4
	mov ebx,0x1
	mov ecx,NewLine
	mov edx,0x1
	int 0x80
%endmacro

SECTION .data

	NewLine       db 0xA

	Snippet       db "KANGAROO"
	SnippetLen    equ $-Snippet

	TestDb        dw "Hi?"

	TestDd        db "Hi :P"
	TestDdLen     equ $-TestDd

	SaveBits      db 11111111b,1b
	SaveHex       dw 0ff01h
	SaveHex1      db 0FFH,01H

	float1        dd 1.234
	real1         dq 123.456

	marks         times 8 db 0f2h

; 	var1: db 05h
; 	message: db "O_o -> XD",0ah
; 	message2: db "Second message",0ah

SECTION .bss

	buffer        resw 100000

SECTION .text

global _start

_start:
	nop


; 	mov esi,[TestDb]
; 	mov esi,TestDb
; 	mov byte [esi],100
; 	inc dword [SaveHex]

	mov eax,0x0

	sub eax,1

	inc edi

do:
	inc al
	jmp do


	add eax,1
	sub eax,1
	inc eax
	dec eax

















	mov ecx,1
	mov ebx,TestDb
	mov eax,[ebx+ecx]
	mov eax,[TestDb+ecx]
	lea eax,[TestDb]
	mov ecx,TestDb
	mov esi,0x0
	
	mov ebx,[Snippet+esi]
	mov ebx,Snippet
	mov eax,[ebx+esi+3]

; 	mov eax,[ebx+ecx+edx]  ; Invalid Effective Address






; 
; ; --------------------------------------
; 
; 	mov eax,0x4                ; Specify sys_write call
; 	mov ebx,0x1                ; Specify File Descriptor 1: Standard Output
; 	mov ecx,Snippet            ; Pass offset of the message
; 	mov edx,SnippetLen         ; Pass the length of the message
; 	int 0x80                   ; Make kernel call
; 
; 	nwln                       ; New Line
; 
; 	mov ebx,Snippet            
; 	mov eax,SnippetLen
; DoMore:	add byte [ebx],32
; 	inc ebx
; 	dec eax
; 	jnz DoMore
; 
; 	mov eax,0x4                ; Specify sys_write call
; 	mov ebx,0x1                ; Specify File Descriptor 1: Standard Output
; 	mov ecx,Snippet            ; Pass offset of the message
; 	mov edx,SnippetLen         ; Pass the length of the message
; 	int 0x80                   ; Make kernel call
; 
; 	nwln                       ; New Line
; 
; 	mov eax,0x1                ; Code for Exit Syscall
; 	mov ebx,0x0                ; Return a code of zero
; 	int 0x80                   ; Make kernel call
; 
; ; --------------------------------------
; 

; 	dec byte [var1]
; 	jnz _start

; 	mov eax,0ff5h
; 	add eax,0bh
; 	add eax,1
; 	inc eax

; 	mov bx,0ffffh
; 	add bx,1

; 	mov dx,0ffffh
; 	dec dx

; 	mov eax,1
; 	add eax,2
; 	mov ebx,1
; 	add bl,0ffh
; 	mov ecx,eax

; 	mov eax,1
; 	mov ebx,1
; 	neg ebx
; 	add eax,ebx
; 	add eax,1
; 	mov eax,10b
; 	add eax,1

; 	mov edx,07FFFFFFFh
; 	add edx,1
; 	add edx,1
; 	add edx,0fffffffeh
; 	mov eax,80000000h
; 	mov ebx,7fffffffh

; 	mov edx,message
; 	mov [message2],byte 67h

; 	mov ax,067FEh
; 	mov bx,ax
; 	mov cl,bh
; 	mov ch,bl
; 
; 	xchg cl,ch
; 
; 	xor cl,ch
; 	xor ch,cl
; 	xor cl,ch
; 
; 	shr bx,1
; 
; 	mov eax,12345678h
; 	mov edx,'wxyz'
; 
; 	mov bx,ax
; 	add di,ax
; 	add di,ax
; 
; 	mov [ebp],edi




	nop
