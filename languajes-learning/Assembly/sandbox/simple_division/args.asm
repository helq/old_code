;  Executable name : args
;  Version         : 0.1
;  Created date    : 30/05/2013
;  Last update     : 01/06/2013
;  Author          : helq
;  Description     : A simple program in assembly for Linux,
;                    using NASM 2.10.07, printing the Quot and Rem of two
;                    numbers in argv
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
    mov eax,0x4                ; Specify syl_write call
    mov ebx,0x1                ; Specify File Descriptor 1: Standard Output
    mov ecx,%1                 ; Pass offset of the message
    mov edx,%2                 ; Pass the length of the message
    int 0x80                   ; Make kernel call
%endmacro

%macro PutInt 1
    mov eax,%1                 ; number to print
    mov ecx,end_tmpStr         ; resgister position to save the number
    call intToStr              ; conversion
    mov edx,end_tmpStr         ; <-
    sub edx,ecx                ; determining the size of the string
    inc edx                    ; <-
    PutStr ecx,edx             ; printing number
%endmacro

; ==================   INPUT   ==================
%macro GetStr 2
    mov eax,0x3                ; Specify syl_read call
    mov ebx,0x0                ; Specify File Descriptor 0: Standard Input
    mov ecx,%1                 ; Pass offset of the message
    mov edx,%2                 ; Pass the length of the message
    int 0x80                   ; Make kernel call
%endmacro

SECTION .data
    space:            db " ",0h,0x0
    endl:             db 0Ah,0x0

    msg_address1:     db "The address for ",0x0
    l_msg_address1:   equ  $-msg_address1-1
    msg_address2:     db " is:",0Ah,0x0
    l_msg_address2:   equ  $-msg_address2-1

    msg_address3:     db "Page number: ",0x0
    l_msg_address3:   equ  $-msg_address3-1

    msg_address4:     db 0Ah,"Alignment: ",0x0
    l_msg_address4:   equ  $-msg_address4-1

;   ------------- error mesages --------------

    incorrect:
            db "Incorrect number of parameters",0Ah
            db "   use: args <pages> <register>",0Ah,0Ah
            db " pages must be powers of 2 between 1024 and 16384",0Ah
            db " a register must be an positive integer of 32bits",0Ah,0x0
    l_incorrect:      equ  $-incorrect-1

    invalid_number:
            db "Invalid number, only positive integers",0Ah,0x0
    l_invalid_number: equ  $-invalid_number-1

    large_number:
            db "Invalid number, very large number, number < 2^32",0Ah,0x0
    l_large_number:   equ  $-large_number-1

    page_size:
            db "Invalid size of page, page must be a power of 2 between "
            db "1024 and 16384",0Ah,0x0
    l_page_size:   equ  $-page_size-1


SECTION .bss
    sizeOfPage:       resw   1h
    offset:           resw   1h

    tmp_intString:    resb   0Bh
    end_tmpStr:         equ    $-1

SECTION .text

global _start

_start:
    nopDebug
    jmp main

; ==================   strToInt   ==================
strToInt:
    ; params: ecx -> offset
    ;         return int -> eax
    ; using: eax, ebx, ecx, edx
    mov eax,0x0                 ; eax = 0
    mov ebx,0x0                 ; for convert a char into a decimal

    .quitspaces:                ; ignore the spaces routine
        cmp byte [ecx],' '      ; compare character with space (' ')
        jnz .convert            ; jump to .convert if the current number != ' '
        inc ecx                 ; next position of the string
        cmp byte [ecx],0x0      ; compare with caracter '\0'
        jz  error_invalid_number; error if there no more characters
        jmp .quitspaces         ; repeat the loop

    .convert:
        mov bl,[ecx]            ; character => bl

        sub bl,'0'              ; bl <- bl - '0'
        jb  .endSpaces          ; if bl - '0' < 0 veryfi bl == ' '

        cmp bl,0xa              ; if 9 - bl < 0
        jae error_invalid_number;   then error

        mov edx,0xa             ; edx <- 10
        mul edx                 ; edx:eax <- eax*edx == eax*10
        cmp edx,0x0             ; if edx != 0 (i.e.: eax*edx > 2^32-1)
        jnz error_large_number  ;    then error
        add eax,ebx             ; eax <- eax + bl == eax + int(character)
        jc  error_large_number  ; if adding cause a carry
        inc ecx                 ; next character
        cmp byte [ecx],0x0      ; compare with caracter '\0'
        jz  .end                ; if esi == 0 then end of the string
        jmp .convert            ; else repeat

    .endSpaces:                 ; ignore end-spaces
        cmp byte [ecx],' '      ; compare character with space (' ')
        jnz error_invalid_number; error if the character isn't ' '
        inc ecx                 ; next position of the string
        cmp byte [ecx],0x0      ; compare with caracter '\0'
        jz  .end
        jmp .endSpaces          ; repeat the loop
    
    .end:
        ret

; ==================   intToStr   ==================
intToStr:
    ; param dword in: eax
    ;       end of the offset: ecx
    ; return start of the offset: ecx
    ; used registers: eax, ebx, ecx, edx
    cmp eax,0x0                 ; if eax != 0
    jnz .loop                   ; then goto .loop
    mov byte [ecx],'0'          ; else put 0 in the string
    jmp .end

    .loop:                      ; put decimal to decimal
        xor edx,edx             ; edx <- 0
        mov ebx,0Ah             ; ebx <- 10
        div ebx                 ; eax <- edx:eax / ebx; edx <- edx:eax % ebx; 
        add dl,'0'              ; dl (Rem) + '0'
        mov [ecx],dl            ; save dl int in [ecx]
        cmp eax,0x0             ; if eax == 0
        jz  .end                ; then finish
        dec ecx                 ; move forward

        jmp .loop

    .end:
        ret

; ==================   check page size number   ==================
checkSize:
    ; param: eax
    ; check if is power of 2 between 1024-16384
    mov ebx,0x400               ; ebx <- 1024

    mov ecx,5                   ; some like for i in [1..5]
    .loop:
        cmp eax,ebx             ; if eax == ebx
        jz  .end                ; then eax is correct
        shl ebx,1               ; else shift to left (equiv. *2)
      loop .loop                ; repeat

    jmp error_page_size         ; page size invalid, error

    .end:
        ret

; ==================   main   ==================
main:
    pop esi                     ; argc
    cmp esi,0x3                 ; if argc != 3
    jnz error_num_args          ; then error

    pop ecx                     ; argv[0]

    pop ecx                     ; argv[1]
    call strToInt               ; eax <- int(argv[1])
    mov esi,eax                 ; esi <- eax (Size of page)

    call checkSize

    pop ecx                     ; argv[2]
    call strToInt               ; eax <- int(argv[2])
    mov edi,eax                 ; edi <- eax (Register)

    PutStr msg_address1,l_msg_address1
    ; PutInt esi
    PutInt edi
    PutStr msg_address2,l_msg_address2

    xor edx,edx
    mov eax,edi
    div esi

    mov esi,eax                 ; page
    mov edi,edx                 ; alignment

    PutStr msg_address3,l_msg_address3
    PutInt esi

    PutStr msg_address4,l_msg_address4
    PutInt edi

    PutStr endl,1

    jmp _exit

; ==================   EXIT errors   ==================

error_num_args:                 ; invalid number of arguments
    PutStr incorrect,l_incorrect
    jmp error

error_invalid_number:           ; isn't a number
    PutStr invalid_number,l_invalid_number
    jmp error

error_large_number:             ; number >= 2^32
    PutStr large_number,l_large_number
    jmp error

error_page_size:             ; number >= 2^32
    PutStr page_size,l_page_size
    jmp error


error:
    mov eax,0x1                ; Code for Exit Syscall
    mov ebx,0x1                ; Return a code of one
    int 0x80                   ; Make kernel call

; ==================   EXIT   ==================
_exit:
    mov eax,0x1                ; Code for Exit Syscall
    mov ebx,0x0                ; Return a code of zero
    int 0x80                   ; Make kernel call
