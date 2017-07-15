
# sandbox: sandbox.o
# 	ld -m elf_i386 -o sandbox sandbox.o   # i386
# # 	ld -o sandbox sandbox.o   # x64
# 
# sandbox.o: sandbox.asm
# 	nasm -f elf -g -F stabs sandbox.asm -l sandbox.lst   # i386
# # 	nasm -f elf64 -g -F stabs sandbox.asm -l sandbox.lst   # x64

newsandbox: newsandbox.o
	ld -m elf_i386 -o newsandbox newsandbox.o   # i386
# 	ld -o newsandbox newsandbox.o   # x64

newsandbox.o: newsandbox.asm
	nasm -w+all -f elf -g -F stabs newsandbox.asm -l newsandbox.lst   # i386
# 	nasm -w+all -f elf64 -g -F stabs newsandbox.asm -l newsandbox.lst   # x64

clean:
	rm newsandbox

clean-all: clean
	rm newsandbox.o