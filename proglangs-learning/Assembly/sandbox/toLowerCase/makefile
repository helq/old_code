
toLowerCase: toLowerCase.o
	ld -m elf_i386 -o toLowerCase toLowerCase.o   # i386
# 	ld -o toLowerCase toLowerCase.o   # x64

toLowerCase.o: toLowerCase.asm
	nasm -f elf -g -F stabs toLowerCase.asm -l toLowerCase.lst   # i386
# 	nasm -f elf64 -g -F stabs toLowerCase.asm -l toLowerCase.lst   # x64

clean-all:
	rm toLowerCase.o toLowerCase

# newtoLowerCase: newtoLowerCase.o
# 	ld -m elf_i386 -o newtoLowerCase newtoLowerCase.o   # i386
# # 	ld -o newtoLowerCase newtoLowerCase.o   # x64
# 
# newtoLowerCase.o: newtoLowerCase.asm
# 	nasm -f elf -g -F stabs newtoLowerCase.asm -l newtoLowerCase.lst   # i386
# # 	nasm -f elf64 -g -F stabs newtoLowerCase.asm -l newtoLowerCase.lst   # x64
