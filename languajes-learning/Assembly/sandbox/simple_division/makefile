
args: args.o
	ld -m elf_i386 -o args args.o   # i386
	# ld -o args args.o   # x64

args.o: args.asm
	nasm -Wall -f elf args.asm -l args.lst   # i386
	# nasm -Wall -f elf64 args.asm -l args.lst   # x64
	# nasm -Wall -dDebug -f elf -g -F stabs args.asm -l args.lst   # i386
	# nasm -Wall -dDebug -f elf64 -g -F stabs args.asm -l args.lst   # x64

clean:
	rm args.o args

