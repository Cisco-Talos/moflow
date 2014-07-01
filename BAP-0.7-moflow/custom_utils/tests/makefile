all:
	nasm -f elf test.asm -o test.asm.o
	gcc -m32 -c test.c -o test.c.o
	gcc -m32 test.c.o test.asm.o -o crash

clean:
	rm -f crash *.o
