riscv64-unknown-elf-gcc -O -fno-common -fno-builtin-printf -specs=htif.specs -c $1.c -lm
riscv64-unknown-elf-gcc -static -specs=htif.specs $1.o -o $1.riscv -lm 

