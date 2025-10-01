CFLAGS =	-march=rv32im_zicsr -mabi=ilp32 -fno-builtin \
					-ffreestanding -nostdlib -O2
CC = riscv32-none-elf-gcc
LD = riscv32-none-elf-ld

compile:
	$(CC) $(CFLAGS) -Tsrc/linker.ld -Isrc \
		-o build/gpu_firmware.elf src/*.c src/*.s -lgcc
