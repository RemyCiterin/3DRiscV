CFLAGS =	-march=rv32im_zicsr -mabi=ilp32 -fno-builtin \
					-ffreestanding -nostdlib -O2
CC = riscv32-none-elf-gcc
LD = riscv32-none-elf-ld

.PHONY: compile
compile:
	$(CC) $(CFLAGS) -Tsrc/linker.ld -Isrc \
		-o build/gpu_firmware.elf src/*.c src/*.s -lgcc

.PHONY: run
run: compile
	make -C .. test_gpu run

.PHONY: verilator
verilator: compile
	make -C .. test_gpu verilator
