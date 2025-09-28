


compile:
	riscv32-none-elf-gcc -I./src/ -march=rv32i_zicsr -mabi=ilp32 \
		-nostdlib -ffreestanding -Tsrc/linker.ld \
		-o build/gpu_firmware.elf src/*.c src/*.s
