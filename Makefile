
sim:
	cabal run blarney-test -- --simulate

build:
	cabal run

test:
	riscv32-none-elf-objcopy -O ihex ../DOoOM/soft/zig-out/bin/zig-unix.elf Mem.ihex
	./ihex-to-img.py Mem.ihex hex 2147483648 4 400000 1 > Mem.hex
