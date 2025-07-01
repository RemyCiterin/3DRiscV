
LIB = \
			src/Top.v \
			simulation/Block*v \
			Verilog/*.v

compile:
	cabal run blarney-test -- --enable-name-prop

test:
	riscv32-none-elf-objcopy -O ihex zig/zig-out/bin/kernel.elf Mem.ihex
	./ihex-to-img.py Mem.ihex hex 2147483648 4 100000 1 > Mem.hex
	riscv32-none-elf-objdump zig/zig-out/bin/kernel.elf -D > zig/firmware.asm

yosys:
	sed -i '/$$finish/d' Verilog/TestCore.v
	yosys \
		-DULX3S -q -p "synth_ecp5 -abc9 -abc2 -top mkTop -json ./build/mkTop.json" \
		$(LIB)

nextpnr:
	nextpnr-ecp5 --force --timing-allow-fail --json ./build/mkTop.json --lpf ulx3s.lpf \
		--textcfg ./build/mkTop_out.config --85k --freq 40 --package CABGA381

ecppack:
	ecppack --compress --svf-rowsize 100000 --svf ./build/mkTop.svf \
		./build/mkTop_out.config ./build/mkTop.bit

prog:
	sudo fujprog build/mkTop.bit -t

verilator: compile
	make -C simulation all
	./sim

iverilog:



simulate:
	iverilog -s top_sim src/SimTop.v simulation/mt48lc16m16a2.v simulation/BlockRAMDual.v Verilog/*.v -o Verilog/SimTop.vvp
	vvp Verilog/SimTop.vvp

run:
	./sim
