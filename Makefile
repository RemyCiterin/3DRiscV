
sim:
	cabal run blarney-test -- --simulate

LIB = \
			src/Top.v \
			Verilog/*.v

compile:
	cabal run

test:
	riscv32-none-elf-objcopy -O ihex ../Superscalar/rust/target/riscv32i-unknown-none-elf/release/SuperOS Mem.ihex
	./ihex-to-img.py Mem.ihex hex 2147483648 4 100000 1 > Mem.hex

yosys:
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
