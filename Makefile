FAMILY = artix7
PART = xc7a200tsbg484-1
DBPART = $(shell echo ${PART} | sed -e 's/-[0-9]//g')
XDC ?= nexysvideo.xdc

LIB = \
			src/Top.v \
			simulation/Block*v \
			simulation/pll_nexys_video.v \
			Verilog/*.v

DDR3 = \
			 UberDDR3/rtl/*.v

compile:
	cabal run blarney-test -- --enable-name-prop

formal:
	cabal run blarney-test -- --formal

test:
	riscv32-none-elf-objcopy --strip-debug -O ihex zig/zig-out/bin/kernel.elf Mem.ihex
	riscv32-none-elf-objcopy --strip-debug -O binary zig/zig-out/bin/user.elf zig/src/user.bin
	./ihex-to-img.py Mem.ihex hex 2147483648 4 1000000 1 > Mem.hex
	riscv32-none-elf-objdump zig/zig-out/bin/kernel.elf -S > zig/kernel.asm
	riscv32-none-elf-objdump zig/zig-out/bin/user.elf -S > zig/user.asm

test_rust:
	riscv32-none-elf-objcopy --strip-debug -O ihex \
		rust/target/riscv32ima-unknown-none-elf/release/SuperOS Mem.ihex
	./ihex-to-img.py Mem.ihex hex 2147483648 4 1000000 1 > Mem.hex
	riscv32-none-elf-objdump rust/target/riscv32ima-unknown-none-elf/release/SuperOS -D > rust/kernel.asm
	# riscv32-none-elf-objdump zig/zig-out/bin/user.elf -S > zig/user.asm

qemu:
	qemu-system-riscv32 \
  	-M virt -serial stdio -display \
  	none -m 1024M -bios none \
  	-kernel zig/zig-out/bin/kernel.elf \
  	-cpu rv32,pmp=false

qemu_rust:
	qemu-system-riscv32 \
  	-M virt -serial stdio -display \
  	none -m 1024M -bios none \
  	-kernel rust/target/riscv32ima-unknown-none-elf/release/SuperOS \
  	-cpu rv32,pmp=false

#yosys:
#	sed -i '/$$finish/d' Verilog/TestCore.v
#	yosys \
#		-DULX3S -q -p "synth_ecp5 -abc9 -abc2 -top mkTop -json ./build/mkTop.json" \
#		$(LIB)

#nextpnr:
#	nextpnr-ecp5 --force --timing-allow-fail --json ./build/mkTop.json --lpf ulx3s.lpf \
#		--textcfg ./build/mkTop_out.config --85k --freq 40 --package CABGA381

ecppack:
	ecppack --compress --svf-rowsize 100000 --svf ./build/mkTop.svf \
		./build/mkTop_out.config ./build/mkTop.bit

prog:
	sudo fujprog build/mkTop.bit -t

verilator: compile
	make -C simulation all
	stdbuf -o0 ./sim

simulate:
	iverilog -s top_sim src/SimTop.v simulation/mt48lc16m16a2.v simulation/BlockRAMDual.v Verilog/*.v -o Verilog/SimTop.vvp
	vvp Verilog/SimTop.vvp

run:
	stdbuf -o0 ./sim

run_verbose:
	./sim_verbose

.PHONY: clean
clean:
	@rm -f build/*.bit
	@rm -f build/*.frames
	@rm -f build/*.fasm
	@rm -f build/*.json
	@rm -f build/*.bin
	@rm -f build/*.bba

.PHONY: pnrclean
pnrclean:
	rm build/*.fasm build/*.frames build/*.bit

.PHONY: program
program:
	sudo openFPGALoader --board nexysVideo --bitstream build/mkTop.bit

yosys:
	sed -i '/$$finish/d' Verilog/TestCore.v
	sed -i '/$$write/d' Verilog/TestCore.v
	sed -i '/$$write/d' Verilog/Uart.v
	sed -i '/$$write/d' Verilog/TestSpi.v
	sed -i '/$$write/d' Verilog/TestDDR3.v
	yosys -q -p \
		"synth_xilinx -flatten -abc9 -arch xc7 -top mkTop; write_json build/mkTop.json" \
		$(LIB) $(DDR3)

# The chip database only needs to be generated once
# that is why we don't clean it with make clean
db/${DBPART}.bin:
	${PYPY3} ${NEXTPNR_XILINX_PYTHON_DIR}/bbaexport.py \
		--device ${PART} --bba ${DBPART}.bba
	bbasm -l ${DBPART}.bba db/${DBPART}.bin
	rm -f ${DBPART}.bba

build/mkTop.fasm: build/mkTop.json db/${DBPART}.bin ${XDC}
	nextpnr-xilinx \
		--router router1 --chipdb db/${DBPART}.bin --xdc ${XDC} \
		--json build/mkTop.json --fasm $@

build/mkTop.frames: build/mkTop.fasm
	fasm2frames --part ${PART} --db-root ${PRJXRAY_DB_DIR}/${FAMILY} \
		build/mkTop.fasm > build/mkTop.frames

build/mkTop.bit: build/mkTop.frames
	xc7frames2bit \
		--part_file ${PRJXRAY_DB_DIR}/${FAMILY}/${PART}/part.yaml \
		--part_name ${PART} --frm_file build/mkTop.frames \
		--output_file build/mkTop.bit

nextpnr: build/mkTop.bit
