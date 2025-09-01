all:
	verilator --cc ../Verilog/TestCore.v -o sim --threads 1 \
		--exe Main.cpp -Wno-UNSIGNED -Wno-UNUSEDSIGNAL -Wno-CMPCONST
	make -C obj_dir -j -f VTestCore.mk sim
	cp obj_dir/sim ../sim
	rm -rf obj_dir


