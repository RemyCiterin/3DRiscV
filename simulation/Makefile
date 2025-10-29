SDL_CFLAGS = `sdl2-config --cflags`
SDL_LDFLAGS = `sdl2-config --libs`

all:
	verilator -O2 --cc --main ../Verilog/TestCore.v -o sim --threads 1 \
		--exe Main.cpp -Wno-UNSIGNED -Wno-UNUSEDSIGNAL -Wno-CMPCONST \
		-CFLAGS "${SDL_CFLAGS}" -LDFLAGS "${SDL_LDFLAGS}"
	make -C obj_dir -j -f VTestCore.mk sim
	cp obj_dir/sim ../sim
	rm -rf obj_dir


