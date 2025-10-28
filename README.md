# 3DRiscV

`3DRiscV` is a system on chip containing a Unix capable rv32-ima cpu and a RISC-V based GPGPU,
both connected using the TileLink cache coherency protocol. This SOC can run on an ULX3S board,
and can be simulated using Verilator.


The GPGPU is programmed using a custom compiler, this allow to automatically insert the
custom instructions to handle thread reconvergence.

# Unix demo

## Clone the repository

```bash
git clone --recursive https://github.com/RemyCiterin/3DRiscV.git
cd blarney-experiments
```

Then install a rv32ima cross-compiler supporting newlib using `nix-shell`.

## Compile and load unix in an SD card

Connect an SD card to your computer and run:

```bash
cd xv6-rv32
make binary
sudo dd if=kernel.bin of=SD_CARD_DESTINATION
sync
cd ..
```

The generated operating system is a fork of Unix using a ramdisk for the moment, I plan to use the
SD card for this in the future as well.

## Compile the bootloader

A bootloader is included with the core at the address `0x8000_0000`, by default it copy the
content of the first 5M of the SD card to the begining of the RAM at the address `0x8001_0000`,
then jump to this address. Given the previous steps, it must jump to the starting address of the
operating system.

```bash
cd bootloader
cargo build --release
cd ..
```

The bootloader is in Rust so you may need to install cargo first.

## Synthetise the core

Finaly, connect the SD card to your ulx3s board, and connect the board to your computer, then run:

```bash
make test_bootloader compile_no_gpu ulx3s
```

You must see this screen:

```
start to initialize SD card driver
receive at command 0 1
receive at command 8: 1001aa
receive at command 55: 1
receive at command 41: 1
receive at command 55: 1
receive at command 41: 1
receive at command 55: 1
receive at command 41: 0
receive at command 58: 0c0ff800ffffffff
receive at command 16: 0
read block 9900
 _____   ____         ____  __  __     ____   _____
|  __ \ / __ \       / __ \|  \/  |   / __ \ / ____|
| |  | | |  | | ___ | |  | | \  / |  | |  | | (___
| |  | | |  | |/ _ \| |  | | |\/| |  | |  | |\___ \
| |__| | |__| | (_) | |__| | |  | |  | |__| |____) |
|_____/ \____/ \___/ \____/|_|  |_|   \____/|_____/

A port of MIT's xv6 OS to my rv32ima softcore. It is a fork of
git@github.com:michaelengel/xv6-rv32.git with some minor midifications.
xv6 kernel is booting:
- physical page allocator
- create kernel page table
- turn on paging
- process table
- trap vectors
- install kernel trap vector
- set up interrupt controller
- ask PLIC for device interrupts
- buffer cache
- inode cache
- fill table
- first user process

start scheduler!
init: starting sh
$
```

# GPGPU demo

TODO
