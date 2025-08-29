{ pkgs ? import <nixpkgs> {} }:

let
  pkgsCross = import pkgs.path {
    localSystem = pkgs.stdenv.buildPlatform.system;
    crossSystem = {
      config = "riscv32-none-elf";
      libc = "newlib-nano";
      #libc = "newlib";
      gcc.arch = "rv32ima";
    };
  };
in

pkgs.mkShell {
  buildInputs = [
    pkgs.libelf
    pkgs.verilator
    pkgs.verilog
    pkgs.gtkwave
    pkgs.openfpgaloader
    #pkgs.pkgsCross.riscv64-embedded.buildPackages.gcc
    #pkgs.pkgsCross.riscv32-embedded.buildPackages.gcc
    pkgsCross.buildPackages.gcc
    pkgs.qemu

    pkgs.yosys
    pkgs.nextpnr
    pkgs.trellis
    pkgs.screen
    pkgs.sail-riscv

    pkgs.graphviz

    pkgs.fujprog

    pkgs.python312Packages.matplotlib
    pkgs.python312Packages.numpy
  ];
}
