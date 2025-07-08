{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  buildInputs = [
    pkgs.libelf
    pkgs.verilator
    pkgs.verilog
    pkgs.gtkwave
    pkgs.openfpgaloader
    pkgs.pkgsCross.riscv32-embedded.buildPackages.gcc
    pkgs.qemu

    pkgs.yosys
    pkgs.nextpnr
    pkgs.trellis
    pkgs.screen

    pkgs.graphviz

    pkgs.fujprog

    pkgs.python312Packages.matplotlib
    pkgs.python312Packages.numpy
  ];
}
