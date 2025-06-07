#include <stdint.h>
#include <verilated.h>
#include "VTestCore.h"

int main(int argc, char **argv, char **env)
{
  Verilated::commandArgs(argc, argv);
  VTestCore *top = new VTestCore;

  while (!Verilated::gotFinish()) {
    //if (top->clock)
    //        std::cout << " " << top->fn << std::endl;
    top->clock ^= 1;
    top->eval();
  }

  delete top;
  exit(0);
}

