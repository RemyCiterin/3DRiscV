#include <stdint.h>
#include <verilated.h>
#include <atomic>
#include <thread>
#include <iostream>
#include "VTestCore.h"

#define BAUD 115200
#define FREQ 25000000


char input = 0;
int rx_bit_counter = 0;
bool generate_rx(std::atomic_char &current_input) {
  if (input != 0) {
    if (rx_bit_counter == 0) {
      // Start every UART message by a zero
      rx_bit_counter++;
      //printf("%d", 0);
      return false;
    } else if (rx_bit_counter <= 8) {
      char c = input & (1 << (rx_bit_counter - 1));
      //printf("%d", c != 0);
      rx_bit_counter++;
      return c != 0;
    } else if (rx_bit_counter == 64) {
      input = 0;
      current_input = 0;
      return true;
    }

    rx_bit_counter++;
    return true;
  }

  input = current_input;
  rx_bit_counter = 0;
  return true;
}

char output = 0;
int tx_counter = 0;
int tx_bit_counter = 0;
void receive_tx(bool pos) {
  if (tx_bit_counter != 0 && tx_counter == 0) {
    int value = pos ? 1 : 0;
    output = output | (value << (8 - tx_bit_counter));

    if (tx_bit_counter == 1)
      printf("%c", output);

    tx_bit_counter -= 1;
    tx_counter = (FREQ / BAUD) + 5;
    return;
  }

  if (tx_counter != 0) tx_counter -= 1;

  if (tx_counter == 0 && tx_bit_counter == 0 && !pos) {
    tx_counter = (FREQ / BAUD) + 5;
    tx_bit_counter = 8;
    output = 0;
  }
}

void run_core(int argc, char **argv, std::atomic_char &current_input) {
  Verilated::commandArgs(argc, argv);
  VTestCore *top = new VTestCore;

  bool rx = true;
  int counter = 0;

  while (!Verilated::gotFinish()) {
    if (top->clock) {
      receive_tx(top->out_0);

      counter++;

      if (counter % (FREQ / BAUD) == 0) {
        rx = generate_rx(current_input);
        counter = 0;
      }
    }

    top->in0 = rx;

    top->clock ^= 1;
    top->eval();
  }

  delete top;
  return;
}

int main(int argc, char **argv)
{
  std::setbuf(stdin, nullptr);
  std::setbuf(stdout, nullptr);
  setvbuf(stdout, NULL, _IONBF, 0);
  setvbuf(stdin, NULL, _IONBF, 0);
  std::ios::sync_with_stdio(false);
  std::atomic_char current_input(0);
  std::thread thread1(run_core, argc, argv, std::ref(current_input));

  while (1) {
    char old = current_input;
    char c = getchar();

    do {
      old = current_input;
    } while (old != 0);

    current_input = c;
    //printf("receive char %c\n", c);
  }

  thread1.join();
  exit(0);
}
