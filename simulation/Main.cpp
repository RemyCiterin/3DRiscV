#include <stdint.h>
#include <verilated.h>
#include <atomic>
#include <thread>
#include <iostream>
#include "VTestCore.h"

#include <SDL2/SDL.h>
#define screenWidth 640
#define screenHeight 480
static SDL_Window *window = NULL;
static SDL_Renderer *renderer = NULL;

#define RENDER false

#define BAUD 115200
#define FREQ 25000000

void vga_init() {
  SDL_SetHint(SDL_HINT_NO_SIGNAL_HANDLERS, "1");
  window = SDL_CreateWindow(
    "ulx3s simulation",
    SDL_WINDOWPOS_CENTERED,
    SDL_WINDOWPOS_CENTERED,
    screenWidth,
    screenHeight,
    0
  );
  if (!window) exit(1);

  renderer = SDL_CreateRenderer(window, -1, SDL_RENDERER_ACCELERATED);
  if (!renderer) exit(1);
}

static int xpos = 0;
static int ypos = 0;
static int blank_num = 0;
static void vga_draw(bool blank, uint8_t r, uint8_t g, uint8_t b) {

  // Update coordinates
  xpos += 1;

  if (blank) {
    xpos = 0;

    // first blank signal
    if (blank_num == 0) ypos += 1;

  } else {
    // New frame
    if (blank_num >= screenWidth) {
      SDL_RenderPresent(renderer);
      ypos = 0;
    }

    SDL_SetRenderDrawColor(renderer, r, g, b, 255);
    SDL_RenderDrawPoint(renderer, xpos, ypos);
  }

  // Update the number of blank pixels
  blank_num = blank ? blank_num+1 : 0;
}


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

  if (RENDER) vga_init();

  bool rx = true;
  int counter = 0;

  while (!Verilated::gotFinish()) {
    if (top->clock) {
      if (RENDER) vga_draw(top->out_2_blank, top->out_2_red, top->out_2_green, top->out_2_blue);
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
