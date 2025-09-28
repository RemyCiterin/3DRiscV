#include <stdint.h>

static uint8_t* UART_BASE= (uint8_t*)0x10000000;


extern void main() {
  *UART_BASE = 42;
  //*UART_BASE = 0;
}
