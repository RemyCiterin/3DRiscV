use core::fmt::{self, Write};
use core::ptr::write_volatile;
use core::ptr::read_volatile;

use core::arch::asm;

struct Writer;

pub const UART_BASE: usize = 0x1000_0000;
pub const UART_PEEK: *const u8 = (UART_BASE + 2) as *const u8;
pub const UART_PUT: *const u8 = (UART_BASE + 1) as *const u8;
pub const UART_DATA: *mut u8 = UART_BASE as *mut u8;

impl Write for Writer {
    fn write_str(&mut self, s: &str) -> fmt::Result {
        for c in s.bytes() {
            //while unsafe{*UART_LSR & UART_LSR_EMPTY_MASK} == 0 {}
            unsafe{
                while read_volatile(UART_PUT) == 0 {}
                write_volatile(UART_DATA, c);
            }

        }

        Ok(())
    }
}

pub fn print(args: fmt::Arguments) {
    Writer.write_fmt(args).unwrap();
}

#[macro_export]
macro_rules! print {
    ($($arg:tt)*) => ($crate::printer::print(format_args!($($arg)*)));
}

#[macro_export]
macro_rules! println {
    () => ($crate::print!("\n"));
    ($($arg:tt)*) => ($crate::print!("{}\n", format_args!($($arg)*)));
}
